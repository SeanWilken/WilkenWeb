namespace Shared

open System

// GRID GAMES THAT USE LIST OF LANE OBJECTS TO DEFINE THEIR GRID BOARD AND CONTENTS
module GridGame =
    
    // Grid Movement Directions
    type MovementDirection =
        | Up
        | Down
        | Left
        | Right

    // Object that can be in any given space
    // THIS SHOULD BE MADE GENERIC WHERE THE SUB IMPLEMENTS
    type LaneObject =
        | Ball
        | Blank
        | Blocker
        | Bomb
        | Goal
        | Heart
        | LaneLock
        | LaneKey
        | MoveArrow of MovementDirection
        | ValueTile of int option

    // List of LaneObjects that occupy a Grid Position
    type GridBoard = {
        GridPositions: LaneObject list
    }

    // Represents a given game state
    type RoundState =
        | Playing
        | Won

    // unwrap optional int from tryFindIndex
    // this is pretty unnecessary and should be 'baked' into the model domain
    let unwrapIndex index =
        match index with
        | Some i -> i
        | _ -> -1

    // returns true or false based on if the lookupObject is at the gridPositionIndex
    let checkGridPositionForObject positions (gridPositionIndex: int) objToFind =
        positions.GridPositions.Item(gridPositionIndex) = objToFind
    // returns the object at a given positions index
    let getObjectAtPositionIndex (positions: LaneObject list) (positionIndex: int) = 
        positions.Item(positionIndex)
    // would return the first index if one is found
    let getObjectPositionIndex (positions: GridBoard) (positionObject: LaneObject) =
        List.tryFindIndex (fun x -> x = positionObject ) positions.GridPositions
    // updates the grid position with an object
    let updatePositionWithObject positions (object: LaneObject) position = 
        let gridLength = positions.GridPositions.Length - 1
        let newGrid =
            // [ for i in 0 .. gridLength do
            [ for i in 0 .. gridLength do
                if i = position
                    then object 
                    else positions.GridPositions.Item(i)
            ]
        {GridPositions = newGrid}

    // GRID POSITION REPRESENTATION
    // GridPositions Represented in column groups
    let getPositionsAsColumns (positions: GridBoard) gridDimension =
        Seq.toList (seq { 
            for i in 0 .. (gridDimension - 1) do
                yield Seq.toList (seq { 
                    for n in 0 .. (gridDimension - 1) do
                        yield! seq{ 
                            positions.GridPositions.Item(i + (n * gridDimension)) 
                        }
                })
        })

    // GridPositions Represented in row groups
    let getPositionsAsRows (positions: GridBoard) gridDimension =
        Seq.toList (seq { 
            for i in 0 .. (gridDimension - 1) do
                yield Seq.toList (seq {
                    for n in 0 .. (gridDimension - 1) do
                        yield! seq{ 
                            positions.GridPositions.Item(n + (i * gridDimension))
                        }
                })
        })

module SharedGoalRoll =

    open GridGame
    
    type Model =
        {
            LevelIndex: int
            BallPositionIndex: int
            GoalPositionIndex: int
            InitialGrid: GridGame.GridBoard
            CurrentGrid: GridGame.GridBoard
            GameState: GridGame.RoundState
            // MovementsMade: MovementDirection list
        }

    let getBallPositionIndex (gameGridPositions: GridBoard) =
        getObjectPositionIndex gameGridPositions Ball
        |> unwrapIndex

    let getGoalPositionIndex (gameGridPositions: GridBoard) =
        getObjectPositionIndex gameGridPositions Goal
        |> unwrapIndex

    let getBallRollPositionIndex ballPosition direction =
        match direction with
        | Up -> (ballPosition - 8)
        | Down -> (ballPosition + 8)
        | Left -> (ballPosition - 1)
        | Right -> (ballPosition + 1)
    let getNormalizedArrowPosition normalizedBallPosition direction =
        match direction with
        | Up -> (normalizedBallPosition - 8)
        | Down -> (normalizedBallPosition + 8)
        | _ -> (normalizedBallPosition % 8)
    let checkNormalizedArrowPosition normalizedArrowPosition direction =
        match direction with
        | Up -> normalizedArrowPosition > 0
        | Down -> normalizedArrowPosition <= 64
        | Left -> normalizedArrowPosition <> 1
        | Right -> normalizedArrowPosition <> 0
    let gridWithoutMoveArrows positions =
        let thing = List.map (fun x -> match x with | MoveArrow _ -> Blank | _ -> x ) positions.GridPositions
        { GridPositions = thing }
    let gridWithGoal positions goalPosition =
        updatePositionWithObject positions Goal goalPosition
    let gridWithMovementArrow positions direction =
        let ballPositionIndex = getBallPositionIndex positions
        let normalizedBallPositionIndex = ballPositionIndex + 1
        let normalizedArrowPositionIndex = getNormalizedArrowPosition normalizedBallPositionIndex direction
        let validArrowPosition = checkNormalizedArrowPosition normalizedArrowPositionIndex direction
        if validArrowPosition
            then 
                let thing = getBallRollPositionIndex ballPositionIndex direction
                if (checkGridPositionForObject positions thing Blank)
                    then updatePositionWithObject positions (MoveArrow direction) thing
                    else positions
            else positions
    // refactor please
    let rec rollBallInGridDirection positions direction =
        let arrowlessGrid = gridWithoutMoveArrows positions
        let ballPositionIndex = getBallPositionIndex arrowlessGrid
        if ballPositionIndex = -1
            then
                positions
            else
                match direction with
                | Up ->
                    let ballRollUpPositionIndex = getBallRollPositionIndex ballPositionIndex Up
                    if (ballRollUpPositionIndex >= 0)
                        then
                            if (checkGridPositionForObject arrowlessGrid ballRollUpPositionIndex Blank) || (checkGridPositionForObject arrowlessGrid ballRollUpPositionIndex Goal)
                                then 
                                    let ballToBlankGrid = updatePositionWithObject (arrowlessGrid) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollUpPositionIndex
                                    rollBallInGridDirection ballRolledGrid direction
                                else positions
                        else positions
                | Down ->
                    let ballRollDownPositionIndex = getBallRollPositionIndex ballPositionIndex Down
                    if (ballRollDownPositionIndex <= 63)
                        then 
                            if (checkGridPositionForObject arrowlessGrid ballRollDownPositionIndex Blank) || (checkGridPositionForObject arrowlessGrid ballRollDownPositionIndex Goal)
                                then
                                    let ballToBlankGrid = updatePositionWithObject (arrowlessGrid) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollDownPositionIndex
                                    rollBallInGridDirection ballRolledGrid direction
                                else positions
                        else positions
                | Right ->
                    let ballRollRightPositionIndex = getBallRollPositionIndex ballPositionIndex Right
                    if (((ballRollRightPositionIndex) % 8) <> 0)
                        then 
                            if (checkGridPositionForObject arrowlessGrid ballRollRightPositionIndex Blank) || (checkGridPositionForObject arrowlessGrid ballRollRightPositionIndex Goal)
                                then
                                    let ballToBlankGrid = updatePositionWithObject (arrowlessGrid) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollRightPositionIndex
                                    rollBallInGridDirection ballRolledGrid direction
                                else positions
                        else positions
                | Left ->
                    let ballRollLeftPositionIndex = getBallRollPositionIndex ballPositionIndex Left
                    if (((ballRollLeftPositionIndex + 1) % 8) >= 1)
                        then 
                            if (checkGridPositionForObject arrowlessGrid ballRollLeftPositionIndex Blank) || (checkGridPositionForObject arrowlessGrid ballRollLeftPositionIndex Goal)
                                then 
                                    let ballToBlankGrid = updatePositionWithObject (arrowlessGrid) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollLeftPositionIndex
                                    rollBallInGridDirection ballRolledGrid direction
                                else positions
                        else positions

    // --------------------------------------
    // SHARABLE (IF REFACTOR AND LEVELS MADE MORE GENERIC FOR LOAD)

    // ------------------------
    // LEVEL CREATION DOMAIN, PLEASE RELOCATE ME, UNSURE OF POSITION
    // SEED GENERATION???
    let Level0 = { 
        GridPositions = [
            Blank; Blocker; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Heart; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; LaneLock; Blank;
            Bomb; Blank; Blank; Ball; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blocker; Blank;
            Blank; Blank; Blank; Goal; Blank; Blank; Blank; Blank;
            Blank; Blank; LaneKey; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Heart;
        ] }
    let Level1 = { 
        GridPositions = [
            Ball; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank;Blank; Blank;Blank; Blank;Blank; Blocker;
            Blank; Blank; Blocker; Goal; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank; 
        ] }
    let Level2 = { 
        GridPositions = [
            Blank; Blank; Blank; Blank; Blank; Blank; Blocker; Blank;
            Blocker; Goal; Blank; Blank; Blank; Blank;Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blocker;
            Ball; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blank;
            Blocker; Blank; Blank; Blank; Blank; Blocker; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blocker; Blocker; Blank;
        ] }
    let Level3 = { 
        GridPositions = [
            Blocker; Blocker; Blocker; Blank; Blank; Blocker; Blank; Blank;
            Blocker; Blocker; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blocker; Blank; Blank;
            Blank; Blocker; Blank; Blank; Blocker; Blank; Blank; Blank;
            Blank; Blank; Blocker; Blocker; Blocker; Blank; Blank; Blank;
            Blank; Blank; Blocker; Blocker; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blocker; Goal; Blank;
            Blocker; Blank; Blank; Blank; Blank; Ball; Blocker; Blocker; 
        ] }

    // --------------------------------------------------------------

    // LEVEL AND MODEL
    let loadRound roundIndex =
        match roundIndex with
        | 1 ->
            Level1
        | 2 ->
            Level2
        | 3 ->
            Level3
        | _ ->
            Level0

    // initial model, no message or command
    let initModel =
        let round = loadRound 1;
        let initialModel = {
            LevelIndex = 1;
            InitialGrid = round;
            CurrentGrid = round;
            BallPositionIndex = getBallPositionIndex round;
            GoalPositionIndex = getGoalPositionIndex round;
            GameState = Playing;
        }
        initialModel

// module SharedPivotPoints = 
// TODO

module SharedTileSmash =

    open GridGame

    type Model = {
        TileSmashGridBoard: GridBoard
        HitPoints: int
        TilesSmashed: int
        }

    let gridDimension = 8
    let generateEmptyTileSmashGrid gridDimension =
        { GridPositions = [
                for i in 0 .. ((gridDimension * gridDimension) - 1) do
                    Blank
            ]
        }

    let initModel = {
        TileSmashGridBoard = generateEmptyTileSmashGrid gridDimension
        HitPoints = 3
        TilesSmashed = 0
    }

module SharedTileSort =

    open GridGame

    // Determines the grid dimensions
    type TileSortDifficulty =
        | Simple // 3x3
        | Easy // 4x4
        | Medium // 5x5
        | Hard // 6x6
    // Determines the current activity state of the TileGame
    // NOT CURRENTLY IMPLEMENTED
    // Tile Game
    // Represents a single tile on the board
    type GameTile = {
        Value: int option
    }
    // board tile list
    type TileSortBoard = {
        GameTiles: GameTile list // all tiles
    }
    //
    type Model = {
        Difficulty: TileSortDifficulty // default at initial
        InitialTiles: TileSortBoard // set at start of round
        CurrentTiles: TileSortBoard // changing list of game tiles to represent moves
        Turns: int list // list of moves made for rewind / total number of moves to solve.
        GameState: GridGame.RoundState // determines whether the puzzle has been solved or in progress
    }

    // REFACTOR TO GAMES HELPER? A LOT OF THIS IS REUSABLE / REQUIRED BY GOALROLL
    // HELPER FUNCTIONS FOR RANDOMIZATION
    // Random Instance
    let rand = Random()
    // return random number within ceiling
    let randomIndex maxNum = rand.Next(maxNum)
    // selects one tile randomly from a given list
    let selectRandomTile (assignedTiles: GameTile list) = assignedTiles.[randomIndex(assignedTiles.Length)]
    //---------------------

    // INITIAL GAMEBOARD FUNCTIONS
    // Step 1: Create a list of values, to be used as the tile values.
    // Returns list of int to be used as the Tile values
    let getGridDimension difficulty =
        match difficulty with
        | Simple -> 3
        | Easy -> 4
        | Medium -> 5
        | Hard -> 6
    let createListOfIntValues gridDimension =
        [1..(gridDimension * gridDimension)]
    let generateGameBoardPositionsBasedOffDifficulty difficulty =
        createListOfIntValues (getGridDimension difficulty)
    // Step 2: Iterate recursively through the generated list of tile values,
        // selects a value randomly from the list of values to create a GameTile, -- THIS SHOULD USE AN EXPOSED SAFE CONSTRUCTOR!!!
        // adds created GameTile to new list that will hold the initial order of the tiles.
    // small helper for empty list for new games
    let unassignedTiles = []
    // recursive function to iterate through list of generated game tile values
    let rec createRandomGameTiles (assignedTiles: GameTile list) (remainingIndexes: int list) =
        // take random element from the array list
        let randomTileValue = remainingIndexes.[randomIndex remainingIndexes.Length]
        // filter out the selected element
        let remain = List.filter(fun elem -> elem <> randomTileValue) remainingIndexes
        // create game tile from index and the selected element // THIS SHOULD USE A CONSTRUCTOR FUNCTION
        let gameTile = {Value = Some randomTileValue;}
        // recurse back through if there are remaining positions
        match remain with
        | [] -> gameTile :: assignedTiles
        | y::ys -> createRandomGameTiles (gameTile :: assignedTiles) remain
    // Step 3: Randomly blank a tile value to None, to act as the void space for sliding a tile.
    // remove one entry randomly to get the blank starting position
    let randomBlankPosition (assignedTiles: GameTile list) =
        let selectedTile = selectRandomTile assignedTiles
        List.map (fun x -> if x = selectedTile then {Value = None;} else x) assignedTiles
    // STEP 4: ASSIGN GENERATED TILE ROUND TO INITIAL AND CURRENT
    let createNewRoundGameBoardBasedOnDifficulty difficulty =
        let newRoundTilePositions = generateGameBoardPositionsBasedOffDifficulty difficulty
        let randomizedNewRoundTilePositions = createRandomGameTiles unassignedTiles newRoundTilePositions
        {GameTiles = randomBlankPosition randomizedNewRoundTilePositions}

    //---------------------

    // GAME BOARD LOGIC
    // get the tile index from the containing list. // RETURNS AN INDEX???
    // returns index of Value positon within list of tiles, if it exists
    let getTileIndexWithValue (tiles: GameTile list) (tileValue: int option) = 
        List.tryFindIndex (fun x -> x.Value = tileValue ) tiles // is this essentially a list.filter?
    // returns index of blank tile positon within list of tiles, if it exists
    let getBlankTileIndex tiles = getTileIndexWithValue tiles None
    // Produces a list of Tile lists, grouped by column.
    // REFACTOR INTO ONE CALL THAT TAKES THE FUNCTION FOR GETTING ITEM
    let getTilesAsColumns tiles difficulty =
        let gridDimension = getGridDimension difficulty
        Seq.toList (seq { 
            for i in 0 .. (gridDimension - 1) do
                yield Seq.toList (seq { 
                    for n in 0 .. (gridDimension - 1) do
                        yield! seq{ 
                            tiles.GameTiles.Item(i + (n * gridDimension)) 
                        }
                })
        })
    // Produces a list of Tile lists, grouped by row.
    let getTilesAsRows tiles difficulty =
        let gridDimension = getGridDimension difficulty
        Seq.toList (seq { 
            for i in 0 .. (gridDimension - 1) do
                yield Seq.toList (seq {
                    for n in 0 .. (gridDimension - 1) do
                        yield! seq{ 
                            tiles.GameTiles.Item(n + (i * gridDimension))
                        }
                })
        })
    //---------------------

    let getValueOrZeroFromGameTile gameTile =
        match gameTile.Value with
        | Some i -> i
        | None -> 0

    let convertValueToProperString tileValue =
        match tileValue with
        | 0 -> ""
        | _ -> string tileValue

    let checkTileGroupForSelected tiles selectedTile =
        tiles
        |> List.map (fun tileGroup ->
            let selectedTileIndex = getTileIndexWithValue tileGroup selectedTile
            let blankTileIndex = getBlankTileIndex tileGroup
            let selTileIndex = 
                match selectedTileIndex with
                | Some i -> i
                | None -> -1
            let blanTileIndex = 
                match blankTileIndex with 
                | Some i -> i 
                | None -> -1
            if (selTileIndex <> -1 && blanTileIndex <> -1) then
                (selTileIndex + 1 = blanTileIndex) || (selTileIndex - 1 = blanTileIndex)
            else
                false
        )
        |> List.contains true
    // TILE SWAP FUNCTION AND VALIDATION
    let checkTileMoveInRows tiles selectedTile difficulty =
        //split tiles into groups
        let tilesAsRows = getTilesAsRows tiles difficulty
        checkTileGroupForSelected tilesAsRows selectedTile
    // REFACTOR REFACTOR REFACTOR
    let checkTileMoveInColumns tiles selectedTile difficulty =
        //split tiles into groups
        let tilesAsColumns = getTilesAsColumns tiles difficulty
        checkTileGroupForSelected tilesAsColumns selectedTile
    //RETURN GAMEBOARD {GAMETILES = GameTile list}
    let swapSelectedTileWithBlank currentTiles selectedTileValue =
        match selectedTileValue with
        | Some i ->
            { GameTiles = List.map (fun x -> if x.Value = None then { Value = selectedTileValue } else if x.Value = Some i then { Value = None } else x ) currentTiles }
        | None -> { GameTiles = currentTiles }
    // REVIEW - REQUIRES TEST AND THINK THROUGH
    let addSelectedToTurnHistory selectedTile turnHistory =
        selectedTile :: turnHistory
    let canSwapSelectedTileWithBlank tiles selectedTile difficulty =
        if (checkTileMoveInColumns tiles selectedTile difficulty || checkTileMoveInRows tiles selectedTile difficulty ) then 
            swapSelectedTileWithBlank tiles.GameTiles selectedTile
        else tiles
    //---------------------
    // BLANK TILE VALUE POSITION VALIDATION
    // gets the current index of the Blank Tile and adds one to the index
    let caclulatedValueOfBlankTile currentTiles =
        match (getBlankTileIndex currentTiles) with
        | Some i -> Some (i + 1)
        | _ -> None
    // checks that the calculated value of the blank tile doesn't exist in the tiles
    // list should be empty, as the missing value should not exist
    let calculatedBlankValueNotFoundInTiles currentTiles =
        let missingValue = caclulatedValueOfBlankTile currentTiles
        let test: GameTile list = List.filter(fun x -> x.Value = missingValue) currentTiles
        List.isEmpty test
    // simplify calls of calculating and checking Blanks calculated value
    let checkBlankTileIsAtCorrectPosition currentTiles =
        calculatedBlankValueNotFoundInTiles currentTiles
    //---------------------
    // VALUE POSITION VALIDATION
    // gameboard list is the same as the list if sorted by value
    // need to filter out None
    let checkTilesInCorrectOrder (currentTiles: GameTile list) =
        (List.filter (fun x -> x.Value <> None) currentTiles) = (List.filter (fun x -> x.Value <> None) currentTiles |> List.sortBy (fun x -> x.Value))
    //---------------------
    // WIN CONDITIONS VALIDATION
    let winValidator currentTiles =
        checkTilesInCorrectOrder currentTiles.GameTiles && checkBlankTileIsAtCorrectPosition currentTiles.GameTiles
    //---------------------
    // CHANGE DIFFICULTY - UPDATE DIFFICULTY WITH SELECTION
    let changeDifficulty difficulty model =
        {model with Difficulty = difficulty }
    // MOVE TILE - UPDATE TURNS WITH SELECTED
    let addTileValueToTurnHistory selectedTile model =
        {model with Turns = (selectedTile :: model.Turns) }
    // MOVE TILE - UPDATE CURRENT WITH SELECTED & BLANK SWAPPED
    let updateCurrentTilesWithMove gameboard model =
        { model with CurrentTiles = gameboard}
    // REWIND - POP HEAD FROM TURNS, SWAP WITH BLANK (REMOVES HEAD FROM TURNS)
    let rewindCurrentTiles model =
        if not (List.isEmpty model.Turns) then
            let rewindTile = model.Turns.Head
            { model with 
                CurrentTiles = canSwapSelectedTileWithBlank model.CurrentTiles (Some rewindTile) model.Difficulty
                Turns = model.Turns.Tail
            }
        else model
    // NEW GAME - INIT FUNCTION BUT WITH PERSISTED SELECTED DIFFICULTY
    let createNewRound model =
        let newRound = createNewRoundGameBoardBasedOnDifficulty model.Difficulty
        { model with
            InitialTiles = newRound
            CurrentTiles = newRound
            Turns = []
            GameState = Playing
        }
    // RESET ROUND - CURRENT TILES = INITIAL TILES && TURNS = []
    let resetRound model =
        { model with 
            CurrentTiles = model.InitialTiles
            Turns = []
        }
    // Helper function for getting initial model
    let getInitialBoard =
        let initialDifficulty = Simple
        let initialRound = createNewRoundGameBoardBasedOnDifficulty initialDifficulty
        {
            Difficulty = initialDifficulty
            CurrentTiles = initialRound
            InitialTiles = initialRound
            Turns = []
            GameState = Playing
        }

        // REFACTOR TO USE GENERIC GRID GAME, BROKE LOGIC BY MOVING FROM LIST OF GAMETILES AS WAS WRITTEN AROUND THAT

//     type TileSortDifficulty =
//         | Simple // 3x3
//         | Easy // 4x4
//         | Medium // 5x5
//         | Hard // 6x6

//     //
//     type Model = {
//         Difficulty: TileSortDifficulty // default at initial
//         InitialTiles: GridBoard // set at start of round
//         CurrentTiles: GridBoard // changing list of game tiles to represent moves
//         Turns: int list // list of moves made for rewind / total number of moves to solve.
//         GameState: GridGame.RoundState // determines whether the puzzle has been solved or in progress
//     }

//     // REFACTOR TO GAMES HELPER? A LOT OF THIS IS REUSABLE / REQUIRED BY GOALROLL
//     // HELPER FUNCTIONS FOR RANDOMIZATION
//     // Random Instance
//     let rand = Random()
//     // return random number within ceiling
//     let randomIndex maxNum = rand.Next(maxNum)
//     // selects one tile randomly from a given list
//     let selectRandomTilePosition (assignedTiles: GridBoard ) = assignedTiles.GridPositions.[randomIndex(assignedTiles.GridPositions.Length)]
//     //---------------------

//     // INITIAL GAMEBOARD FUNCTIONS
//     // Step 1: Create a list of values, to be used as the tile values.
//     // Returns list of int to be used as the Tile values
//     let getGridDimension difficulty =
//         match difficulty with
//         | Simple -> 3
//         | Easy -> 4
//         | Medium -> 5
//         | Hard -> 6
//     let createListOfIntValues gridDimension =
//         [1..(gridDimension * gridDimension)]
//     let generateGameBoardPositionsBasedOffDifficulty difficulty =
//         createListOfIntValues (getGridDimension difficulty)
//     // Step 2: Iterate recursively through the generated list of tile values,
//         // selects a value randomly from the list of values to create a GameTile, -- THIS SHOULD USE AN EXPOSED SAFE CONSTRUCTOR!!!
//         // adds created GameTile to new list that will hold the initial order of the tiles.
//     // small helper for empty list for new games
//     let unassignedTiles = { GridPositions = [] }
//     // recursive function to iterate through list of generated game tile values
//     let rec createRandomGameTiles (assignedTiles: GridBoard) (remainingIndexes: int list) =
//         // take random element from the array list
//         let randomTileValue = remainingIndexes.[randomIndex remainingIndexes.Length]
//         // filter out the selected element
//         let remain = List.filter(fun elem -> elem <> randomTileValue) remainingIndexes
//         // create game tile from index and the selected element // THIS SHOULD USE A CONSTRUCTOR FUNCTION
//         // let gameTile = { ValueTile = Some randomTileValue;}
//         let gameTile = ValueTile (Some randomTileValue)
//         // recurse back through if there are remaining positions
//         match remain with
//         | [] -> { GridPositions = gameTile :: assignedTiles.GridPositions }
//         | y::ys -> createRandomGameTiles { GridPositions = gameTile :: assignedTiles.GridPositions } remain
//     // Step 3: Randomly blank a tile value to None, to act as the void space for sliding a tile.
//     // remove one entry randomly to get the blank starting position
//     let randomBlankPosition (assignedTiles: GridBoard) =
//         let selectedTile = selectRandomTilePosition assignedTiles
//         List.map (fun x -> if x = selectedTile then ValueTile None else x) assignedTiles.GridPositions
//     // STEP 4: ASSIGN GENERATED TILE ROUND TO INITIAL AND CURRENT
//     let createNewRoundGameBoardBasedOnDifficulty difficulty =
//         let newRoundTilePositions = generateGameBoardPositionsBasedOffDifficulty difficulty
//         let randomizedNewRoundTilePositions = createRandomGameTiles unassignedTiles newRoundTilePositions
//         {GridPositions = randomBlankPosition randomizedNewRoundTilePositions}

//     //---------------------

//     // GAME BOARD LOGIC
// // I THINK THIS NEEDS TO BE DONE IN ROWS / COLUMNS BASED ON GRID SIZE!!!!!
//     // get the tile index from the containing list. // RETURNS AN INDEX???
//     // returns index of Value positon within list of tiles, if it exists
//     let getTilePositionIndexWithValue tiles valueTile =
//         GridGame.getObjectPositionIndex tiles valueTile

//     let getBlankTileIndex tiles = getTilePositionIndexWithValue tiles (ValueTile None)

//     //---------------------

//     let getValueOrZeroFromGameTile (gameTile) =
//         match gameTile with
//         | ValueTile i ->
//             match i with
//             | Some i -> i
//             | None -> 0
//         | _ -> 0

//     let convertValueToProperString tileValue =
//         match tileValue with
//         | 0 -> ""
//         | _ -> string tileValue

//     let checkTileGroupForSelected tiles selectedTile =
//         // getPositionsAsRows tiles 
//         let selectedTileIndex = getTilePositionIndexWithValue tiles selectedTile
//         let blankTileIndex = getBlankTileIndex tiles
//         let selTileIndex = 
//             match selectedTileIndex with
//             | Some i -> i
//             | None -> -1
//         let blanTileIndex = 
//             match blankTileIndex with 
//             | Some i -> i 
//             | None -> -1
//         if (selTileIndex <> -1 && blanTileIndex <> -1) then
//             (selTileIndex + 1 = blanTileIndex) || (selTileIndex - 1 = blanTileIndex)
//         else
//             false

//     // TILE SWAP FUNCTION AND VALIDATION
//     let checkTileMoveInRows tiles selectedTile difficulty =
//         checkTileGroupForSelected tiles selectedTile

//     // REFACTOR REFACTOR REFACTOR
//     let checkTileMoveInColumns tiles selectedTile difficulty =
//         checkTileGroupForSelected tiles selectedTile

//     //RETURN GAMEBOARD {GAMETILES = GameTile list}
//     let swapSelectedTileWithBlank (currentTiles : GridBoard) (selectedTileValue : LaneObject ) =
//         { GridPositions = List.map (fun x -> if x = ValueTile None then selectedTileValue else if x = selectedTileValue then ValueTile None else x ) currentTiles.GridPositions }


//     //// PROBABLY NEEDS TO BE ROWS / COLUMNS!
//     /// // NEEDS HELP FROM THIS WAIST DOWN>>>>>>

//     // REVIEW - REQUIRES TEST AND THINK THROUGH
//     let addSelectedToTurnHistory selectedTile turnHistory =
//         selectedTile :: turnHistory
//     let canSwapSelectedTileWithBlank tiles selectedTile difficulty =
//         if (checkTileMoveInColumns tiles selectedTile difficulty || checkTileMoveInRows tiles selectedTile difficulty ) then 
//             swapSelectedTileWithBlank tiles selectedTile
//         else tiles
//     //---------------------
//     // BLANK TILE VALUE POSITION VALIDATION
//     // gets the current index of the Blank Tile and adds one to the index
//     let caclulatedValueOfBlankTile currentTiles =
//         match (getBlankTileIndex currentTiles) with
//         | Some i -> Some (i + 1)
//         | _ -> None
//     // checks that the calculated value of the blank tile doesn't exist in the tiles
//     // list should be empty, as the missing value should not exist
//     let calculatedBlankValueNotFoundInTiles currentTiles =
//         let missingValue = caclulatedValueOfBlankTile currentTiles
//         let test = List.filter (fun x -> x = ValueTile missingValue) currentTiles.GridPositions
//         List.isEmpty test
//     // simplify calls of calculating and checking Blanks calculated value
//     let checkBlankTileIsAtCorrectPosition currentTiles =
//         calculatedBlankValueNotFoundInTiles currentTiles
//     //---------------------
//     // VALUE POSITION VALIDATION
//     // gameboard list is the same as the list if sorted by value
//     // need to filter out None
//     let checkTilesInCorrectOrder (currentTiles: GridBoard) =
//         (List.filter (fun x -> x <> ValueTile None) currentTiles.GridPositions) = (List.filter (fun x -> x <> ValueTile None) currentTiles.GridPositions |> List.sortBy (fun x -> Some x))//huh?
//     //---------------------
//     // WIN CONDITIONS VALIDATION
//     let winValidator currentTiles =
//         checkTilesInCorrectOrder currentTiles && checkBlankTileIsAtCorrectPosition currentTiles
//     //---------------------
//     // CHANGE DIFFICULTY - UPDATE DIFFICULTY WITH SELECTION
//     let changeDifficulty difficulty model =
//         {model with Difficulty = difficulty }
//     // MOVE TILE - UPDATE TURNS WITH SELECTED
//     let addTileValueToTurnHistory selectedTile model =
//         {model with Turns = (selectedTile :: model.Turns) }
//     // MOVE TILE - UPDATE CURRENT WITH SELECTED & BLANK SWAPPED
//     let updateCurrentTilesWithMove gameboard model =
//         { model with CurrentTiles = gameboard}
//     // REWIND - POP HEAD FROM TURNS, SWAP WITH BLANK (REMOVES HEAD FROM TURNS)
//     let rewindCurrentTiles model =
//         if not (List.isEmpty model.Turns) then
//             let rewindTile = model.Turns.Head
//             { model with 
//                 CurrentTiles = canSwapSelectedTileWithBlank model.CurrentTiles (ValueTile (Some rewindTile)) model.Difficulty
//                 Turns = model.Turns.Tail
//             }
//         else model
//     // NEW GAME - INIT FUNCTION BUT WITH PERSISTED SELECTED DIFFICULTY
//     let createNewRound model =
//         let newRound = createNewRoundGameBoardBasedOnDifficulty model.Difficulty
//         { model with
//             InitialTiles = newRound
//             CurrentTiles = newRound
//             Turns = []
//             GameState = Playing
//         }
//     // RESET ROUND - CURRENT TILES = INITIAL TILES && TURNS = []
//     let resetRound model =
//         { model with 
//             CurrentTiles = model.InitialTiles
//             Turns = []
//         }
//     // Helper function for getting initial model
//     let getInitialBoard =
//         let initialDifficulty = Simple
//         let initialRound = createNewRoundGameBoardBasedOnDifficulty initialDifficulty
//         {
//             Difficulty = initialDifficulty
//             CurrentTiles = initialRound
//             InitialTiles = initialRound
//             Turns = []
//             GameState = Playing
//         }



//<<<<<<<<<<<<<

module SharedCodeGallery =
    
    open SharedGoalRoll
    open SharedTileSmash
    open SharedTileSort

    type Model =
        | CodeGallery
        | GoalRoll of SharedGoalRoll.Model
        | TileSmash of SharedTileSmash.Model
        | TileSort of SharedTileSort.Model
        //COMMENTED CODE - VIEW CODE BUTTON TO DROP DOWN LIKE GITHUB / FIDDLES
        // | WebAppExample // THIS SITE 
        // | JavaScriptExamples
        // | TypeScriptExamples
        // | PhpExamples
        // | C#Examples
        // | SQLExamples
        // | DeploymentExamples
        // | PowerShellExamples //{AUTOMATION: CREATE BING POINT FARMER? NIRCMD SCRIPT}
        // CODE {Productivity / Social (IG+reddit)} // SAFE CHAT

    let getInitialModel = CodeGallery


module SharedDesignGallery =

    type Model = {
        CurrentPieceIndex: int
    }

    let getInitialModel = { CurrentPieceIndex = 0; }

module SharedPortfolioGallery =
    
    open SharedCodeGallery
    open SharedDesignGallery

    type Model =
        | PortfolioGallery
        | CodeGallery of SharedCodeGallery.Model
        | DesignGallery of SharedDesignGallery.Model

    let getInitialModel = PortfolioGallery

module SharedAboutSection =
    type Model =
        {
            ActiveModalIndex : int
            ModalIsActive : bool
        }
       
    let getInitialModel = {ActiveModalIndex = 0; ModalIsActive = false}

module SharedWebAppModels =
    type Model =
        | Welcome
        | AboutSection of SharedAboutSection.Model
        | Portfolio of SharedPortfolioGallery.Model
        | Contact

// NEEDS MAJOR REVIEW FOR HYDRATION OF SERVER REQUEST
// url updating and page routing
// module PageRouter =

//     // PAGE SHOULD REALLY NOT EXIST UNLESS A WRAPPER IS REQUIRED
//     type Page =
//         | Welcome
//         | About
//         | Portfolio
//         | Contact

//     let fromModelToPath =
//         function
//         | SharedWebAppModels.Welcome ->
//             "/welcome"
//         | SharedWebAppModels.AboutSection _ ->
//             "/about"
//         | SharedWebAppModels.Portfolio (SharedPortfolioGallery.PortfolioGallery) ->
//             "/portfolio"
//         | SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery _) ->
//             "/portfolio/code"
//         | SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery _) ->
//             "/portfolio/designs"
//         // | SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery _) ->
//         //     "/portfolio/code"
//         // | About General ->
//         //     "/about"
//         // | About Personal ->
//         //     "/about/personal"
//         // | About Professional ->
//         //     "/about/professional"
//         // | About Resume ->
//         //     "/about/resume"
//         // | Portfolio (Code (CodeUrl (codeSection, int))) ->
//         //     match codeSection, int with
//         //     | GoalRoll, int ->
//         //         sprintf "/portfolio/goalRoll/%i" int 
//         //     | TileSmash, int ->
//         //         sprintf "/portfolio/tileSmash/%i" int 
//         //     | TileSort, int ->
//         //         sprintf "/portfolio/tileSort/%i" int 
//         // | Portfolio (Design int) ->
//         //     sprintf "/portfolio/design/%i" int
//         | SharedWebAppModels.Contact ->
//             "/contact"
//         // | _ ->
//         //     "/welcome"

//     let toPath =
//         function
//         | About ->
//             "/about"
//         | Portfolio ->
//             "/portfolio"
//         | Contact ->
//             "/contact"
//         | _
//         | Welcome ->
//             "/welcome"
//         // | About General ->
//         //     "/about"
//         // | About Personal ->
//         //     "/about/personal"
//         // | About Professional ->
//         //     "/about/professional"
//         // | About Resume ->
//         //     "/about/resume"
//         // | Portfolio (Code (CodeUrl (codeSection, int))) ->
//         //     match codeSection, int with
//         //     | GoalRoll, int ->
//         //         sprintf "/portfolio/goalRoll/%i" int 
//         //     | TileSmash, int ->
//         //         sprintf "/portfolio/tileSmash/%i" int 
//         //     | TileSort, int ->
//         //         sprintf "/portfolio/tileSort/%i" int 
//         // | Portfolio (Design int) ->
//         //     sprintf "/portfolio/design/%i" int
    
//     let fromPath =
//         function
//         | "/about" ->
//             SharedWebAppModels.AboutSection (SharedAboutSection.getInitialModel)
//         | "/portfolio" ->
//             SharedWebAppModels.Portfolio (SharedPortfolioGallery.PortfolioGallery)
//         | "/contact" ->
//             SharedWebAppModels.Contact
//         | "/welcome"
//         | _ ->
//             SharedWebAppModels.Welcome
//         // | About General ->
//         //     "/about"
//         // | About Personal ->
//         //     "/about/personal"
//         // | About Professional ->
//         //     "/about/professional"
//         // | About Resume ->
//         //     "/about/resume"
//         // | Portfolio (Code (CodeUrl (codeSection, int))) ->
//         //     match codeSection, int with
//         //     | GoalRoll, int ->
//         //         sprintf "/portfolio/goalRoll/%i" int 
//         //     | TileSmash, int ->
//         //         sprintf "/portfolio/tileSmash/%i" int 
//         //     | TileSort, int ->
//         //         sprintf "/portfolio/tileSort/%i" int 
//         // | Portfolio (Design int) ->
//         //     sprintf "/portfolio/design/%i" int
//         // API / SERVER CALLS
// NEEDS REFACTOR
// Ensure that the Client and Server use same end-point
module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName


type IPageApi =
    {
        GetPage :  string -> Async<SharedWebAppModels.Model>
    }