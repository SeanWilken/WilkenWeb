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

    type TapTileValue =
        | Minor
        | Modest
        | Major
        | Heart
        | Bomb

    type TapTile = {
        TapPosition: int
        LifeTime: int
        Value: TapTileValue
    }

    type TileSortValueTile = { Value : int option }

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
        | TileSortLaneObject of TileSortValueTile
        | TapTile of TapTile

    // List of LaneObjects that occupy a Grid Position
    type GridBoard = {
        GridPositions: LaneObject list
    }

    // Represents a given game state
    type RoundState =
        | Settings
        | Paused // change to idle (no game active)
        | Playing // there is an active round
        | Won // Round ended, model contains round details

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

    let modelValueAsString strin value =
        if value = -1 
            then strin + "\u221E";
            else strin + string value

module RollableGridGameHelpers =

    open GridGame

    let getPiecePositionIndex ( gameGridPositions: GridBoard ) positionObject =
        getObjectPositionIndex gameGridPositions positionObject
        |> unwrapIndex

    let getRolledBallPositionIndex wrapped ballPosition direction =
        match wrapped, direction with
        | false, MovementDirection.Up -> (ballPosition - 8)
        | true, MovementDirection.Up ->  ballPosition + 56
        | false, MovementDirection.Down -> (ballPosition + 8)
        | true, MovementDirection.Down ->  ballPosition - 56
        | false, MovementDirection.Left -> (ballPosition - 1)
        | true, MovementDirection.Left ->  ballPosition + 7
        | false, MovementDirection.Right -> (ballPosition + 1)
        | true, MovementDirection.Right ->  ballPosition - 7

    let checkDirectionalBound ballRollPositionIndex direction =
        match direction with
        | MovementDirection.Up -> ballRollPositionIndex >= 0
        | MovementDirection.Down -> ballRollPositionIndex <= 63
        | MovementDirection.Right -> ((ballRollPositionIndex) % 8) <> 0
        | MovementDirection.Left -> ((ballRollPositionIndex + 1) % 8) >= 1

    let checkGridBoardPositionForBlocker gridBoard positionIndex =
        gridBoard.GridPositions.Item ( positionIndex ) = Blocker

    let updateGameBoardMovedPosition gameBoard piecePosition movedPiecePosition =
        let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (piecePosition)
        updatePositionWithObject ballToBlankGrid Ball movedPiecePosition

    let nonWrappingPieceMovementPositionIndex ballPosition direction = 
        getRolledBallPositionIndex false ballPosition direction

    let wrappablePieceMovementPositionIndex ballPosition direction = 
        checkDirectionalBound ( getRolledBallPositionIndex false ballPosition direction ) direction
        |> fun x -> getRolledBallPositionIndex ( not x ) ballPosition direction 
    
    let checkDirectionMovementWithFunction movementPositionFunc ballPosition direction gameBoard =
        let updatedPiecePosition = movementPositionFunc ballPosition direction
        if checkGridBoardPositionForBlocker gameBoard updatedPiecePosition
            then gameBoard
            else updateGameBoardMovedPosition gameBoard ballPosition updatedPiecePosition

    let checkRollableDirectionMovement ballPosition direction gameBoard =
        checkDirectionMovementWithFunction nonWrappingPieceMovementPositionIndex ballPosition direction gameBoard
    
    let checkDirectionMovement ballPosition direction gameBoard =
        checkDirectionMovementWithFunction wrappablePieceMovementPositionIndex ballPosition direction gameBoard


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
            MovesMade: int
        }

    // --------------------------------------
    // SHARABLE (IF REFACTOR AND LEVELS MADE MORE GENERIC FOR LOAD)
    let getBallPositionIndex (gameGridPositions: GridBoard) =
        getObjectPositionIndex gameGridPositions Ball
        |> unwrapIndex

    let getGoalPositionIndex (gameGridPositions: GridBoard) =
        getObjectPositionIndex gameGridPositions Goal
        |> unwrapIndex

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
        | 1 -> Level1
        | 2 -> Level2
        | 3 -> Level3
        | _ -> Level0

    // initial model, no message or command
    let initModel =
        let round = loadRound 3;
        let initialModel = {
            LevelIndex = 3;
            InitialGrid = round;
            CurrentGrid = round;
            BallPositionIndex = getBallPositionIndex round;
            GoalPositionIndex = getGoalPositionIndex round;
            GameState = Playing;
            MovesMade = 0;
        }
        initialModel

module SharedPivotPoint = 
    open GridGame

    type LaneOrientation =
        | LaneRow
        | LaneColumn

    type Model = {
        GameBoard : GridBoard // the playing game board
        BoardOrientation: LaneOrientation
        GameState : RoundState
        DispatchPointer: float
        GameClock: int
        RollInterval: int
        BallDirection: MovementDirection // direction of ball's momentum
        BallPosition: int // position of the ball currently
        CoinPosition: int // position of the coin to collect
        CoinsCollected: int // # of coins obtained
    }

    let demoGameBoard = { 
        GridPositions = [
            Blocker; Blank; Blank; Blank; Blank; Blocker; Blank; Blank;
            Blank; Blocker; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blocker; Blank;
            Blank; Blocker; Blank; Blank; Blank; Blank; Blank; Blank;
            Blank; Blank; Blocker; Blank; Goal; Blank; Blank; Blank;
            Blank; Blank; Blocker; Blank; Blank; Blank; Blank; Blank;
            Ball; Blank; Blank; Blank; Blank; Blocker; Blank; Blank;
            Blank; Blank; Blank; Blank; Blank; Blank; Blank; Blocker; 
        ] }

    let initModel = {
        GameBoard = demoGameBoard
        GameState = Paused
        BoardOrientation = LaneColumn
        GameClock = 0
        RollInterval = 0
        DispatchPointer = 0.0
        BallDirection = Right
        BallPosition = 11
        CoinPosition = 37
        CoinsCollected = 0
    }

module SharedTileTap =

    open GridGame

    type TileTapDifficulty =
        | Simple
        | Easy
        | Intermediate
        | Hard

    // KINDA USELESS IMPLEMENTATION CURRENTLY, 
    // NEEDS MORE TWEAKS
    type TileTapGameMode =
        | Survival
        | TimeAttack
    (*
        Simple:
            - Mistakes are ignored.
            - 30 Second Round Timer
        Easy:
            - 5 Mistake Limit
            - 30 Second Round Timer
        Medium:
            - 3 Mistake Limit
            - 30 Second Round Timer
        Hard (Survival):
            - Go until you fail
            - No Mistakes, insta-fail
            - No Round Timer
    *)

    // function to determine the model based on difficulty
        // Simple -> model with AllowableMistakes = -1, timer = 30
        // Easy -> model with mistakes = 5, timer = 30
        // Medium -> model with mistakes = 3, timer = 30
        // Hard -> model with mistakes = 1, timer = -1
        // Time Attack (?) -> Hearts (-1 mistake), Clocks (slow down or speed up), Bomb (insta-fail)

    // RoundConditions ->
        // AllowableRoundMistakes ->
            // ARM < 0 = Ignore Mistakes
            // ARM > 0 = Mistakes allowed up to ARM
                // RoundMistakes >= ARM then RoundEnd
        // RoundTimer ->
            // RT < 0 = Ignore Timer, play until ARM reached
            // RT > 0 = Timer value in seconds, checks against ( GameClock ticks / 4 )
                // RoundTimer < ( GameClock / 4 ) then RoundEnd

    type TileTapRoundDetails = {
        RoundMistakes: int // How many mistakes were made that Round
        TilesSpawned: int // Current # of tiles spawned on the board 
        TilesSmashed: int // # of tiles destroyed by the player
        RoundScore: int // Score of tiles destroyed within Round
        GameClock: int
    }

    let emptyTileTapRoundDetails = {
        RoundMistakes = 0
        TilesSpawned = 0
        TilesSmashed = 0
        RoundScore = 0
        GameClock = 0
    }

    type Model = {
        TileTapGridBoard: GridBoard // grid board that will contain the various tiles
        LastSpawnInterval: int // Cooldown of new Tile being placed into the GameGrid
        GameMode: TileTapGameMode
        GameState: GridGame.RoundState
        DispatchPointer: float // the float pointer to the GameLoop's dispatch
        RoundTimer: int // Max allowable seconds for this Round on GameClock
        AllowableRoundMistakes: int // max # of mistakes allowed before the round is considered 'lost' and will end
        // RoundTileLifeTime: int // How many GameTicks the tile will live for // tie into Value?
        CurrentRoundDetails: TileTapRoundDetails
        CompletedRoundDetails: TileTapRoundDetails
    }

    let levelCeiling = 1
    let gridDimension = 8
    let generateEmptyTileTapGrid gridDimension =
        { GridPositions = [
                for i in 0 .. ((gridDimension * gridDimension) - 1) do
                    Blank
            ]
        }

    let initModel = {
        TileTapGridBoard = generateEmptyTileTapGrid gridDimension
        LastSpawnInterval = 2
        GameMode = Survival
        GameState = Paused
        DispatchPointer = 0.0
        RoundTimer = 30
        AllowableRoundMistakes = 5
        // RoundTileLifeTime = 15 (just under 4 seconds)
        CurrentRoundDetails = emptyTileTapRoundDetails
        CompletedRoundDetails = emptyTileTapRoundDetails
    }

    // doesn't fucking work, pissing me the fuck off
    let endRound model =
        { model with
            TileTapGridBoard = generateEmptyTileTapGrid gridDimension
            GameState = Won
            DispatchPointer = 0.0
            CurrentRoundDetails = emptyTileTapRoundDetails
            CompletedRoundDetails = model.CurrentRoundDetails
        }

    let resetRound model = 
        { model with
            TileTapGridBoard = generateEmptyTileTapGrid gridDimension
            LastSpawnInterval = 2
            GameState = Paused
            DispatchPointer = 0.0
            CurrentRoundDetails = emptyTileTapRoundDetails
        }

    // When ChangeDifficulty Msg is dispatched,
    // returns model with different round parameters
    // based on requested TileTapDifficulty
    let updateSurvivalModeDifficulty  ( model : Model ) ( difficulty : TileTapDifficulty ) =
        match difficulty with
        | Simple -> { model with RoundTimer = 30; AllowableRoundMistakes = 10 }
        | Easy -> { model with RoundTimer = 60; AllowableRoundMistakes = 5 }
        | Intermediate -> { model with RoundTimer = -1; AllowableRoundMistakes = 3 }
        | Hard -> { model with RoundTimer = -1; AllowableRoundMistakes = 1 }

    let updateTimeAttackModeDifficulty  ( model : Model ) ( difficulty : TileTapDifficulty ) =
        match difficulty with
        | Simple -> { model with RoundTimer = 90; AllowableRoundMistakes = -1 }
        | Easy -> { model with RoundTimer = 60; AllowableRoundMistakes = -1 }
        | Intermediate -> { model with RoundTimer = 45; AllowableRoundMistakes = -1 }
        | Hard -> { model with RoundTimer = 30; AllowableRoundMistakes = -1 }

    // When ChangeDifficulty Msg is dispatched,
    // returns model with different round parameters
    // based on requested TileTapDifficulty
    let updateModelGameMode  ( model : Model ) ( mode : TileTapGameMode ) =
        match mode with
        | Survival -> { model with GameMode = Survival; RoundTimer = 30; AllowableRoundMistakes = 10 } // Timer will last as long as you will
        | TimeAttack -> { model with GameMode = TimeAttack; RoundTimer = 30; AllowableRoundMistakes = -1 }

module SharedTileSort =

    open GridGame

    // Determines the grid dimensions
    type TileSortDifficulty =
        | Simple // 3x3
        | Easy // 4x4
        | Medium // 5x5
        | Hard // 6x6

    type Model = {
        Difficulty: TileSortDifficulty // default at initial
        InitialTiles: GridBoard // set at start of round
        CurrentTiles: GridBoard // changing list of game tiles to represent moves
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
    let selectRandomTilePosition ( assignedTiles: GridBoard ) = assignedTiles.GridPositions.[randomIndex(assignedTiles.GridPositions.Length)]

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
    let unassignedTiles = { GridPositions = [] }

    // recursive function to iterate through list of generated game tile values
    let rec createRandomGameTiles (assignedTiles: GridBoard) (remainingIndexes: int list) =
        // take random element from the array list
        let randomTileValue = remainingIndexes.[randomIndex remainingIndexes.Length]
        // filter out the selected element
        let remain = List.filter(fun elem -> elem <> randomTileValue) remainingIndexes
        // create game tile from index and the selected element // THIS SHOULD USE A CONSTRUCTOR FUNCTION
        // let gameTile = { SortTile = Some randomTileValue;}
        let gameTile = GridGame.TileSortLaneObject { Value = Some randomTileValue }
        // recurse back through if there are remaining positions
        match remain with
        | [] -> { GridPositions = gameTile :: assignedTiles.GridPositions }
        | y::ys -> createRandomGameTiles { GridPositions = gameTile :: assignedTiles.GridPositions } remain

    // Step 3: Randomly blank a tile value to None, to act as the void space for sliding a tile.
    // remove one entry randomly to get the blank starting position
    let randomBlankPosition (assignedTiles: GridBoard) =
        let selectedTile = selectRandomTilePosition assignedTiles
        List.map (fun x -> if x = selectedTile then GridGame.TileSortLaneObject { Value = None } else x ) assignedTiles.GridPositions

    // STEP 4: ASSIGN GENERATED TILE ROUND TO INITIAL AND CURRENT
    let createNewRoundGameBoardBasedOnDifficulty difficulty =
        let newRoundTilePositions = generateGameBoardPositionsBasedOffDifficulty difficulty
        let randomizedNewRoundTilePositions = createRandomGameTiles unassignedTiles newRoundTilePositions
        {GridPositions = randomBlankPosition randomizedNewRoundTilePositions}

    //---------------------
    let getValueOrZeroFromGameTile (gameTile) =
        match gameTile with
        | GridGame.TileSortLaneObject i ->
            match i.Value with
            | Some i -> i
            | None -> 0
        | _ -> 0

    let convertValueToProperString tileValue =
        match tileValue with
        | 0 -> ""
        | _ -> string tileValue

    // //---------------------
    let selectedTilePositionIndex (gameGrid : GridBoard) selectedTile : int option =
        gameGrid.GridPositions
        |> List.tryFindIndex ( fun x -> x = selectedTile )
    
    let blankTilePositionIndex (gameGrid : GridBoard) =
        gameGrid.GridPositions
        |> List.tryFindIndex (
            fun x ->
                match x with
                | TileSortLaneObject tile ->
                    tile.Value = None
                | _ -> false
        )

    let checkTileGroup tileGroup selected =
        [ for tile in tileGroup do
            if tile = selected then true else false ]
        |> List.contains true

    let sortTilesFromGridBoard grid =
        List.filter ( fun x -> 
            match x with 
            | TileSortLaneObject x ->
                true
            | _ -> false
        ) grid.GridPositions

    let checkSwapInColumn selectedIndex blankIndex tiles selected gridDimension =
        let tileColumns = 
            getPositionsAsColumns tiles gridDimension
        let selectedColIndex = 
            unwrapIndex ( List.tryFindIndex ( fun x -> checkTileGroup x selected ) ( tileColumns ) )
        let blankColIndex = 
            unwrapIndex ( List.tryFindIndex ( fun x -> checkTileGroup x  ( TileSortLaneObject { Value = None } ) ) ( tileColumns ) )
        
        if ( selectedColIndex <> -1 && selectedColIndex = blankColIndex )
            then
                if selectedIndex + gridDimension = blankIndex || selectedIndex - gridDimension = blankIndex
                    then true
                    else false
            else false

    let checkSwapInRow selectedIndex blankIndex tiles selected gridDimension =
        let tileRows = 
            getPositionsAsRows tiles gridDimension
        let selectedColIndex = 
            unwrapIndex ( List.tryFindIndex ( fun x -> checkTileGroup x selected ) ( tileRows ) )
        let blankColIndex = 
            unwrapIndex ( List.tryFindIndex ( fun x -> checkTileGroup x  ( TileSortLaneObject { Value = None } ) ) ( tileRows ) )
        if ( selectedColIndex <> -1 && selectedColIndex = blankColIndex )
            then
                if selectedIndex + 1 = blankIndex || selectedIndex - 1 = blankIndex
                    then true
                    else false
            else false

    let swapBlankWithSelected tiles selected selectedIndex blankIndex =
        let selectedSwapped = updatePositionWithObject tiles selected blankIndex
        let blankSwapped = updatePositionWithObject selectedSwapped (TileSortLaneObject { Value = None }) selectedIndex
        blankSwapped

    let selectedCanSwapWithBlank tiles selected gridDimension =
        let selectedIndex = unwrapIndex ( getObjectPositionIndex tiles selected )
        let blankIndex = unwrapIndex ( getObjectPositionIndex tiles ( TileSortLaneObject { Value = None } ) )
        if selectedIndex = -1 || blankIndex = -1
            then tiles
            else
                let colSwap = checkSwapInColumn selectedIndex blankIndex tiles selected gridDimension
                let rowSwap = checkSwapInRow selectedIndex blankIndex tiles selected gridDimension
                if (colSwap || rowSwap) 
                    then swapBlankWithSelected tiles selected selectedIndex blankIndex
                    else tiles

    // // BLANK TILE VALUE POSITION VALIDATION
    // // gets the current index of the Blank Tile and adds one to the index
    let caclulatedValueOfBlankTile currentTiles =
        match (blankTilePositionIndex currentTiles) with
        | Some i -> Some (i + 1)
        | _ -> None

    // checks that the calculated value of the blank tile doesn't exist in the tiles
    // list should be empty, as the missing value should not exist
    let calculatedBlankValueNotFoundInTiles currentTiles =
        let missingValue = caclulatedValueOfBlankTile currentTiles
        let filteredByMissing = List.filter (fun x -> x = GridGame.TileSortLaneObject { Value = missingValue }) currentTiles.GridPositions
        List.isEmpty filteredByMissing


    // simplify calls of calculating and checking Blanks calculated value
    let checkBlankTileIsAtCorrectPosition currentTiles =
        calculatedBlankValueNotFoundInTiles currentTiles

    //---------------------
    // VALUE POSITION VALIDATION
    // gameboard list is the same as the list if sorted by value
    // need to filter out None
    let checkTilesInCorrectOrder (currentTiles: GridBoard) =
        (List.filter (fun x -> x <> GridGame.TileSortLaneObject { Value = None }) currentTiles.GridPositions) = (List.filter (fun x -> x <> GridGame.TileSortLaneObject { Value = None }) currentTiles.GridPositions |> List.sortBy (fun x -> Some x))


    //---------------------
    // WIN CONDITIONS VALIDATION
    let winValidator currentTiles =
        checkTilesInCorrectOrder currentTiles && checkBlankTileIsAtCorrectPosition currentTiles

    //---------------------
    // CHANGE DIFFICULTY - UPDATE DIFFICULTY WITH SELECTION
    let changeDifficulty difficulty model =
        { model with Difficulty = difficulty }

    // MOVE TILE - UPDATE TURNS WITH SELECTED
    let addTileValueToTurnHistory selectedTile model =
        { model with Turns = (selectedTile :: model.Turns) }

    // MOVE TILE - UPDATE CURRENT WITH SELECTED & BLANK SWAPPED
    let updateCurrentTilesWithMove gameboard model =
        { model with CurrentTiles = gameboard }

    // REWIND - POP HEAD FROM TURNS, SWAP WITH BLANK (REMOVES HEAD FROM TURNS)
    let rewindCurrentTiles model =
        if not (List.isEmpty model.Turns) then
            let rewindTile = model.Turns.Head
            let selectedTile = TileSortLaneObject { Value = Some rewindTile }
            let selectedIndex = unwrapIndex ( selectedTilePositionIndex model.CurrentTiles selectedTile )
            let blankIndex = unwrapIndex ( blankTilePositionIndex model.CurrentTiles )
            { model with 
                CurrentTiles = swapBlankWithSelected (model.CurrentTiles) (selectedTile) ( selectedIndex ) (blankIndex)
                Turns = model.Turns.Tail
            }
        else model

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

    let initModel =
        let initialDifficulty = Simple
        let initialRound = createNewRoundGameBoardBasedOnDifficulty initialDifficulty
        {
            Difficulty = initialDifficulty
            CurrentTiles = initialRound
            InitialTiles = initialRound
            Turns = []
            GameState = Playing
        }

module SharedCodeGallery =
    
    open SharedGoalRoll
    open SharedTileTap
    open SharedTileSort

    type Model =
        | CodeGallery
        | GoalRoll of SharedGoalRoll.Model
        | TileTap of SharedTileTap.Model
        | TileSort of SharedTileSort.Model
        | PivotPoint of SharedPivotPoint.Model

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

    type Model = {
        ActiveModalIndex : int
        ModalIsActive : bool
    }

    // add images
    // revert back to string list for bullet points?
    type ModalContent = {
        Title: string
        MainContent: string //list
        PreviousLabel: string
        NextLabel: string
    }
       
    let getInitialModel = {ActiveModalIndex = 0; ModalIsActive = false}

module SharedWebAppModels =

    // Represents which of the web app's subsections is to be displayed
    // Welcome -> not much to see here, a landing page with element to drive along user interaction
    // AboutSection -> Overview of the purpose of the web app, in this case some details about it's creator
    // Portfolio -> Split view landing page to separate categories from one another at a high level
    // Contact -> How to get in touch with the entity the web app represents
    type Model =
        | Welcome
        | About of SharedAboutSection.Model
        | Portfolio of SharedPortfolioGallery.Model
        | Contact


module SharedWebAppViewSections =
    type AppSection =
    | WelcomeAppView
    | AboutAppView
    | PortfolioAppLandingView
    | PortfolioAppCodeView
    | PortfolioAppDesignView
    | ContactAppView

    let appSectionStringTitle appSection =
        match appSection with
        | WelcomeAppView -> "Welcome"
        | AboutAppView -> "About"
        | PortfolioAppLandingView -> "Portfolio"
        | PortfolioAppCodeView -> "Code"
        | PortfolioAppDesignView -> "Design"
        | ContactAppView -> "Contact"

// Ensure that the Client and Server use same end-point
module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IPageApi =
    { GetPage :  string -> Async<SharedWebAppModels.Model> }