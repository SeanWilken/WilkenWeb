module TileSort

open Elmish
open Shared.SharedTileSort
open Shared.GridGame
open Shared

// CURRENTLY BROKEN WHEN USING SHARED, AS I TRIED TO UPDATE TO USE GRIDGAME AND BROKE STUFF
// TODO:
    //IMPLEMENT:
        //Move Count Display
        //Round Timer
    // FIX THE IPHONE & BACKGROUND, SHOWS LINE ACROSS RIGHT HAND SIDE OF SCREEN
    // CLEAN COMMENTS

type Msg =
    | SetGameState of Shared.GridGame.RoundState
    | NewRound // Starts a New Round, initializing the board again with the selected difficulty
    | ResetRound // Resets the round to the same initial configuration
    | RewindMove // Undo the last tile move made by the user
    | MoveTile of TileSortValueTile
    | UpdateDifficulty of Shared.SharedTileSort.TileSortDifficulty // Change the current difficulty for the given one
    | CheckSolution // Run the current board through the win validation check logic
    | Solved // Board is in the winning configuration
    | QuitGame // Quits back one level, exiting the game.

// DIFFICULTY HELPERS
// GET CASES FROM DESCRIMINATED UNION
open FSharp.Reflection
let tileDifficulties = FSharpType.GetUnionCases typeof<TileSortDifficulty>
let decodeDifficultyByString string =
    match string with
    | "Simple" -> Simple
    | "Easy" -> Easy
    | "Medium" -> Medium
    | "Hard" -> Hard
    | _ -> Simple

let difficultyToString difficulty =
    match difficulty with
    | Simple -> "3x3 - Simple"
    | Easy -> "4x4 - Easy"
    | Medium -> "5x5 - Medium"
    | Hard -> "6x6 - Hard"

// VIEW
let tileSortDescriptions = [ 
    "- Rearrange the tiles in correct ascending order, starting @ the top left position."
    "- Select one of the tiles adjacent to the empty space to slide that tile into the blank."
    "- The blank space must match the missing number." 
]

let controlList = [ 
    "Play", (SetGameState (Playing))
    "Settings", (SetGameState (Settings)) 
]

let gameControls = [
    "New Round", NewRound
    "Reset Round", ResetRound
    "Undo Move", RewindMove
    "3 x 3", UpdateDifficulty Simple
    "4 x 4", UpdateDifficulty Easy
    "5 x 5", UpdateDifficulty Medium
    "6 x 6", UpdateDifficulty Hard
]

let modelTileSortRoundDetails ( model : SharedTileSort.Model ) = [
    "You completed " + difficultyToString model.Difficulty + " difficulty."
    "It took you " + string model.Turns.Length + " number of moves."
]

//---------------------

let init (): Shared.SharedTileSort.Model * Cmd<Msg> =
    let initialDifficulty = Shared.SharedTileSort.Simple
    let initialRound = Shared.SharedTileSort.createNewRoundGameBoardBasedOnDifficulty initialDifficulty
    let model = {
        Difficulty = initialDifficulty
        CurrentTiles = initialRound
        InitialTiles = initialRound
        Turns = []
        GameState = Shared.GridGame.Playing
    }
    model, Cmd.none
//---------------------
let update ( msg: Msg ) ( model: Shared.SharedTileSort.Model ): Shared.SharedTileSort.Model * Cmd<Msg> =
    match msg with
    | SetGameState gameState ->
        { model with GameState = gameState }, Cmd.none
    | NewRound ->
        let newRound = createNewRound model
        newRound, Cmd.none
    | ResetRound ->
        resetRound model, Cmd.none
    | MoveTile gameTile ->
        match gameTile.Value with
        | Some i ->
            let gridAfterTileMove = selectedCanSwapWithBlank model.CurrentTiles ( TileSortLaneObject gameTile ) ( getGridDimension model.Difficulty )
            { model with CurrentTiles = gridAfterTileMove}, Cmd.none
        | None ->
            model, Cmd.none
    | RewindMove ->
        rewindCurrentTiles model, Cmd.none
    | UpdateDifficulty difficulty ->
        let newDiff = changeDifficulty difficulty model
        let newRound = createNewRound newDiff
        newRound, Cmd.none
    | CheckSolution ->
        match winValidator model.CurrentTiles with
        | true -> 
            { model with GameState = Shared.GridGame.Won }, Cmd.none
        | false -> model, Cmd.none
    | Solved -> { model with GameState = Shared.GridGame.Won }, Cmd.none // Was used as a test state for button
    | QuitGame -> model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let laneObjectToTileSortTile tile dispatch = 
    match tile with
    | TileSortLaneObject tileValue ->
            Tile.child [] [ 
                let displayValue = convertValueToProperString ( getValueOrZeroFromGameTile tile )
                let tileClass = "valueTile"
                Box.box' [ Props [ ClassName tileClass; OnClick( fun _ -> MoveTile ( tileValue ) |> dispatch ) ]  ] [ Image.image [] []; str displayValue ] 
            ]
    | _ ->
            Tile.child [] [ 
                let tileClass = "emptyTile"
                Box.box' [ Props [ ClassName tileClass; OnClick( fun _ -> MoveTile { Value = None } |> dispatch ) ]  ] [ Image.image [] [] ] 
            ]

// main content
let tileSortRowCreator ( tileRow: LaneObject list ) ( dispatch: Msg -> unit ) =
    div [ Style [ Display DisplayOptions.Flex; AlignItems AlignItemsOptions.Center; JustifyContent "center"; Margin "auto" ] ] [
    // Level.level [ Level.Level.IsMobile ] [
        for tile in tileRow do
            laneObjectToTileSortTile tile dispatch
    ]

let tileSortGameBoard model dispatch =
    let tileRows = getPositionsAsRows model.CurrentTiles ( getGridDimension model.Difficulty )
    div [] [ for row in tileRows do tileSortRowCreator row dispatch ]

// modal content container
let tileSortModalContent model dispatch =
    SharedViewModule.gameModalContent ( 
        match model.GameState with 
        | Shared.GridGame.Settings -> 
            div [] [
                SharedViewModule.codeModalInstructionContent tileSortDescriptions
                SharedViewModule.codeModalControlsContent gameControls dispatch
            ]
        | Shared.GridGame.Won -> SharedViewModule.roundCompleteContent ( modelTileSortRoundDetails model )
        | Shared.GridGame.Playing
        | Shared.GridGame.Paused -> 
            Level.level [] [
                Level.item [] [
                    tileSortGameBoard model dispatch 
                ]
            ]
    )

// 2.0

// main view
let view model dispatch =
    div [] [
        ( SharedViewModule.sharedModalHeader "Tile Sort" QuitGame dispatch )
        ( tileSortModalContent model dispatch ) 
        ( SharedViewModule.codeModalFooter controlList dispatch )
    ]

// TEST ABOVE FUNCTIONALITY AND LOGIC
// GENERATE A DIFFICULTY HARD TILE LIST
// let hardModeGameBoardPositions = generateGameBoardPositionsBasedOffDifficulty Hard
// let randomizedGeneratedHardGamePositions = createRandomGameTiles unassignedTiles hardModeGameBoardPositions
// let randomTileGameRoundHard = randomBlankPosition randomizedGeneratedHardGamePositions
// // REAL WAY TO CREATE GAME
// let newRoundTest = createNewRoundGameBoardBasedOnDifficulty Hard
// // AS ROWS: FOR VISIBILTY
// let randomTileGameRoundHardAsRows = getTilesAsRows newRoundTest Hard
// // AS COLUMNS: FOR VISIBILTY
// let randomTileGameRoundHardAsColumns = getTilesAsColumns newRoundTest Hard
// //explicitly sorted out correct solution
// let explicitHardSolution30BlankValue = { GameTiles = [   
//         {Value = Some 1 }; { Value = Some 2 }; { Value = Some 3 }; { Value = Some 4 }; { Value = Some 5 }; { Value = Some 6 }; // row representation
//         { Value = Some 7 }; { Value = Some 8 }; { Value = Some 9 }; { Value = Some 10 }; { Value = Some 11 }; { Value = Some 12 };
//         { Value = Some 13 }; { Value = Some 14 }; { Value = Some 15 }; { Value = Some 16 }; { Value = Some 17 }; { Value = Some 18 };
//         { Value = Some 19 }; { Value = Some 20 }; { Value = Some 21 }; { Value = Some 22 }; { Value = Some 23 }; { Value = Some 24 };
//         { Value = Some 25 }; { Value = Some 26 }; { Value = Some 27 }; { Value = Some 28 }; { Value = Some 29 }; { Value = None };
//         { Value = Some 31 }; { Value = Some 32 }; { Value = Some 33 }; { Value = Some 34 }; { Value = Some 35 }; { Value = Some 36 }
//     ]}
// let explicitHardSolution30BlankValueAsRows = getTilesAsRows explicitHardSolution30BlankValue Hard
// let explicitHardSolution30BlankValueAsColumns = getTilesAsColumns explicitHardSolution30BlankValue Hard
// // check valid move -> currentTiles, the selected tile value, difficulty
// let noneSelectedReturnFalse = checkTileMoveInRows explicitHardSolution30BlankValue (None) Hard
// let selected29ReturnTrue = checkTileMoveInRows explicitHardSolution30BlankValue (Some 29) Hard
// let selected18BlockedBy24ReturnFalse = checkTileMoveInColumns explicitHardSolution30BlankValue (Some 18) Hard
// let selected24AboveBlankReturnTrue = checkTileMoveInColumns explicitHardSolution30BlankValue (Some 24) Hard
// let selected36BelowBlankReturnTrue = checkTileMoveInColumns explicitHardSolution30BlankValue (Some 36) Hard
// // VALID MOVES, RETURNS SELECTED SWAPPED WITH BLANK
// let explicitHard30BlankSwappedWith24InColumn = canSwapSelectedTileWithBlank explicitHardSolution30BlankValue (Some 24) Hard
// let explicitHard30BlankSwappedWith29InRow = canSwapSelectedTileWithBlank explicitHardSolution30BlankValue (Some 29) Hard
// let explicitHard30BlankSwappedWith36InColumn = canSwapSelectedTileWithBlank explicitHardSolution30BlankValue (Some 36) Hard
// // INVALID MOVES, RETURNS TILEBOARD UNCHANGED
// let unchangedExplicitHardSolution30BlankValueFromNoneSwap = canSwapSelectedTileWithBlank explicitHardSolution30BlankValue (None) Hard
// let unchangedExplicitHardSolution30BlankValueFromNotValidSwap = canSwapSelectedTileWithBlank explicitHardSolution30BlankValue (Some 18) Hard
// //win case validation testing
// let blankAtIncorrectIndexReturnFalse = winValidator explicitHard30BlankSwappedWith36InColumn
// let tilesAreRandomReturnFalse = winValidator newRoundTest
// let YouWinTrueCase = winValidator explicitHardSolution30BlankValue