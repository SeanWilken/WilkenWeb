module TileSort

open System
open Elmish
// open Shared
open Shared.SharedTileSort

// CURRENTLY BROKEN WHEN USING SHARED, AS I TRIED TO UPDATE TO USE GRIDGAME AND BROKE STUFF
// TODO:
    //IMPLEMENT:
        //Move Count Display
        //Round Timer
    // FIX THE IPHONE & BACKGROUND, SHOWS LINE ACROSS RIGHT HAND SIDE OF SCREEN
    // CLEAN COMMENTS

type Msg =
    | NewRound // Starts a New Round, initializing the board again with the selected difficulty
    | ResetRound // Resets the round to the same initial configuration
    | RewindMove // Undo the last tile move made by the user
    | MoveTile of GameTile//GridGame.LaneObject // Move the selected tile to the empty space
    | UpdateDifficulty of Shared.SharedTileSort.TileSortDifficulty // Change the current difficulty for the given one
    | CheckSolution // Run the current board through the win validation check logic
    | Won // Board is in the winning configuration
    | QuitGame // Quits back one level, exiting the game.
    | ChangeView // NEW WIP EWW
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
        ContentView = Modal // NEW WIP EWW
    }
    model, Cmd.none
//---------------------
let update ( msg: Msg ) ( model: Shared.SharedTileSort.Model ): Shared.SharedTileSort.Model * Cmd<Msg> =
    match msg with
    | NewRound ->
        let newRound = createNewRound model
        newRound, Cmd.none
    | ResetRound ->
        resetRound model, Cmd.none
    | MoveTile gameTile ->
        let gameTileValue = 
            match gameTile.Value with
            | Some i ->
                let tilesAfterMove = canSwapSelectedTileWithBlank model.CurrentTiles ( Some i ) model.Difficulty
                if( tilesAfterMove = model.CurrentTiles ) then model, Cmd.ofMsg CheckSolution
                else { model with CurrentTiles = tilesAfterMove; Turns = (i) :: model.Turns }, Cmd.ofMsg CheckSolution
            | None ->
                model, Cmd.none
        gameTileValue
    | RewindMove ->
        rewindCurrentTiles model, Cmd.none
    | UpdateDifficulty difficulty ->
        let newDiff = changeDifficulty difficulty model
        let newRound = createNewRound newDiff
        newRound, Cmd.none
    | ChangeView -> // NEW WIP EWW
        let swappedViewModel = 
            if model.ContentView = Modal 
                then { model with ContentView = Card } 
                else { model with ContentView = Modal }
        swappedViewModel, Cmd.none
    | CheckSolution ->
        match winValidator model.CurrentTiles with
        | true -> 
            model, Cmd.ofMsg Won
        | false -> model, Cmd.none
    | Won -> { model with GameState = Shared.GridGame.Won }, Cmd.none // Should do more
    | QuitGame -> model, Cmd.ofMsg QuitGame 

open Fable.React
open Fable.React.Props
open Fulma

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

// VIEW

let tileSortDescriptions = [ 
    "- Rearrange the tiles in correct ascending order, starting with the top left position being the lowest number."
    "- Select one of the tiles adjacent to the empty space to slide that tile into the blank."
    "- The blank space must match the missing number." 
]
let sourceCodeLinks = [
    "Model", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Shared/Shared.fs"
    "View", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Shared/Index.fs"
    "Logic", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Portfolio/Games/TileSort/Index.fs"
]
let gameControls = [
    // QUIT GAME NEEDS TO BE ON CARD VIEW?
    // "Quit Game", QuitGame
    "New Round", NewRound
    "Reset Round", ResetRound
    "Undo Move", RewindMove
    "3 x 3", UpdateDifficulty Simple
    "4 x 4", UpdateDifficulty Easy
    "5 x 5", UpdateDifficulty Medium
    "6 x 6", UpdateDifficulty Hard
    "Toggle View", ChangeView
]

//header
let tileSortHeader dispatch =
    SharedViewModule.sharedModalHeaderControls "Tile Sort" QuitGame dispatch

// left content
let tileSortLeftModal =
    SharedViewModule.sharedModalLeft tileSortDescriptions sourceCodeLinks

// main content
let tileSortRowCreator ( tileRow: GameTile list ) ( dispatch: Msg -> unit ) =
    Tile.parent [] [
        for tile in tileRow do
            let displayValue = convertValueToProperString ( getValueOrZeroFromGameTile tile )
            let tileClass = if ( displayValue <> "" ) then "valueTile" else "blankTile"
            Column.column [] [ Tile.child [] [ Box.box' [ Props [ ClassName tileClass; OnClick( fun _ -> MoveTile tile |> dispatch ) ]  ] [ str displayValue ] ] ]
    ]
let tileSortGameBoard model dispatch =
    let tileRows = Shared.SharedTileSort.getTilesAsRows model.CurrentTiles model.Difficulty
    Container.container [ Container.CustomClass "tileSortCenter" ] [ for row in tileRows do tileSortRowCreator row dispatch ]
// modal content container
let tileSortModalContent model dispatch =
    match model.GameState with 
    | Shared.GridGame.Won -> div [ ClassName "levelCompletedCard" ] [ str "Congrats, you win!!!" ]
    | Shared.GridGame.Playing -> tileSortGameBoard model dispatch
// right content controls
let tileSortModalRight dispatch =
    SharedViewModule.sharedModalRight gameControls dispatch

// main view
let view model dispatch =
    SharedViewModule.sharedModal ( tileSortHeader dispatch ) ( tileSortLeftModal ) ( tileSortModalContent model dispatch ) ( tileSortModalRight dispatch )





// card style
let tileSortHeaderCard =
    SharedViewModule.contentHeaderCard "Tile Sort" sourceCodeLinks tileSortDescriptions

let tileSortContentHeaderControls dispatch =
    SharedViewModule.contentHeaderControls gameControls dispatch

let tileSortCardView model dispatch =
    SharedViewModule.sharedContentCardView ( tileSortHeaderCard ) ( tileSortContentHeaderControls dispatch ) ( tileSortModalContent model dispatch ) ( dispatch )


// OLD STYLE OF CONTENT CARDS
// make generic also
// MOVE TO ANOTHER SECTION, SWITCH VIEW TYPE?
    // MODAL?
    // CONTENT CARDS?

// TILE SORT CONTROLS HEADER
// let tileSortHeader =
//     Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
//         Container.container [] [
//             Columns.columns [ Columns.IsVCentered ] [
//                 Column.column [] [
//                     a [ Href "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Shared/Shared.fs" ] [ 
//                         h2 [] [ str "Shared Code" ]
//                     ]
//                 ]
//                 Column.column [] [ h1 [] [ str "Tile Sort" ] ]
//                 Column.column [] [ 
//                     a [ Href "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Portfolio/Games/TileSort/Index.fs" ] [ 
//                         h2 [] [ str "Client Code" ]
//                     ]
//                 ]
//             ]
//             p [] [ str "- Rearrange the tiles in correct ascending order, starting with the top left position being the lowest number." ]
//             p [] [ str "- Select one of the tiles adjacent to the empty space to slide that tile into the blank." ]
//             p [] [ str "- The blank space must match the missing number." ]
//             // p [] [ str "- Select a difficulty: Simple = 3 x 3; Easy = 4 x 4; Medium = 5 x 5; Hard = 6 x 6." ]
//             // p [] [ str "- Use the 'Undo Turn' button to rewind the previous action (back to the initial board)." ]
//             // p [] [ str "- Use the 'New Round' button to generate a new board of the selected difficulty." ]
//             // p [] [ str "- Use the 'Reset Round' button to set the board back to it's initial state." ]
//         ]
//     ]
// // DIFFICULTY SELECTOR CONTROLS
// let difficultySelector ( currentDifficulty: TileSortDifficulty ) dispatch =
//     Level.item [ Level.Item.HasTextCentered; ] [
//         Level.item [] [ p [] [ str "Difficulty:"] ]
//         for difficulty in tileDifficulties do
//             let unionCaseDifficultyName = difficulty.Name
//             let unionCaseDifficulty =  decodeDifficultyByString unionCaseDifficultyName
//             if unionCaseDifficulty = currentDifficulty then 
//                 Level.item [ Level.Item.HasTextCentered ] [ p [] [ str unionCaseDifficultyName ] ]
//             else 
//                 Level.item [ Level.Item.HasTextCentered ] [ a [ OnClick ( fun _ -> UpdateDifficulty unionCaseDifficulty |> dispatch ) ] [ str unionCaseDifficultyName ] ]
//     ]

// // DIFFICULTY SELECTION & ROUND CONTROLS
// let tileSortHeaderControls difficulty dispatch =
//     Container.container [ Container.Props [ ClassName "gameGridControlBar"] ] [
//         Level.level [] [
//             difficultySelector difficulty dispatch
//             Level.item [] [ a [ OnClick(fun _ -> NewRound |> dispatch ) ] [ str "New Round" ] ]
//             Level.item [] [ a [ OnClick(fun _ -> ResetRound |> dispatch ) ] [ str "Reset Round" ] ]
//             Level.item [] [ a [ OnClick(fun _ -> RewindMove |> dispatch ) ] [ str "Undo Turn" ] ]
//         ]
//     ]

// // TILE GRID ROW
// let generateTileGridRow ( tileRow: GameTile list ) ( dispatch: Msg -> unit ) =
//     Tile.ancestor [] [ 
//         for tile in tileRow do
//             let displayValue = convertValueToProperString ( getValueOrZeroFromGameTile tile )
//             let tileClass = if ( displayValue <> "" ) then "valueTile" else "blankTile"
//             Tile.parent [] [ 
//                 Tile.child [] [
//                     Box.box' [ Props [ ClassName tileClass; OnClick( fun _ -> MoveTile tile |> dispatch ) ]  ] [ str displayValue ] 
//                 ] 
//             ]
//         ]
// let gameGrid model dispatch =
//     let tileRows = Shared.SharedTileSort.getTilesAsRows model.CurrentTiles model.Difficulty
//     Container.container [ Container.Props [  ] ] [
//         for row in tileRows do
//             generateTileGridRow row dispatch
//     ]

// // TILE SORT GRID VIEW
// let view ( model : Model ) ( dispatch : Msg -> unit ) =
//     div [] [
//         SharedViewModule.backToGallery QuitGame dispatch
//         tileSortHeader
//         tileSortHeaderControls model.Difficulty dispatch
//         match model.GameState with
//         | Shared.GridGame.Won ->
//             div [ ClassName "levelCompletedCard"; ] [ str "Congrats, you win!!!" ] //Style [ Padding 15; Border "1px solid #000000"; FontSize 100; TextAlign TextAlignOptions.Center; Color "#FF2843"; FontFamily "Philosopher"]
//         | Shared.GridGame.Playing ->
//             Container.container [ Container.Props [ ClassName "gameGridContainer" ] ] [ gameGrid model dispatch ]
//     ]
    
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