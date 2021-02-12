module TileTap

open System
open FSharp
open Shared
open GridGame
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

type Msg =
    | QuitGame

let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none
let update msg model =
    match msg with
    | QuitGame ->
        model, Cmd.ofMsg QuitGame

// Time drives main game state, as things happen in intervals contained within the main loop
    // tile expire -> scales with time remaining
    // smaller game grid, doesn't work well on 1080 (ALSO OTHER SECTIONS REVIEW)
    // What is a tile smash board
        // empty board with spaces
            // smash
            // bomb
            // heart / clock
            // blank
// function to sleep and update the smash tiles
    // if clicked
        // smashed -> destroy active -> incrementScore -> spawnNewTile
        // missed time -> tileExpire -> loseHealth -> spawn New
        // wrongTile -> loseHealth
        // Time attack version -> heart = clock to gain more time
            // bomb takes away time
// sleep function on spawn with timeout value for expiration
// explode if timeoutValue reached
// if clicked, intercept that timeout & destroy the tile and spawn a new one, with a fresh countdown clock


// TODO: -- THIS SHOULD BE USING A SHARED GENERIC TILE GAME BOARD BUILDER, NEED TO IMPLEMENT IN SHARED!
// STYLED WHEN REFACTOR ABOVE DONE?
let goalRollRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                | Blocker -> // TO BECOME BOMB -2 Health 
                    Tile.child [] [
                        Box.box' [ Props [ Style [ Border "1px solid white"; Background "#000000"; Width 100; Height 100 ] ] ] []
                    ]
                | Goal ->
                    // 3HP
                    // EZ: (3 missed intervals = -1 HP) MD: (2 missed intervals = -1 HP) HD: () XD: (1 miss = -3HP)
                    Tile.child [] [
                        Box.box' [ Props [ Style [ Border "1px solid black"; Background "#FF2843"; Width 100; Height 100 ] ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ]
                    ]
                | _ -> 
                    Tile.child [] [
                        Box.box' [ Props [ Style [ Border "1px solid black"; Width 100; Height 100 ] ] ] []
                    ]
            ]
    ]
// TILE SORT CONTROLS HEADER
let tileSmashHeader =
    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
        Container.container [] [
            Columns.columns [ Columns.IsVCentered ] [
                Column.column [] [
                    a [ Href "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Shared/Shared.fs" ] [ 
                        h2 [] [ str "Shared Code" ]
                    ]
                ]
                Column.column [] [ h1 [] [ str "Tile Tap" ] ]
                Column.column [] [ 
                    a [ Href "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Portfolio/Games/TileTap/Index.fs" ] [ 
                        h2 [] [ str "Client Code" ]
                    ]
                ]
            ]
            p [] [ str "- Survival Mode:" ]
            p [] [ str "- Smash the tile before it's timer runs out." ]
            p [] [ str "- Tile timer reaches 0 takes away 1 HP." ]
            p [] [ str "- Smash a Heart to gain 1 HP." ]
            p [] [ str "- Smashing bombs takes away 2 HP."]
            // p [] [ str "- Time Attack: 30 or 60 seconds?: how many can you smash?" ]
        ]
    ]
// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileSmashBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [
        for row in board do
            goalRollRowCreator row dispatch
    ]
let view dispatch =
    Container.container [] [
        SharedModule.backToGallery QuitGame dispatch
        tileSmashHeader
        // match model.GameState with
        Container.container [ Container.Props [ ClassName "gameGridContainer" ] ] [
            tileSmashBoardView ( SharedTileTap.generateEmptyTileTapGrid SharedTileTap.gridDimension ) dispatch
        ]
    ]