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
    | LogInterval
    | QuitGame

// let simpleSecondTimer stringLog =
//     Async.Sleep 5000
//     printfn stringLog


let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none
let update msg model =
    match msg with
    | LogInterval -> 
        printfn "waiting 2 sec"
        // simpleSecondTimer "waited"
        model, Cmd.none
    | QuitGame ->
        model, Cmd.ofMsg QuitGame

// Game starts
    // Spawns object(s) - timer starts

        // Tap object before timer, kill that objects timer, increment score, spawn new tile
        // Tile times out - decrement score / time, spawn new tile

        // health or time runs out
            // end game, show user their score.

// function that acts as the countdown
    //


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

let tileTapDescriptions = [
    "Survival Mode:"
    "- Smash the tile before time runs out."
    "- Tile timer reaches 0 takes away 1 HP."
    "- Smash a Heart to gain 1 HP."
    "- Smashing bombs takes away 2 HP."
]
let sourceCodeLinks = [
    "Model", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Shared/Shared.fs"
    "View", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Shared/Index.fs"
    "Logic", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Portfolio/Games/TileTap/Index.fs"
]
let gameControls = [ "Quit Game", QuitGame; ]//"Timer Start", LogInterval ]

let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                // TO BECOME BOMB -2 Health 
                | Blocker -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid white"; Background "#000000"; Width 75; Height 75 ] ] ] [] ]
                | Goal -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid black"; Background "#FF2843"; Width 75; Height 75 ] ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ] ]
                | _ -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid black"; Width 75; Height 75 ] ] ] [] ]
                // 3HP // EZ: (3 missed intervals = -1 HP) MD: (2 missed intervals = -1 HP) HD: () XD: (1 miss = -3HP)
            ]
    ]
// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileTapBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [ for row in board do tileTapRowCreator row dispatch ]
 
let tileTapHeader dispatch =
    SharedViewModule.sharedModalHeaderControls "Tile Tap" QuitGame dispatch

let tileTapLeftModal =
    SharedViewModule.sharedModalLeft tileTapDescriptions sourceCodeLinks

let tileTapModalContent dispatch =
    tileTapBoardView ( SharedTileTap.generateEmptyTileTapGrid SharedTileTap.gridDimension ) dispatch

let tileTapModalRight dispatch =
    ( SharedViewModule.sharedModalRight gameControls dispatch )

let view model dispatch =
    SharedViewModule.sharedModal ( tileTapHeader dispatch ) ( tileTapLeftModal ) ( tileTapModalContent dispatch ) ( tileTapModalRight dispatch )