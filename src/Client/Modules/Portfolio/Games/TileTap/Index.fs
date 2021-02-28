module TileTap

open System
open FSharp
open Shared
open GridGame
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open System.Threading
open Browser

type Msg =
    | GameLoopTick
    | SpawnNewActiveTile
    | CheckGridboardTiles
    | ToggleGameState
    | DestroyTile of TapTile
    | QuitGame







    // setinterval returns a float, that I need to capture in order to call clear interval,
    // otherwise runs away once started

    // should be done through subscription. How to call subscription from submodule, rather than from program level?
        // is this even possible

// come back to this
// trigger this from parent?
let subscribe initial =
    let sub dispatch =
        window.setInterval((fun _ -> dispatch (GameLoopTick)), 1000) |> ignore
        // window.setTimeout((fun _ -> dispatch (GameLoopTick)), 5000) |> ignore
    Cmd.ofSub sub

// NEED LOOP TO DICTATE AND INCREMENT THE PORTIONS OF DATA THAT ARE TIME DEPENDANT ^^

// tile spawn / destroy
let insertNewTapTile gridBoard position =
    let gridPositions = gridBoard.GridPositions
    { GridPositions = [
        for i in 0 .. ( gridPositions.Length - 1 ) do
            if i = position 
                then TapTile { TapPosition = position; LifeTime = 0; Value = SharedTileTap.randTapTileValue } 
                else gridPositions.[i]
    ] }
let removeExpiredTiles gridBoard activeTilePositions =
    { GridPositions = [
        for position in activeTilePositions do
            let currentActiveTile = gridBoard.GridPositions.[position]
            match currentActiveTile with
            | TapTile x ->
                if x.LifeTime >= 5 
                    then Blank
                    else TapTile x
            | _ -> Blank
    ] }

// runs off with interval
// THIS WORKS BUT IS TRASH!!
// NEED TO USE SUBSCRIBE
let shittyWrapper ( model : SharedTileTap.Model ) dispatch =
    if model.GameState = Playing
        then
            printfn "LOOPING"
            window.setInterval((fun _ -> dispatch (GameLoopTick)), 500) |> ignore
            // window.setTimeout((fun _ -> dispatch (GameLoopTick)), 500) |> ignore
    else 
            printfn "UNLOOPING"
            window.setInterval((), 0) |> ignore
            // window.clearInterval()


let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none
let update msg ( model : SharedTileTap.Model ) =
    match msg with
    | GameLoopTick ->
        printfn "hello"
        { model with GameState = Playing}, Cmd.batch [ Cmd.ofMsg ( CheckGridboardTiles ); Cmd.ofMsg ( SpawnNewActiveTile ) ]
    | ToggleGameState ->
        let toggledGameState = 
            if model.GameState = Playing then { model with GameState = Paused }
            else {model with GameState = Playing}
        toggledGameState, Cmd.none
    | SpawnNewActiveTile ->
        // check if max amount of tiles is reached && no tile spawned in last 2 ticks
        if ( model.TilesSpawned < 5 ) && ( model.LastSpawnInterval >= 1 ) then 
            let activeTilePositionList = ( SharedTileTap.activeTilePositionsFromBoard model.TileTapGridBoard )
            // filter a list of all positions by what is already active
            // let availablePositions = List.filter ( fun x -> List.contains activeTilePositionList ) [0..64]
            let emptyGrid = [ 0..63 ]
            let availablePositions = List.filter ( fun x -> not (List.contains x activeTilePositionList ) ) ( emptyGrid )
            // randomly select a  position from remaining available
            let selectedPosition = if not(availablePositions.IsEmpty) then availablePositions.[SharedTileSort.randomIndex availablePositions.Length] else ( SharedTileSort.randomIndex ( emptyGrid.Length - 1 ) )
            let gridBoardWithTile = insertNewTapTile model.TileTapGridBoard selectedPosition
            // model with new taptile in active tile list
            { model with TileTapGridBoard = gridBoardWithTile; LastSpawnInterval = 0; TilesSpawned = model.TilesSpawned + 1 }, Cmd.none
        else 
            { model with LastSpawnInterval = model.LastSpawnInterval + 1 }, Cmd.none
    | CheckGridboardTiles ->
        let activeTilePositionList = ( SharedTileTap.activeTilePositionsFromBoard model.TileTapGridBoard )
        if not(activeTilePositionList.IsEmpty) then
            let tickedGrid = SharedTileTap.tickActiveTiles model.TileTapGridBoard
            let expiredTiles = ( List.filter ( fun x -> 
                match x with
                | TapTile x -> x.LifeTime < 5 
                | _ -> false
            ) tickedGrid.GridPositions )
            { model with TileTapGridBoard = tickedGrid; TilesMissed = model.TilesMissed + expiredTiles.Length }, Cmd.none
        else
            model, Cmd.none 
    | DestroyTile tappedTile ->
        let grid = { 
            GridPositions = [ 
            for tile in model.TileTapGridBoard.GridPositions do
                match tile with 
                | TapTile x ->
                    if x = tappedTile 
                        then Blank
                        else tile
                | _ -> tile
        ] }
        // check if tilePosition is contained within the model, filter the list of that and return it.
        // will return the same if it doesn't exist.
        { model with TileTapGridBoard = grid; TilesSpawned = model.TilesSpawned - 1 }, Cmd.none
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
let gameControls = [ "Quit Game", QuitGame; "Timer Start", GameLoopTick; "Toggle Game State", ToggleGameState] //


// distusting hack
let shittyButton model dispatch =
    div [ OnClick ( fun _ -> shittyWrapper model dispatch |> ignore ); Style [BackgroundColor "#FF2843"; Height 100; Width 100] ] []

let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                // TO BECOME BOMB -2 Health 
                | TapTile x -> Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); Style [ Border "1px solid white"; Background "#000000"; Width 75; Height 75 ] ] ] [] ]
                | Blocker -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid white"; Background "#000000"; Width 75; Height 75 ] ] ] [] ]
                | Goal -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid black"; Background "#FF2843"; Width 75; Height 75 ] ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ] ]
                | _ -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid white"; Background "#FF2843"; Width 75; Height 75 ] ] ] [] ]// Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid black"; Width 75; Height 75 ] ] ] [] ]
                // 3HP // EZ: (3 missed intervals = -1 HP) MD: (2 missed intervals = -1 HP) HD: () XD: (1 miss = -3HP)
            ]
    ]
// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileTapBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [ for row in board do tileTapRowCreator row dispatch ]
 
let tileTapHeader model dispatch =
    div [] [
        shittyButton model dispatch
        SharedViewModule.sharedModalHeaderControls "Tile Tap" QuitGame dispatch
    ]

let tileTapLeftModal =
    SharedViewModule.sharedModalLeft tileTapDescriptions sourceCodeLinks

let tileTapModalContent  ( model : SharedTileTap.Model ) dispatch =
    tileTapBoardView model.TileTapGridBoard dispatch

let tileTapModalRight dispatch =
    ( SharedViewModule.sharedModalRight gameControls dispatch )

let view model dispatch =
    SharedViewModule.sharedModal ( tileTapHeader model dispatch ) ( tileTapLeftModal ) ( tileTapModalContent (model) (dispatch) ) ( tileTapModalRight dispatch )