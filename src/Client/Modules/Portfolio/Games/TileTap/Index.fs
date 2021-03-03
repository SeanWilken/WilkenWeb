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
    | SetGameState of float
    | SpawnNewActiveTile
    | CheckGridboardTiles
    | DestroyTile of TapTile
    | ExpireTile of TapTile
    | ExitGameLoop
    | QuitGame

// tile spawn / destroy
let insertNewTapTile gridBoard position =
    let gridPositions = gridBoard.GridPositions
    { GridPositions = [
        for i in 0 .. ( gridPositions.Length - 1 ) do
            if i = position 
                then TapTile { TapPosition = position; LifeTime = 0; Value = SharedTileTap.randTapTileValue (3) } 
                else gridPositions.[i]
    ] }

let stopGameLoop loopFloat =
    window.clearInterval(loopFloat)

let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none
let update msg ( model : SharedTileTap.Model ) =
    match msg with
    | SetGameState flt ->
        { model with GameState = flt }, Cmd.none
    | GameLoopTick ->
        // this should be checking things like: 
            // if total # of allowable tiles expired surpassed
            // if 
        { model with GameClock = model.GameClock + 1 }, Cmd.batch [ Cmd.ofMsg ( CheckGridboardTiles ); Cmd.ofMsg ( SpawnNewActiveTile ) ]
    | SpawnNewActiveTile ->
        // check if max amount of tiles is reached && no tile spawned in last 2 ticks
        if ( model.TilesSpawned < 10 ) && ( model.LastSpawnInterval >= 2 ) then 
            let activeTilePositionList = ( SharedTileTap.activeTilePositionsFromBoard model.TileTapGridBoard )
            // filter a list of all positions by what is already active
            // let availablePositions = List.filter ( fun x -> List.contains activeTilePositionList ) [0..64]
            let emptyGrid = [ 0..63 ]
            let availablePositions = List.filter ( fun x -> not (List.contains x activeTilePositionList ) ) ( emptyGrid )
            // randomly select a  position from remaining available
            let selectedPosition = if not ( availablePositions.IsEmpty ) then availablePositions.[SharedTileSort.randomIndex availablePositions.Length] else ( SharedTileSort.randomIndex ( emptyGrid.Length - 1 ) )
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
                | TapTile x -> x.LifeTime > 15
                | _ -> false
            ) tickedGrid.GridPositions )
            let expiredTile = 
                if expiredTiles.Length > 0 then 
                    match expiredTiles.Head with
                    | TapTile x ->
                        Some x
                    | _ -> None
                else None
            { model with TileTapGridBoard = tickedGrid; TilesMissed = model.TilesMissed + expiredTiles.Length }, match expiredTile with | None -> Cmd.none; | Some x -> Cmd.ofMsg ( ExpireTile ( x ) )
        else
            model, Cmd.none
    | ExpireTile expiredTile ->
        let grid = { 
            GridPositions = [ 
            for tile in model.TileTapGridBoard.GridPositions do
                match tile with 
                | TapTile x -> if x = expiredTile then Blank else tile
                | _ -> tile
        ] }
        { model with TileTapGridBoard = grid; TilesSpawned = model.TilesSpawned - 1; }, Cmd.none
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
        { model with TileTapGridBoard = grid; TilesSpawned = model.TilesSpawned - 1; TilesSmashed = model.TilesSmashed + 1 }, Cmd.none
    | ExitGameLoop ->
        if (model.GameState <> 0.0) then stopGameLoop(model.GameState)
        model, Cmd.ofMsg QuitGame
    | QuitGame ->
        model, Cmd.ofMsg QuitGame

// Game starts
    // Spawns object(s) - timer starts
        // Tap object before timer, kill that objects timer, increment score, spawn new tile
        // Tile times out - decrement score / time, spawn new tile

        // health or time runs out
            // end game, show user their score.

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

let gameControls = [ "Timer Start", GameLoopTick; ]

let styleForTile tile =
    match tile.Value with
    | Minor -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background "#000000"; Width 75; Height 75 ]
    | Modest -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background "#555555"; Width 75; Height 75 ]
    | Major -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background "#ffffff"; Width 75; Height 75 ]

let tileClock tile =
    string (tile.LifeTime / 4)

let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                // TO BECOME BOMB -2 Health 
                | TapTile x -> Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); styleForTile x ] ] [str (tileClock x) ] ]
                | _ -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid white"; Background "#FF2843"; Width 75; Height 75 ] ] ] [] ]
                // 3HP // EZ: (3 missed intervals = -1 HP) MD: (2 missed intervals = -1 HP) HD: () XD: (1 miss = -3HP)
            ]
    ]
// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileTapBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [ for row in board do tileTapRowCreator row dispatch ]
 
let tileTapHeader  dispatch =
    div [] [ SharedViewModule.sharedModalHeaderControls "Tile Tap" ExitGameLoop dispatch ]

let tileTapLeftModal =
    SharedViewModule.sharedModalLeft tileTapDescriptions sourceCodeLinks

let tileTapModalContent  ( model : SharedTileTap.Model ) dispatch =
    tileTapBoardView model.TileTapGridBoard dispatch

// HACKED AROUND UPDATE, TO DISPATCH THE GAMELOOP TICK + SET INTERVAL FOR RETURNED FLOAT
let startGameLoop ( model : SharedTileTap.Model ) dispatch =
    if model.GameState = 0.0
        then
            let loopFloat = window.setInterval((fun _ -> dispatch (GameLoopTick)), 250)
            dispatch (SetGameState loopFloat)
        else
            window.clearInterval(model.GameState)
            dispatch ( SetGameState 0.0 )

// custom non-shared right modal, needs to handle functions outside update loop to dispatch messages
let tileTapGameLoopToggle model dispatch =
    Container.container [] [
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str "Start Game" ] ] ]

let modelValueAsString strin value =
    strin + string value

let tileTapModalRight model dispatch =
    Tile.parent [ Tile.Size Tile.Is3 ] [ 
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ ClassName "modalLeft" ] [
                        Container.container [] [
                            tileTapGameLoopToggle model dispatch
                        ]
                        Container.container [] [
                            div [] [ str ( modelValueAsString "Timer: " ( model.GameClock / 4 ) ) ]
                            div [] [ str ( modelValueAsString "Spawned: " model.TilesSpawned ) ]
                            div [] [ str ( modelValueAsString "Smashed: " model.TilesSmashed ) ]
                            div [] [ str ( modelValueAsString "Expired: " model.TilesMissed ) ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let view model dispatch =
    SharedViewModule.sharedModal ( tileTapHeader dispatch ) ( tileTapLeftModal ) ( tileTapModalContent (model) (dispatch) ) ( tileTapModalRight (model) ( dispatch ) )