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
    | SetGameState of float // stores float pointer to dispatch setInterval loop
    | GameLoopTick // batches commands to check the lifetime and spawn new tiles
    | CheckGridboardTiles // command to go through and check for expired tiles
    | SpawnNewActiveTile // places a new tile on the board
    | DestroyTile of TapTile // removes the tile (in a way that the player intended / made an effort)
    | ExpireTile of TapTile // remove the tile (in a way that the player missed the time interval)
    | ResetRound
    | ExitGameLoop // stops the setInterval loop from running while not within this sub-module.
    | QuitGame // returns control to the parent, exiting to gallery

// NEED TO IMPLEMENT
    // health or time runs out
        // End Round ->
            // Survived -> Time Expired
            // Lost -> Health = 0
    // smaller game grid?, doesn't work well on 1080 (ALSO OTHER SECTIONS REVIEW)
    // LoseHealth Msg -> Fire off when tile expires -> hook into Lost
        // missed time -> tileExpire -> loseHealth -> modelUpdate + check lose conditions
    // Survival Mode -> how long can you last before a mistake is made?

// LifeCycle & LifeCycle Helper Functions

let stopGameLoop loopFloat =
    window.clearInterval(loopFloat)

// take gridSize?
let tileSpawnPosition activeTilePositionList =
    let emptyGrid = [ 0..63 ]
    let availablePositions = List.filter ( fun x -> not (List.contains x activeTilePositionList ) ) ( emptyGrid )
    // randomly select a  position from remaining available
    if not ( availablePositions.IsEmpty ) 
        then availablePositions.[SharedTileSort.randomIndex availablePositions.Length] 
        else ( SharedTileSort.randomIndex ( emptyGrid.Length - 1 ) )

let randTapTileValue seed =
    let randomVal = Random().Next(seed)
    match randomVal with
    | 1 -> Modest
    | 2 -> Major
    | _ -> Minor

let insertNewTapTile gridBoard position =
    let gridPositions = gridBoard.GridPositions
    { GridPositions = [
        for i in 0 .. ( gridPositions.Length - 1 ) do
            if i = position 
                then TapTile { TapPosition = position; LifeTime = 0; Value = randTapTileValue (3) } 
                else gridPositions.[i]
    ] }

let activeTilePositionsFromBoard gridBoard =
    gridBoard.GridPositions
    |> List.map ( fun x -> 
        match x with 
        | TapTile x -> x.TapPosition
        | _ -> 0
    ) |> List.filter (fun x -> x <> 0)

let tickActiveTiles gridBoard =
    { GridPositions =
        gridBoard.GridPositions
        |> List.map ( fun x -> 
            match x with 
            | TapTile x -> TapTile ( { x with LifeTime = x.LifeTime + 1 } )
            | _ -> Blank
        ) 
    }

let expiredTileFromGrid grid =
    let expiredTileOption = ( List.tryFind ( fun x -> 
        match x with 
            | TapTile x -> x.LifeTime > 15
            | _ -> false 
    ) grid.GridPositions )
    match expiredTileOption with 
        | Some ( TapTile x ) -> Some x
        | _ -> None

let gridWithoutTile grid tileToRemove = 
    { GridPositions = [ 
        for tile in grid.GridPositions do
            match tile with 
            | TapTile x -> if x = tileToRemove then Blank else tile
            | _ -> tile
    ] }

let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none

let update msg ( model : SharedTileTap.Model ) =
    match msg with
    
    // Game starts
    | SetGameState flt ->
        { model with GameState = flt }, Cmd.none
    
    // Spawns object(s) - timer starts
    | GameLoopTick ->
        if model.TilesMissed >= 10 then model, Cmd.ofMsg (ResetRound) // 10 Mistakes // THIS SHOULD BE MSG | EndRound
        elif model.GameClock > 120 then model, Cmd.ofMsg (ResetRound) // 30 seconds on clock // THIS SHOULD BE MSG | EndRound
        else { model with GameClock = model.GameClock + 1 }, Cmd.batch [ Cmd.ofMsg ( CheckGridboardTiles ); Cmd.ofMsg ( SpawnNewActiveTile ) ]
    
    | SpawnNewActiveTile ->
        // check if max amount of tiles is reached && no tile spawned in last 2 ticks
        if ( model.TilesSpawned < 10 ) && ( model.LastSpawnInterval >= 2 ) then 
            let activeTilePositionList = ( activeTilePositionsFromBoard model.TileTapGridBoard )
            // filter a list of all positions by what is already active
            let selectedPosition = tileSpawnPosition activeTilePositionList
            let gridBoardWithTile = insertNewTapTile model.TileTapGridBoard selectedPosition
            // model with new taptile in active tile list
            { model with TileTapGridBoard = gridBoardWithTile; LastSpawnInterval = 0; TilesSpawned = model.TilesSpawned + 1 }, Cmd.none
        else 
            { model with LastSpawnInterval = model.LastSpawnInterval + 1 }, Cmd.none
    
    // function to update the tiles lifetimes if they exist
    | CheckGridboardTiles ->
        let activeTilePositionList = ( activeTilePositionsFromBoard model.TileTapGridBoard )
        if not(activeTilePositionList.IsEmpty) then
            let tickedGrid = tickActiveTiles model.TileTapGridBoard
            let expiredTile = expiredTileFromGrid tickedGrid
            match expiredTile with
            | Some x ->
                { model with TileTapGridBoard = tickedGrid; TilesMissed = model.TilesMissed + 1 }, Cmd.ofMsg ( ExpireTile x )
            | None -> 
                { model with TileTapGridBoard = tickedGrid }, Cmd.none
        else model, Cmd.none
    
    // Tile times out - decrement HP, if difficulty permits
    | ExpireTile expiredTile ->
        let grid = gridWithoutTile model.TileTapGridBoard expiredTile
        { model with TileTapGridBoard = grid; TilesSpawned = model.TilesSpawned - 1; }, Cmd.none
    
    // Tap object before timer, kill that objects timer, increment score, spawn new tile
    | DestroyTile tappedTile ->
        let grid = gridWithoutTile model.TileTapGridBoard tappedTile
        { model with TileTapGridBoard = grid; TilesSpawned = model.TilesSpawned - 1; TilesSmashed = model.TilesSmashed + 1 }, Cmd.none
    
    | ResetRound -> // Not exactly what I need to do here
        if (model.GameState <> 0.0) then stopGameLoop(model.GameState)
        let initModel, com = init()
        initModel, com
    
    | ExitGameLoop -> // Stops the setInterval dispatch loop if the game is exited
        if (model.GameState <> 0.0) then stopGameLoop(model.GameState)
        model, Cmd.ofMsg QuitGame
    
    | QuitGame ->
        model, Cmd.none

// --------------------------------------


// *********************************

// Module Content & Helpers -
let tileClock tile =
    string (tile.LifeTime / 4)

let modelValueAsString strin value =
    strin + string value
// ----------------

// *********************************

// HEADER ---------
let tileTapHeader  dispatch =
    div [] [ SharedViewModule.sharedModalHeaderControls "Tile Tap" ExitGameLoop dispatch ]
// ----------------

// *********************************

// LEFT -----------
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

let tileTapLeftModal =
    SharedViewModule.sharedModalLeft tileTapDescriptions sourceCodeLinks
// ----------------

// ********************************

// CONTENT --------
let styleForTile tile =
    match tile.Value with
    | Minor -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background "#000000"; Width 75; Height 75 ]
    | Modest -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background "#555555"; Width 75; Height 75 ]
    | Major -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background "#ffffff"; Width 75; Height 75 ]

// What is a tile smash board
    // blank // taptile // bomb?? // heart?? // clock??
let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                // if clicked // smashed -> destroy active -> incrementScore -> spawnNewTile
                | TapTile x -> Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); styleForTile x ] ] [str (tileClock x) ] ]
                // wrongTile -> loseHealth
                | _ -> Tile.child [] [ Box.box' [ Props [ Style [ Border "1px solid white"; Background "#FF2843"; Width 75; Height 75 ] ] ] [] ]
            ]
    ]

// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileTapBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [ for row in board do tileTapRowCreator row dispatch ]

let tileTapModalContent  ( model : SharedTileTap.Model ) dispatch =
    tileTapBoardView model.TileTapGridBoard dispatch
// ----------------

// *********************************

// RIGHT ----------
// NOT BEING USED CURRENTLY
// let gameControls = [ "Timer Start", GameLoopTick; ]

// Controls are being made in more custom override fashion,
// as the needs for this submodule require a bit more of a workaround
// due to Async.RunAsyncronously: not available in Fable
// wanting to set the window interval to dispatch game tick functions
    // while still being able to set the GameLoopTick's pointer returned setInterval on the model
// Tried with subscriptions, but as implemented it would be firing off regardless of the current
    // model that was being used / viewed by the User.
        // launch a sub-program with the subscriptions being fired off when that module is launched?

// Currently hacked around the Elmish dispatch loop - not ideal

// Time drives main game state, as things happen in intervals contained within the main loop
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
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str "Start Round" ] ] 
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Restart Round" ] ]
    ]

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
// ----------------

// *********************************

// MODULE VIEW ----
let view model dispatch =
    SharedViewModule.sharedModal ( tileTapHeader dispatch ) ( tileTapLeftModal ) ( tileTapModalContent (model) (dispatch) ) ( tileTapModalRight (model) ( dispatch ) )
// ----------------