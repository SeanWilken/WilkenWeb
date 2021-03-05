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
    // GAME LOOP
    | ChangeDifficulty of SharedTileTap.TileTapDifficulty
    | SetGameState of float // stores float pointer to dispatch setInterval loop
    | GameLoopTick // batches commands to check the lifetime and spawn new tiles
    // GRID TILES
    | CheckGridboardTiles // command to go through and check for expired tiles
    | SpawnNewActiveTile // places a new tile on the board
    | DestroyTile of TapTile // removes the tile (in a way that the player intended / made an effort)
    | ExpireTile of TapTile // remove the tile (in a way that the player missed the time interval)
    | Mistake
    // ROUND STATE
    | EndRound // FURTHER DEFINE THE REASON FOR ROUND ENDING
    | ResetRound
    | ExitGameLoop // stops the setInterval loop from running while not within this sub-module.
    | QuitGame // returns control to the parent, exiting to gallery

// NEED TO IMPLEMENT
    // End Round ->
        // Survived -> Time Expired
        // Lost -> Health = 0
    // smaller game grid?, doesn't work well on 1080 (ALSO OTHER SECTIONS REVIEW)
    // win and lose conditions and GUI

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
    ( List.tryFind ( fun x -> 
        match x with 
            | TapTile x -> x.LifeTime > 15
            | _ -> false 
    ) grid.GridPositions )
    |> fun expiredTileOption -> 
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

// just use int as the value, rather than wrap in type?
let calculateTilePointValue tile =
    match tile.Value with 
    | Minor -> 1 * tile.LifeTime
    | Modest -> 2 * tile.LifeTime
    | Major -> 5 * tile.LifeTime

let updateModelDifficulty  ( model : SharedTileTap.Model ) ( difficulty : SharedTileTap.TileTapDifficulty ) =
    match difficulty with
    | SharedTileTap.TileTapDifficulty.Simple -> { model with RoundTimer = 30; AllowableRoundMistakes = -1 }
    | SharedTileTap.TileTapDifficulty.Easy -> { model with RoundTimer = 30; AllowableRoundMistakes = 5 }
    | SharedTileTap.TileTapDifficulty.Intermediate -> { model with RoundTimer = 60; AllowableRoundMistakes = 3 }
    | SharedTileTap.TileTapDifficulty.Survival -> { model with RoundTimer = -1; AllowableRoundMistakes = 1 }

let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none

let update msg ( model : SharedTileTap.Model ) =
    match msg with
    | ChangeDifficulty difficulty ->
        updateModelDifficulty model difficulty, Cmd.ofMsg ResetRound   
    // Game starts
    | SetGameState flt ->
        { model with GameState = flt }, Cmd.none
    // Spawns object(s) - timer starts
    | GameLoopTick -> // this should check against Round 'Allowances'
        // If more than mistakes are made than the difficulty on the model allows
        if ( model.AllowableRoundMistakes > 0 ) && ( model.RoundMistakes >= model.AllowableRoundMistakes ) then model, Cmd.ofMsg EndRound
        // End the round when the GameClock exceeds the RoundTimer.  ( -1 : no round timer)
        elif ( model.RoundTimer > 0 ) && ( ( model.GameClock / 4 ) >= model.RoundTimer ) then model, Cmd.ofMsg EndRound
        // No conditions to end round met, batch check grid and spawn tile commands
        else { model with GameClock = model.GameClock + 1 }, Cmd.batch [ Cmd.ofMsg ( CheckGridboardTiles ); Cmd.ofMsg ( SpawnNewActiveTile ) ]
    | SpawnNewActiveTile ->
        // check if max amount of tiles is reached && no tile spawned in last 2 ticks (.5 second)
        if ( model.TilesSpawned < 10 ) && ( model.LastSpawnInterval >= 2 ) then
            activeTilePositionsFromBoard model.TileTapGridBoard
            |> fun activeTilePositionList -> tileSpawnPosition activeTilePositionList // filter a list of all positions by what is already active
            |> fun selectedPosition -> insertNewTapTile model.TileTapGridBoard selectedPosition
            // model with new taptile in active tile list
            |> fun gridBoardWithTile -> { model with TileTapGridBoard = gridBoardWithTile; LastSpawnInterval = 0; TilesSpawned = model.TilesSpawned + 1 }, Cmd.none
        else 
            { model with LastSpawnInterval = model.LastSpawnInterval + 1 }, Cmd.none
    // function to update the tiles lifetimes if they exist
    | CheckGridboardTiles ->
        let activeTilePositionList = ( activeTilePositionsFromBoard model.TileTapGridBoard )
        if not(activeTilePositionList.IsEmpty) then
            let tickedGrid = tickActiveTiles model.TileTapGridBoard
            expiredTileFromGrid tickedGrid
            |> fun expiredTile ->
                match expiredTile with
                | Some x -> Cmd.batch [ Cmd.ofMsg Mistake; Cmd.ofMsg ( ExpireTile x ) ]
                | None -> Cmd.none
            |> fun comms -> { model with TileTapGridBoard = tickedGrid }, comms
        else model, Cmd.none
    // Tile times out - decrement HP, if difficulty permits
    | ExpireTile expiredTile ->
        let grid = gridWithoutTile model.TileTapGridBoard expiredTile
        { model with TileTapGridBoard = grid; TilesSpawned = model.TilesSpawned - 1; }, Cmd.none
    // Tap object before timer, kill that objects timer, increment score, spawn new tile
    | DestroyTile tappedTile ->
        gridWithoutTile model.TileTapGridBoard tappedTile
        |> fun grid -> 
            { model with 
                TileTapGridBoard = grid
                TilesSpawned = model.TilesSpawned - 1
                TilesSmashed = model.TilesSmashed + 1
                RoundScore = model.RoundScore + (calculateTilePointValue tappedTile)
        }, Cmd.none
    | Mistake ->
        if (model.GameState <> 0.0) 
            then 
                let totalRoundMistakes = model.RoundMistakes + 1
                if ( model.AllowableRoundMistakes > 0 ) && ( totalRoundMistakes >= model.AllowableRoundMistakes ) then { model with RoundMistakes = totalRoundMistakes }, Cmd.ofMsg EndRound
                else { model with RoundMistakes = totalRoundMistakes }, Cmd.none
            else model, Cmd.none
    // EndRound of EndResult
    // EndResult -> 
        // Win -> lasted until gameloop execution ended. Award bonus.
        // Lose -> Round ended due to mistakes or player errors.
        // RoundState -> idle, playing, ended
    | EndRound -> 
        if (model.GameState <> 0.0) then stopGameLoop(model.GameState)
        // MODEL SHOULD HAVE EMPTY GRID, NO MORE POINTS CAN BE AWARDED, TILES SHOULDN'T BE DESTROYABLE
        model, Cmd.ofMsg (SetGameState 0.0)
    | ResetRound ->
        if (model.GameState <> 0.0) then stopGameLoop(model.GameState)
        SharedTileTap.resetRound model, Cmd.none
    | ExitGameLoop -> // Stops the setInterval dispatch loop when the game is exited
        if (model.GameState <> 0.0) then stopGameLoop(model.GameState)
        model, Cmd.ofMsg QuitGame
    | QuitGame -> // intercepted by parent, returns flow to
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
    | Minor -> "#000000"
    | Modest -> "#555555"
    | Major -> "#ffffff"
    |> fun tileColor -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background tileColor; Width 75; Height 75 ]

// What is a tile smash board
    // blank // taptile // bomb?? // heart?? // clock??
let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                | TapTile x -> Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); styleForTile x ] ] [str (tileClock x) ] ]
                | _ -> Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> Mistake |> dispatch); Style [ Border "1px solid white"; Background "#FF2843"; Width 75; Height 75 ] ] ] [] ]
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
            window.setInterval((fun _ -> dispatch (GameLoopTick)), 250)
            |> fun loopFloat -> dispatch (SetGameState loopFloat)
        else
            window.clearInterval(model.GameState)
            dispatch ( SetGameState 0.0 )

// custom non-shared right modal, needs to handle functions outside update loop to dispatch messages
let tileTapGameLoopToggle model dispatch =
    Container.container [] [
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str "Start Round" ] ] 
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Restart Round" ] ]
        div [ ClassName "mainContainer" ] [ 
            span [] [ 
                h3 [] [ str "Round Difficulty: "] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Simple ) ) |> dispatch ) ] [ str "Simple" ] ] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Easy ) ) |> dispatch ) ] [ str "Easy" ] ] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Intermediate ) ) |> dispatch ) ] [ str "Medium" ] ] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Survival ) ) |> dispatch ) ] [ str "Survival" ] ] 
            ]
        ]
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
                            h2 [] [ str "Round Stats: "] 
                            div [] [ str ( modelValueAsString "Round Score: " model.RoundScore ) ]
                            div [] [ str ( modelValueAsString "Round Timer: " ( model.GameClock / 4 ) + modelValueAsString " / " model.RoundTimer) ]
                            div [] [ str ( modelValueAsString "Round Mistakes: " model.RoundMistakes + modelValueAsString " / " model.AllowableRoundMistakes) ]
                            h2 [] [ str "Details: "] 
                            // div [] [ str ( modelValueAsString "Total Tiles Spawned: " model.TilesSpawned ) ]
                            div [] [ str ( modelValueAsString "Total Tiles Smashed: " model.TilesSmashed ) ]
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