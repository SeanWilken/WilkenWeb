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
    | ChangeGameMode of SharedTileTap.TileTapGameMode
    | ChangeDifficulty of SharedTileTap.TileTapDifficulty
    | SetDispatchPointer of float // stores float pointer to dispatch setInterval loop
    | GameLoopTick // batches commands to check the lifetime and spawn new tiles
    // GRID TILES
    | CheckGridboardTiles // command to go through and check for expired tiles
    | SpawnNewActiveTile // places a new tile on the board
    | DestroyTile of TapTile // removes the tile (in a way that the player intended / made an effort)
    | ExpireTile of TapTile // remove the tile (in a way that the player missed the time interval)
    | Mistake of int
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

// clears the interval function dispatching GameLoopTick
// via stored float pointer on the Model
let stopGameLoop loopFloat =
    window.clearInterval(loopFloat)

// checks the list of current active tiles
// against a list of all positions to ensure
// that the same index can't be called to insert a new tile.
let tileSpawnPosition activeTilePositionList =
    // all positions (take a new parameter for gridSizeCeiling?)
    let allGridPositions = [ 0..63 ] 
    // filter the activeTilePositions from the totalPositions
    List.filter ( fun x -> not (List.contains x activeTilePositionList ) ) ( allGridPositions )
    // if there are available positions, select one randomly
    |> fun availablePositions -> 
        if not ( availablePositions.IsEmpty ) 
            then availablePositions.[SharedTileSort.randomIndex availablePositions.Length] 
            else ( SharedTileSort.randomIndex ( allGridPositions.Length - 1 ) )

// Given a max ceiling int value,
let randomValue ceiling =
    Random().Next(ceiling)
// return a mapped out TileValue
let alternateRandValue randVal =
    if randVal < 10 then TapTileValue.Bomb
    elif randVal >= 10 && randVal < 45 then TapTileValue.Minor
    elif randVal >= 45 && randVal < 70 then TapTileValue.Modest
    elif randVal >= 70 && randVal < 90 then TapTileValue.Major
    else TapTileValue.Heart

// Given a Gridboard and desired position,
// return that Gridboard with a tile @ desired position
// What about Clock, Heart or Bomb???
let insertNewTapTile gridBoard position =
    let gridPositions = gridBoard.GridPositions
    { GridPositions = [
        for i in 0 .. ( gridPositions.Length - 1 ) do
            if i = position 
                then TapTile { TapPosition = position; LifeTime = 0; Value = alternateRandValue ( randomValue (100) ) } 
                else gridPositions.[i]
    ] }

// Given a Gridboard, return a list of it's tile positions
let activeTilePositionsFromBoard gridBoard =
    gridBoard.GridPositions
    |> List.map ( fun x -> 
        match x with 
        | TapTile x -> x.TapPosition
        | _ -> 0
    ) |> List.filter (fun x -> x <> 0)

// Increment the LifeTime value on every tile on the Gridboard
let tickActiveTiles gridBoard =
    { GridPositions =
        gridBoard.GridPositions
        |> List.map ( fun x -> 
            match x with 
            | TapTile x -> TapTile ( { x with LifeTime = x.LifeTime + 1 } )
            | _ -> Blank
        ) 
    }

// Returns the first ( Some TapTile ) 
// if found on the given Gridboard.
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

// Given a Gridboard and a TapTile
// swap the gridPosition to a blank
// if it matches the given TapTile.
let gridWithoutTile grid tileToRemove = 
    { GridPositions = [ 
        for tile in grid.GridPositions do
            match tile with 
            | TapTile x -> if x = tileToRemove then Blank else tile
            | _ -> tile
    ] }

// Given a tile, based on it's Value
// returns a score value to increment the score
let calculateTilePointValue tile =
    match tile.Value with 
    | Minor -> 1 * tile.LifeTime
    | Modest -> 2 * tile.LifeTime
    | Major -> 5 * tile.LifeTime
    | _ -> 0


let gameModeRoundMistake ( model : SharedTileTap.Model ) ( mistakeValue : int ) : ( SharedTileTap.Model * Cmd<Msg> )=
    if model.GameMode = SharedTileTap.TileTapGameMode.Survival
        then 
            let totalRoundMistakes = if model.RoundMistakes + mistakeValue < 0 then 0 else model.RoundMistakes + mistakeValue
            let com = 
                if ( model.AllowableRoundMistakes > 0 ) && ( totalRoundMistakes >= model.AllowableRoundMistakes ) 
                    then Cmd.ofMsg EndRound
                    else Cmd.none
            { model with RoundMistakes = totalRoundMistakes }, com
        else
            let currentTimeExpired = model.GameClock + ( mistakeValue * 4 )
            let com = 
                if ( model.RoundTimer * 4 <= currentTimeExpired ) then Cmd.ofMsg EndRound else Cmd.none
            { model with GameClock = currentTimeExpired; RoundMistakes = ( model.RoundMistakes + 1 ) }, com

// Initialize the modules model
// No command dispatched currently
let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none

// Handles the various different messages that
// can be dispatched throughout the use and
// interaction of this module
let update msg ( model : SharedTileTap.Model ) =
    match msg with
    // Change Round Parameters based on requested difficulty
    | ChangeGameMode gameMode ->
        SharedTileTap.updateModelGameMode model gameMode, Cmd.ofMsg ResetRound
    | ChangeDifficulty difficulty ->
        if model.GameMode = SharedTileTap.TileTapGameMode.Survival 
            then SharedTileTap.updateSurvivalModeDifficulty model difficulty, Cmd.ofMsg ResetRound
            else SharedTileTap.updateTimeAttackModeDifficulty model difficulty, Cmd.ofMsg ResetRound
    // Game starts
    | SetDispatchPointer flt ->
        if flt <> 0.0 then Playing else Paused
        |> fun roundState -> { model with RoundState = roundState; DispatchPointer = flt }, Cmd.none
    // 'Tick' Interval in which the module operates.
    // this is dispatched through the browser windows setInterval function
    // Checks against Round 'Allowances' to see if the round should be stopped.
    // If round is still in play, then it will dispatch 
        // to check for expired tiles
        // spawn a new tile if the spawn interval is matched or exceeded
    | GameLoopTick -> // this should check against Round 'Allowances'
        // If more than mistakes are made than the difficulty on the model allows
        if ( model.AllowableRoundMistakes > 0 ) && ( model.RoundMistakes >= model.AllowableRoundMistakes ) then model, Cmd.ofMsg EndRound
        // End the round when the GameClock exceeds the RoundTimer.  ( -1 : no round timer)
        elif ( model.RoundTimer > 0 ) && ( ( model.GameClock / 4 ) >= model.RoundTimer ) then model, Cmd.ofMsg EndRound
        // No conditions to end round met, batch check grid and spawn tile commands
        else { model with GameClock = model.GameClock + 1 }, Cmd.batch [ Cmd.ofMsg ( CheckGridboardTiles ); Cmd.ofMsg ( SpawnNewActiveTile ) ]
    // If there hasn't been a new tile placed onto the Gridboard
    // but the interval is matched or exceeded, generate the Gridboard with a new tile added
    // otherwise increment the LastSpawnInterval by one
    | SpawnNewActiveTile ->
        // check if max amount of tiles is reached && no tile spawned in last 2 ticks (.5 second)
        if ( model.LastSpawnInterval >= 2 ) then //( model.TilesSpawned < 10 ) && 
            // filter a list of all positions by what is already active
            activeTilePositionsFromBoard model.TileTapGridBoard
            // select a position within the Gridboard to place a new tile
            |> fun activeTilePositionList -> tileSpawnPosition activeTilePositionList 
            // return the Gridboard with the selected position filled by a new tile
            |> fun selectedPosition -> insertNewTapTile model.TileTapGridBoard selectedPosition
            // model with new taptile in active tile list
            |> fun gridBoardWithTile -> { model with TileTapGridBoard = gridBoardWithTile; LastSpawnInterval = 0; TilesSpawned = model.TilesSpawned + 1 }, Cmd.none
        else
            // increment the spawn interval, as one was not spawned this pass.
            { model with LastSpawnInterval = model.LastSpawnInterval + 1 }, Cmd.none
    // Will check the Gridboard for expired tiles after
    // incrementing all their respective lifetime values
    // Dispatch Mistake & ExpireTile if one exists
    | CheckGridboardTiles ->
        let activeTilePositionList = ( activeTilePositionsFromBoard model.TileTapGridBoard )
        if not(activeTilePositionList.IsEmpty) then
            let tickedGrid = tickActiveTiles model.TileTapGridBoard
            expiredTileFromGrid tickedGrid
            |> fun expiredTile ->
                match expiredTile with
                | Some x -> 
                    match x.Value with 
                    | TapTileValue.Heart                        
                    | TapTileValue.Bomb ->
                        Cmd.ofMsg ( ExpireTile x )
                    | _ ->
                        Cmd.batch [ Cmd.ofMsg ( Mistake (1) ); Cmd.ofMsg ( ExpireTile x ) ]
                | None -> Cmd.none
            |> fun comms -> { model with TileTapGridBoard = tickedGrid }, comms
        else model, Cmd.none
    // Tile times out - remove that tile from the board
    | ExpireTile expiredTile ->
        let grid = gridWithoutTile model.TileTapGridBoard expiredTile
        { model with TileTapGridBoard = grid }, Cmd.none
    // Player tapped the tile before LifeTime was exceeded,
    // removes the tappedTile from the Gridboard and increments the RoundScore
    // based on the tappedTile's Value & LifeTime
    | DestroyTile tappedTile ->
        // need to do some stuff based on game mode
        gridWithoutTile model.TileTapGridBoard tappedTile
        |> fun grid -> 
            { model with 
                TileTapGridBoard = grid
                TilesSmashed = model.TilesSmashed + 1
                RoundScore = model.RoundScore + ( calculateTilePointValue tappedTile )
        }, match tappedTile.Value with
            | TapTileValue.Heart -> Cmd.ofMsg ( Mistake (-1) )
            | TapTileValue.Bomb -> Cmd.ofMsg ( Mistake (3) )
            | _ -> Cmd.none
    // User didn't tap the tile in time or 
    // selected a harmful or blank tile
    | Mistake mistakeValue ->
        // needs to be based on selected mode: Survival || Time Attack
        // negative value mistakes are helpful actually as they reduce total mistakes made or increase the clock
        // some might say, a "happy little mistake" :^)
        if (model.DispatchPointer <> 0.0)
            then gameModeRoundMistake model mistakeValue
            else model, Cmd.none
    // Round has finished, cleanup the intervalTimer
    | EndRound -> 
        if (model.DispatchPointer <> 0.0) then stopGameLoop(model.DispatchPointer)
        SharedTileTap.endRound model, Cmd.none
    // Resets the values for a round aside 
    // from it's difficulty parameters
    | ResetRound ->
        if (model.DispatchPointer <> 0.0) then stopGameLoop(model.DispatchPointer)
        SharedTileTap.resetRound model, Cmd.none
    // Stops the setInterval dispatch loop when the game is exited
    | ExitGameLoop -> 
        if (model.DispatchPointer <> 0.0) then stopGameLoop(model.DispatchPointer)
        model, Cmd.ofMsg QuitGame
    // Msg that will be intercepted by CodeGallery
    // where User will be returned to
    | QuitGame -> 
        model, Cmd.none

// --------------------------------------


// *********************************

// Module Content & Helpers -
// divide
let gameTickClock ticks =
    string (ticks / 4)

let modelValueAsString strin value =
    if value = -1 
        then strin + "\u221E";
        else strin + string value
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


// ** IMPORTANT --
// NEED TO MAKE SURE IF CARD VIEW IS USED (CAN CLICK ON HEADER)
    // Ignore Message, as if App gets a ChangeSection message, 
    // it wouldn't stop the game dispatch loop interval

    // Ensure that message will always find it's way down to clear the interval

// CONTENT --------
let styleForTile tile =
    match tile.Value with
    | TapTileValue.Bomb -> "#FF2843"
    | TapTileValue.Heart -> "#000000"
    | Minor -> "#000000"
    | Modest -> "#555555"
    | Major -> "#ffffff"
    |> fun tileColor -> Style [ FontFamily "Ubuntu"; FontSize 25; TextAlign TextAlignOptions.Center; Color "#FF2483"; Border "1px solid white"; Background tileColor; Width 75; Height 75 ]

let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                | TapTile x -> 
                    match x.Value with 
                    | TapTileValue.Bomb ->
                        Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); styleForTile x ] ] [ Image.image [] [ img [ Src "./imgs/icons/Bomb.png"] ] ] ]
                    | TapTileValue.Heart ->
                        Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); styleForTile x ] ] [ Image.image [] [ img [ Src "./imgs/icons/Heart.png"] ] ] ]
                    | TapTileValue.Minor
                    | TapTileValue.Modest
                    | TapTileValue.Major ->
                        Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> DestroyTile ( x ) |> dispatch); styleForTile x ] ] [ str ( gameTickClock x.LifeTime ) ] ]
                | _ -> Tile.child [] [ Box.box' [ Props [ OnClick (fun _ -> Mistake (1) |> dispatch); Style [ Border "1px solid white"; Background "#FF2843"; Width 75; Height 75 ] ] ] [] ]
            ]
    ]

// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileTapBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [ for row in board do tileTapRowCreator row dispatch ]
  
let roundOverString ( model : SharedTileTap.Model ) =
    Container.container [ Container.Props [ Style [ FontSize 30; Color "#FFFFFF" ] ] ] [ // 69A69A
        h1 [ Style [ Padding 10; Color "#69A69A" ] ] [ str ( "The round lasted " + ( gameTickClock ( model.GameClock ) ) + " second(s)," ) ]
        h1 [ Style [ Padding 10; Color "#69A69A" ] ] [ str ( "You smashed " + string ( model.TilesSmashed ) + " tiles and made " + string ( model.RoundMistakes ) + " mistakes," ) ]
        h1 [ Style [ Padding 10; Color "#69A69A" ] ] [ str ( " This results in a final score of:" ) ]
        h1 [ Style [ Padding 20; Color "#00ff00"; FontSize 50 ] ] [ str ( string ( model.RoundScore ) ) ]
    ]

let tileTapModalContent  ( model : SharedTileTap.Model ) dispatch =
    match model.RoundState with
    | Won -> // WIP
        div [ ClassName "levelCompletedCard" ] [ 
            Container.container [ Container.Props [ Style [ Padding 20 ] ] ] [
                str "ROUND OVER!"
            ]
            Container.container [ Container.Props [ Style [ FontSize 20; Padding 20] ] ] [
                h2 [ Style [ FontSize 50; Color "#FF2843" ] ] [ str "Round Stats: "] 
                div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( modelValueAsString "Round Score: " model.RoundScore ) ]
                div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( "Round Timer: " + ( gameTickClock model.GameClock ) + modelValueAsString " / " model.RoundTimer) ]
                div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( modelValueAsString "Round Mistakes: " model.RoundMistakes + modelValueAsString " / " model.AllowableRoundMistakes) ]
            ]
            Container.container [ Container.Props [ Style [ FontSize 20; Padding 20 ] ] ] [
                h2 [ Style [ FontSize 50; Color "#FF2843" ] ] [ str "Details: "]
                roundOverString model
            ]
        ]
    | _ -> 
        tileTapBoardView model.TileTapGridBoard dispatch
    (*
    *)
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
    if model.DispatchPointer = 0.0
        then
            window.setInterval((fun _ -> dispatch (GameLoopTick)), 250)
            |> fun loopFloat -> dispatch (SetDispatchPointer loopFloat)
        else
            window.clearInterval(model.DispatchPointer)
            dispatch ( SetDispatchPointer 0.0 )

// custom non-shared right modal, needs to handle functions outside update loop to dispatch messages
let tileTapGameLoopToggle ( model : SharedTileTap.Model ) dispatch =
    let roundStateToggleString = 
        if ( model.DispatchPointer <> 0.0 ) then "Pause" else "Start"
    Container.container [] [
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str ( roundStateToggleString + " Round" ) ] ] 
        div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Restart Round" ] ]
        div [ ClassName "mainContainer" ] [ 
            span [] [ 
                h3 [] [ str "Game Mode: "] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeGameMode SharedTileTap.TileTapGameMode.Survival |> dispatch ) ) ] [ str "Survival" ] ]
                div [] [ a [ OnClick ( fun _ -> ( ChangeGameMode SharedTileTap.TileTapGameMode.TimeAttack |> dispatch ) ) ] [ str "Time Attack" ] ] 
            ]
        ]
        div [ ClassName "mainContainer" ] [ 
            span [] [ 
                h3 [] [ str "Round Difficulty: "] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Simple ) ) |> dispatch ) ] [ str "Simple" ] ] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Easy ) ) |> dispatch ) ] [ str "Easy" ] ] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Intermediate ) ) |> dispatch ) ] [ str "Medium" ] ] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeDifficulty ( SharedTileTap.TileTapDifficulty.Hard ) ) |> dispatch ) ] [ str "Hard" ] ] 
            ]
        ]
    ]

let tileTapModalRight model dispatch =
    Tile.parent [ Tile.Size Tile.Is3 ] [ 
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ ClassName "modalLeft" ] [
                        Container.container [] [ tileTapGameLoopToggle model dispatch ]
                        Container.container [] [
                            h2 [] [ str "Round Stats: "] 
                            div [] [ str ( modelValueAsString "Round Score: " model.RoundScore ) ]
                            div [] [ str ( "Round Timer: " + ( gameTickClock model.GameClock ) + modelValueAsString " / " model.RoundTimer) ]
                            div [] [ str ( modelValueAsString "Round Mistakes: " model.RoundMistakes + modelValueAsString " / " model.AllowableRoundMistakes) ]
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