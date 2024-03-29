module TileTap

open System
open Shared
open GridGame
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Browser

(*

// Bomb diffuse by clicking a tile sequence around it?

// implement?
// store this on the model?
// let roundDifficultySpawnInterval difficulty =
//     match difficulty with
//     | SharedTileTap.TileTapDifficulty.Simple -> 6
//     | SharedTileTap.TileTapDifficulty.Easy -> 4
//     | SharedTileTap.TileTapDifficulty.Intermediate -> 2
//     | SharedTileTap.TileTapDifficulty.Hard -> 1

*)

type Msg =
    // GAME LOOP
    | SetGameState of RoundState
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

// *********************************

// Module Content & Helpers -

let tileTapDescriptions = [
    // "Survival Mode:"
    "- Tap the tile before it's timer runs out."
    "- If the tile timer reaches 0 adds 1 mistake."
    "- Don't tap bombs, they add 3 mistakes."
    "- Tap a Heart to take away 1 mistake."
    "- Make it until the round timer ends."
    "- Hard mode: 1 mistake ends it all, unlimited time."
]

let controlList = [ 
    "Settings", ( SetGameState ( RoundState.Settings ) ) 
]

let modelTileTapRoundDetails ( model : SharedTileTap.Model ) = [
    ( modelValueAsString "Round Score: " model.CompletedRoundDetails.RoundScore )
    "Round Timer: " + ( SharedViewModule.gameTickClock model.CompletedRoundDetails.GameClock ) + modelValueAsString " / " model.RoundTimer
    modelValueAsString "Round Mistakes: " model.CompletedRoundDetails.RoundMistakes + modelValueAsString " / " model.AllowableRoundMistakes
]

// ********************************

// LifeCycle & LifeCycle Helper Functions

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
let randomTapTile randVal =
    if randVal < 15 then TapTileValue.Bomb, 21
    elif randVal >= 15 && randVal < 50 then TapTileValue.Minor, 17
    elif randVal >= 50 && randVal < 75 then TapTileValue.Modest, 13
    elif randVal >= 75 && randVal < 95 then TapTileValue.Major, 9
    else TapTileValue.Heart, 9

// Given a Gridboard and desired position,
// return that Gridboard with a tile @ desired position
// What about Clock, Heart or Bomb???
let insertNewTapTile gridBoard position =
    let gridPositions = gridBoard.GridPositions
    let tapTileValue, tapTileLifeTime = randomTapTile ( randomValue ( 100 ) )
    { GridPositions = [
        for i in 0 .. ( gridPositions.Length - 1 ) do
            if i = position
                then TapTile { TapPosition = position; LifeTime = tapTileLifeTime; Value = tapTileValue } 
                else gridPositions.[i]
    ] }

// Given a Gridboard, return a list of it's tile positions
let activeTilePositionsFromBoard gridBoard =
    gridBoard.GridPositions
    |> List.map ( fun x -> 
        match x with 
        | TapTile x -> x.TapPosition
        | _ -> 0
    ) |> List.filter ( fun x -> x <> 0 )

// Increment the LifeTime value on every tile on the Gridboard
let tickActiveTiles gridBoard =
    { GridPositions =
        gridBoard.GridPositions
        |> List.map ( fun x -> 
            match x with 
            | TapTile x -> TapTile ( { x with LifeTime = x.LifeTime - 1 } )
            | _ -> Blank
        ) 
    }

// Returns the first ( Some TapTile ) 
// if found on the given Gridboard.
let expiredTileFromGrid grid =
    ( List.tryFind ( fun x -> 
        match x with 
            | TapTile x -> x.LifeTime <= 0
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
// change this for the lifeTime to be reversed from curent implementation..
let calculateTilePointValue ( tile : TapTile )=
    match tile.Value with 
    | Minor -> 1 * tile.LifeTime
    | Modest -> 2 * tile.LifeTime
    | Major -> 5 * tile.LifeTime
    | _ -> 0

let gameModeRoundMistake ( model : SharedTileTap.Model ) ( mistakeValue : int ) : ( SharedTileTap.Model * Cmd<Msg> )=
    if model.GameMode = SharedTileTap.TileTapGameMode.Survival
        then 
            let totalRoundMistakes = if model.CurrentRoundDetails.RoundMistakes + mistakeValue < 0 then 0 else model.CurrentRoundDetails.RoundMistakes + mistakeValue
            printfn "%i" totalRoundMistakes
            let com = 
                if ( model.AllowableRoundMistakes > 0 ) && ( totalRoundMistakes >= model.AllowableRoundMistakes ) 
                    then Cmd.ofMsg EndRound
                    else Cmd.none
            { model with CurrentRoundDetails = { model.CurrentRoundDetails with RoundMistakes = totalRoundMistakes } }, com
        else
            let currentTimeExpired = model.CurrentRoundDetails.GameClock + ( mistakeValue * 4 )
            let com = if ( model.RoundTimer * 4 <= currentTimeExpired ) then Cmd.ofMsg EndRound else Cmd.none
            { model with 
                CurrentRoundDetails = { model.CurrentRoundDetails with RoundMistakes = model.CurrentRoundDetails.RoundMistakes + 1; GameClock = currentTimeExpired } 
            }, com

// Initialize the modules model
// No command dispatched currently
let init(): SharedTileTap.Model * Cmd<Msg> =
    SharedTileTap.initModel, Cmd.none

// Handles the various different messages that
// can be dispatched throughout the use and
// interaction of this module
let update msg ( model : SharedTileTap.Model ) =
    match msg with
    // 'Tick' Interval in which the module operates.
    // this is dispatched through the browser windows setInterval function
    // Checks against Round 'Allowances' to see if the round should be stopped.
    // If round is still in play, then it will dispatch 
    // to check for expired tiles
    // spawn a new tile if the spawn interval is matched or exceeded
    | GameLoopTick -> // this should check against Round 'Allowances'
        // If more than mistakes are made than the difficulty on the model allows
        printfn "%i" model.CurrentRoundDetails.RoundMistakes
        if ( model.AllowableRoundMistakes > 0 ) && ( model.CurrentRoundDetails.RoundMistakes >= model.AllowableRoundMistakes ) then model, Cmd.ofMsg EndRound
        // End the round when the GameClock exceeds the RoundTimer.  ( -1 : no round timer)
        elif ( model.RoundTimer > 0 ) && ( ( model.CurrentRoundDetails.GameClock / 4 ) >= model.RoundTimer ) then model, Cmd.ofMsg EndRound
        // No conditions to end round met, batch check grid and spawn tile commands
        else { model with CurrentRoundDetails = { model.CurrentRoundDetails with GameClock = model.CurrentRoundDetails.GameClock + 1 } }, Cmd.batch [ Cmd.ofMsg ( CheckGridboardTiles ); Cmd.ofMsg ( SpawnNewActiveTile ) ]
    // Game starts
    | SetDispatchPointer flt ->
        // if I set the float as any float that isn't zero, you are playing
        if flt <> 0.0 
            then Playing 
            else Paused // if it is zero you are paused (or other state)
        |> fun gameRoundState -> { model with GameState = gameRoundState; DispatchPointer = flt }, Cmd.none
    | SetGameState gameState ->
        if model.DispatchPointer <> 0.0 then SharedViewModule.stopGameLoop model.DispatchPointer
        { model with GameState = gameState; DispatchPointer = 0.0 }, Cmd.none
    // Change Round Parameters based on requested difficulty
    | ChangeGameMode gameMode ->
        SharedTileTap.updateModelGameMode model gameMode, Cmd.ofMsg ResetRound
    | ChangeDifficulty difficulty ->
        if model.GameMode = SharedTileTap.TileTapGameMode.Survival
            then SharedTileTap.updateSurvivalModeDifficulty model difficulty, Cmd.ofMsg ResetRound
            else SharedTileTap.updateTimeAttackModeDifficulty model difficulty, Cmd.ofMsg ResetRound
    // If there hasn't been a new tile placed onto the Gridboard
    // but the interval is matched or exceeded, generate the Gridboard with a new tile added
    // otherwise increment the LastSpawnInterval by one
    | SpawnNewActiveTile ->
        // spawnInterval should be based on difficulty?
        // check if max amount of tiles is reached && no tile spawned in last 2 ticks (.5 second)
        if ( model.LastSpawnInterval >= 2 ) then //( model.TilesSpawned < 10 ) && 
            // filter a list of all positions by what is already active
            activeTilePositionsFromBoard model.TileTapGridBoard
            // select a position within the Gridboard to place a new tile
            |> fun activeTilePositionList -> tileSpawnPosition activeTilePositionList 
            // return the Gridboard with the selected position filled by a new tile
            |> fun selectedPosition -> insertNewTapTile model.TileTapGridBoard selectedPosition
            // model with new taptile in active tile list
            |> fun gridBoardWithTile -> { 
                model with 
                    TileTapGridBoard = gridBoardWithTile; 
                    LastSpawnInterval = 0; 
                    CurrentRoundDetails = { model.CurrentRoundDetails with TilesSpawned = model.CurrentRoundDetails.TilesSpawned + 1 } 
            }, Cmd.none
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
                CurrentRoundDetails = {
                    model.CurrentRoundDetails with 
                        TilesSmashed = model.CurrentRoundDetails.TilesSmashed + 1
                        RoundScore = model.CurrentRoundDetails.RoundScore + ( calculateTilePointValue tappedTile )
                }
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
        if (model.DispatchPointer <> 0.0) then SharedViewModule.stopGameLoop(model.DispatchPointer)
        SharedTileTap.endRound model, Cmd.none
    // Resets the values for a round aside 
    // from it's difficulty parameters
    | ResetRound ->
        if (model.DispatchPointer <> 0.0) then SharedViewModule.stopGameLoop(model.DispatchPointer)
        SharedTileTap.resetRound model, Cmd.none
    // Stops the setInterval dispatch loop when the game is exited
    | ExitGameLoop -> 
        if (model.DispatchPointer <> 0.0) then SharedViewModule.stopGameLoop(model.DispatchPointer)
        model, Cmd.ofMsg QuitGame
    // Msg that will be intercepted by CodeGallery
    // where User will be returned to
    | QuitGame -> 
        model, Cmd.none

// --------------------------------------

// CONTENT --------
let colorForTile ( tile : TapTile ) =
    match tile.Value with
    | TapTileValue.Bomb -> "#FF2843"
    | TapTileValue.Heart -> "#000000"
    | Minor -> "#000000"
    | Modest -> "#555555"
    | Major -> "#ffffff"
    |> fun tileColor -> Style [ Background tileColor; ]

let tileTapRowCreator ( rowPositions: LaneObject list ) dispatch =
    div [ Style [ Display DisplayOptions.Flex; AlignContent AlignContentOptions.Center; JustifyContent "center" ] ] [
        for positionObject in rowPositions do // REVIEW STYLE ON THESE
            match positionObject with
            | TapTile x -> 
                match x.Value with 
                | TapTileValue.Bomb ->
                    Box.box' [ Props [ ClassName "gameTile"; OnClick (fun _ -> DestroyTile ( x ) |> dispatch); colorForTile x ] ] [ Image.image [] [ img [ Src "./imgs/icons/Bomb.png"] ] ]
                | TapTileValue.Heart ->
                    Box.box' [ Props [ ClassName "gameTile"; OnClick (fun _ -> DestroyTile ( x ) |> dispatch); colorForTile x ] ] [ Image.image [] [ img [ Src "./imgs/icons/Heart.png"] ] ]
                | TapTileValue.Minor
                | TapTileValue.Modest
                | TapTileValue.Major ->
                    Box.box' [ Props [ ClassName "gameTile"; OnClick (fun _ -> DestroyTile ( x ) |> dispatch); colorForTile x ] ] [ str ( SharedViewModule.gameTickClock x.LifeTime ) ]
            | _ -> Box.box' [ Props [ ClassName "blankTile"; OnClick (fun _ -> Mistake (1) |> dispatch); ] ] [ Image.image [] [ img [ Src "./imgs/icons/Ball.png"] ] ]
    ]

// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileTapBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [ for row in board do tileTapRowCreator row dispatch ]
  
let roundOverString ( model : SharedTileTap.Model ) =
    Container.container [ Container.Props [ Style [ FontSize 30; Color "#FFFFFF" ] ] ] [
        h1 [ Style [ Padding 10; Color "#69A69A" ] ] [ str ( "The round lasted " + ( SharedViewModule.gameTickClock ( model.CompletedRoundDetails.GameClock ) ) + " second(s)," ) ]
        h1 [ Style [ Padding 10; Color "#69A69A" ] ] [ str ( "You smashed " + string ( model.CompletedRoundDetails.TilesSmashed ) + " tiles and made " + string ( model.CompletedRoundDetails.RoundMistakes ) + " mistakes," ) ]
        h1 [ Style [ Padding 10; Color "#69A69A" ] ] [ str ( " This results in a final score of:" ) ]
        h1 [ Style [ Padding 20; Color "#00ff00"; FontSize 50 ] ] [ str ( string ( model.CompletedRoundDetails.RoundScore ) ) ]
    ]

// *********************************

// Controls are being made in more custom override fashion,
// as the needs for this submodule require a bit more of a workaround
// due to Async.RunAsyncronously: not available in Fable
// wanting to set the window interval to dispatch game tick functions
    // while still being able to set the GameLoopTick's pointer returned setInterval on the model
// Tried with subscriptions, but as implemented it would be firing off regardless of the current
    // model that was being used / viewed by the User.
        // launch a sub-program with the subscriptions being fired off when that module is launched?
// Currently hacked around the Elmish dispatch loop

// Time drives main game state, as things happen in intervals contained within the main loop
let startGameLoop ( model : SharedTileTap.Model ) dispatch =
    if model.DispatchPointer = 0.0 && model.GameState = Paused
        then
            window.setInterval((fun _ -> dispatch (GameLoopTick)), 250)
            |> fun loopFloat -> dispatch (SetDispatchPointer loopFloat)
        else
            window.clearInterval(model.DispatchPointer)
            dispatch ( SetDispatchPointer 0.0 )

let roundStateToggleString ( model : SharedTileTap.Model ) = 
    if model.GameState = Won || model.GameState = Settings
        then "Play"
        elif ( model.DispatchPointer <> 0.0 ) then "Pause" 
        else if model.CurrentRoundDetails.GameClock <> 0 then "Resume" else "Start"

// custom non-shared right modal, needs to handle functions outside update loop to dispatch messages
let tileTapGameLoopToggle ( model : SharedTileTap.Model ) dispatch =
    let toggleString = roundStateToggleString model
    Container.container [] [
        div [] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str ( toggleString + " Round" ) ] ]
        div [] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Restart Round" ] ]
        div [] [
            span [] [ 
                h3 [] [ str "Game Mode: "] 
                div [] [ a [ OnClick ( fun _ -> ( ChangeGameMode SharedTileTap.TileTapGameMode.Survival |> dispatch ) ) ] [ str "Survival" ] ]
                div [] [ a [ OnClick ( fun _ -> ( ChangeGameMode SharedTileTap.TileTapGameMode.TimeAttack |> dispatch ) ) ] [ str "Time Attack" ] ] 
            ]
        ]
        div [] [
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
    Column.column [] [
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ ClassName "modalAltContent" ] [
                        Container.container [] [ tileTapGameLoopToggle model dispatch ]
                        Container.container [] [
                            h2 [] [ str "Round Stats: "] 
                            for detail in ( modelTileTapRoundDetails model) do div [] [ str detail ]
                        ]
                    ]
                ]
            ]
        ]
    ]
// ----------------

// *********************************

let tileTapModalContent  ( model : SharedTileTap.Model ) dispatch =
    SharedViewModule.gameModalContent ( 
        Column.column [] [
            match model.GameState with
            | RoundState.Settings -> 
                div [] [
                    SharedViewModule.codeModalInstructionContent tileTapDescriptions
                    tileTapModalRight model dispatch
                ]
            | Won -> SharedViewModule.roundCompleteContent ( modelTileTapRoundDetails model )
            | _ -> tileTapBoardView model.TileTapGridBoard dispatch 
        ]
    )

let roundStateToggle ( model : SharedTileTap.Model ) dispatch =
    let toggleString = roundStateToggleString model
    a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ) ] [
        h1 [ ClassName "modalControls" ] [ str ( toggleString ) ] 
    ]

//Custom
let codeModalFooterOverride model controlList dispatch =
    div [] [
        Level.level [Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            Level.item [] [ roundStateToggle model dispatch ]
            for controlTitle, controlMsg in controlList do
                Level.item [] [
                    a [ OnClick ( fun _ -> controlMsg |> dispatch ) ] [
                        h1 [ ClassName "modalControls"; ] [ str controlTitle ]
                    ]
                ]
        ]
    ]
// ----------------


// MODULE VIEW ----
let view model dispatch =
    div [] [
        ( SharedViewModule.sharedModalHeader "Tile Tap" QuitGame dispatch )
        ( tileTapModalContent model dispatch ) 
        ( codeModalFooterOverride model controlList dispatch )
    ]
// ----------------