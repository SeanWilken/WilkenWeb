module PivotPoints

open Shared
open Shared.GridGame
open Elmish
open Browser
open Fable.React
open Fable.React.Props
open Fulma
    
// - Extras:
// speeds up like snake as more are picked up
// certain coins have certain effects (?) // turns to blocker // speed up round // reverse roll direction // etc..(?)

type PivotDirection =
    | Ascend
    | Descend

type Msg =
    // Game State
    | GameLoopTick
    | SetGameState of RoundState
    | SetDispatchPointer of float
    // Arrow Movement
    | MoveArrow // will be called on certain game ticks to move the balls position
    | PivotArrow of PivotDirection // pivot to either ascend or descend
    // round messages
    | Ignore // I really don't like this, how to assign NO message..need separate funcs
    | ResetRound // resets the board and player round details
    | EndRound // you crashed and will be brought to game over screen
    | ExitGameLoop // Call to ensure no window intervals running when game is exited
    | QuitGame // Leave this game and return to the code gallery

type LaneDetails = {
    Style : CSSProp list
    Message : Msg
}

// content descriptions
let pivotPointsDescriptions = [
    "- Select Start to begin the game." 
    "- Arrow will continue to move in the direction it is pointing in intervals."
    "- Pivot the arrow from it's current direction to either direction of the intersecting lane."
    "- Red will pivot the arrow to descend the intersecting lane (left in a row, up in a column)"
    "- Blue will pivot the arrow to ascend the intersecting lane (right in a row, down in a column)"
    "- Avoid crashing into the lane blockers, or it's game over."
    "- Guide the arrow to the flags to collect points."
    // "- The more points you get, the faster the movement interval scale gets." // - Need to implement
]

let modelPivotPointRoundDetails ( model : SharedPivotPoint.Model ) =
    [
        "You collected " + string model.CoinsCollected + " coins."
        "You lasted " + string ( SharedViewModule.gameTickClock model.GameClock ) + " seconds."
    ]

let controlList = [ 
    "Settings", (SetGameState (RoundState.Settings)) 
]

let ascendLane = Some { Style = [ BackgroundColor "#801515" ]; Message = PivotArrow PivotDirection.Ascend }
let descendLane = Some { Style = [ BackgroundColor "#143054" ]; Message = PivotArrow PivotDirection.Descend }
let floorLane = [ None; ascendLane; ascendLane; ascendLane; ascendLane; ascendLane; ascendLane; descendLane; ]
let ceilingLane = [ ascendLane; descendLane; descendLane; descendLane; descendLane; descendLane; descendLane; None ]

// Update functions ---------

// shared between goal roll and pivot points...
let moveArrowForward ( model : SharedPivotPoint.Model) =
    let gameBoard = model.GameBoard
    let piecePositionIndex = RollableGridGameHelpers.getPiecePositionIndex gameBoard Ball
    if piecePositionIndex = -1
        then gameBoard
        else RollableGridGameHelpers.checkDirectionMovement piecePositionIndex model.BallDirection gameBoard

// GAME LOOP FUNCTIONS

// Time drives main game state, as things happen in intervals contained within the main loop
let startGameLoop ( model : SharedPivotPoint.Model ) dispatch =
    if model.DispatchPointer = 0.0 && model.GameState = Paused
        then
            window.setInterval((fun _ -> dispatch (GameLoopTick)), 250)
            |> fun loopFloat -> dispatch (SetDispatchPointer loopFloat)
        else
            SharedViewModule.stopGameLoop model.DispatchPointer
            dispatch ( SetDispatchPointer 0.0 )

// COINS
let coinSpawnPosition ( gridBoard : GridBoard ) =
    let validPositions = 
        [ for i in 0 .. gridBoard.GridPositions.Length - 1 do
            if gridBoard.GridPositions.Item(i) = Blank 
                then i
                else -1 
        ]
    List.filter ( fun x -> ( x <> -1 ) ) ( validPositions )
    |> fun availablePositions -> 
        availablePositions.Item(SharedTileSort.randomIndex availablePositions.Length)

//----------------

// Lifecycle -------------

let init(): SharedPivotPoint.Model * Cmd<Msg> =
    SharedPivotPoint.initModel, Cmd.none

let update msg ( model : SharedPivotPoint.Model ) : SharedPivotPoint.Model * Cmd<Msg> = 
    match msg with
    | GameLoopTick ->
        let coinPoints = 
            match getObjectPositionIndex model.GameBoard Goal with
            | Some x -> 0
            | None -> 1
        let gridWithCoinUpdate = 
            if coinPoints = 1 
                then 
                    let newCoinPosition = coinSpawnPosition model.GameBoard
                    updatePositionWithObject model.GameBoard Goal newCoinPosition
                else model.GameBoard

        let tickedClock = model.GameClock + 1
        let rollInterval = model.RollInterval + 1
        if rollInterval > 2
            then { model with GameBoard = gridWithCoinUpdate; RollInterval = 0; GameClock = tickedClock; CoinsCollected = model.CoinsCollected + coinPoints }, Cmd.ofMsg MoveArrow
            else { model with GameBoard = gridWithCoinUpdate; RollInterval = rollInterval; GameClock = tickedClock; CoinsCollected = model.CoinsCollected + coinPoints }, Cmd.none
    | SetDispatchPointer pointer ->
        if model.GameState = Won 
            then SharedPivotPoint.initModel, Cmd.none
            else
                if pointer <> 0.0 then Playing else Paused
                |> fun gameRoundState -> { model with GameState = gameRoundState; DispatchPointer = pointer }, Cmd.none
    | SetGameState gameState ->
        if gameState <> Playing && model.DispatchPointer <> 0.0 
            then 
                SharedViewModule.stopGameLoop model.DispatchPointer
                { model with DispatchPointer = 0.0; GameState = gameState; }, Cmd.none
            else
                { model with GameState = gameState }, Cmd.none
    | PivotArrow pivotDirection ->
        match model.BallDirection with
        | MovementDirection.Up
        | MovementDirection.Down ->
            match pivotDirection with
            | Ascend ->
                { model with BallDirection = MovementDirection.Right; BoardOrientation = SharedPivotPoint.LaneOrientation.LaneRow }, Cmd.none
            | Descend ->
                { model with BallDirection = MovementDirection.Left; BoardOrientation = SharedPivotPoint.LaneOrientation.LaneRow }, Cmd.none
        | MovementDirection.Left
        | MovementDirection.Right ->
            match pivotDirection with
            | Ascend ->
                { model with BallDirection = Down; BoardOrientation = SharedPivotPoint.LaneOrientation.LaneColumn }, Cmd.none
            | Descend ->
                { model with BallDirection = Up; BoardOrientation = SharedPivotPoint.LaneOrientation.LaneColumn }, Cmd.none
    | MoveArrow ->
        let movedArrowGrid = moveArrowForward model
        if movedArrowGrid = model.GameBoard
            then model, Cmd.ofMsg EndRound
            else { model with GameBoard = movedArrowGrid }, Cmd.none
    | ResetRound ->
        if (model.DispatchPointer <> 0.0) then SharedViewModule.stopGameLoop(model.DispatchPointer)
        SharedPivotPoint.initModel, Cmd.none
    | EndRound ->
        SharedViewModule.stopGameLoop model.DispatchPointer
        { model with GameBoard = SharedPivotPoint.demoGameBoard; DispatchPointer = 0.0; GameState = Won }, Cmd.none
    | ExitGameLoop ->
        if (model.DispatchPointer <> 0.0) then SharedViewModule.stopGameLoop(model.DispatchPointer)
        model, Cmd.none
    | QuitGame ->
        if (model.DispatchPointer <> 0.0) then SharedViewModule.stopGameLoop(model.DispatchPointer)
        model, Cmd.none
    | _ -> model, Cmd.none

// --------------------------

// VIEW FUNCTIONS 

let roundStateToggleString ( model : SharedPivotPoint.Model ) = 
    if model.GameState = Won || model.GameState = Settings
        then "Play"
        elif ( model.DispatchPointer <> 0.0 ) then "Pause" 
        else if model.GameClock <> 0 then "Resume" else "Start"

let roundStateToggle ( model : SharedPivotPoint.Model ) dispatch =
    let toggleString = roundStateToggleString model
    a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ) ] [
        h1 [ ClassName "modalControls" ] [ str ( toggleString ) ] 
    ]

// SETTINGS VIEW

let rulesAndSettingsGameControls ( model : SharedPivotPoint.Model ) dispatch =
    let toggleString = roundStateToggleString model
    div [] [
        div [] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str ( toggleString + " Round" ) ] ]
        div [] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Restart Round" ] ]
    ]

let modalGameSettingsView model dispatch =
    div [ ClassName "modalAltContent"; Style [ TextAlign TextAlignOptions.Center ] ] [
        Container.container [] [ rulesAndSettingsGameControls model dispatch ]
    ]

let gameRulesAndSettingsView model dispatch =
    div [] [
        SharedViewModule.gameInstructionContent pivotPointsDescriptions
        modalGameSettingsView model dispatch
    ]

// Rules
let gameContentViewControls model controlList dispatch =
    div [] [
        Level.level [ Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            Level.item [] [ roundStateToggle model dispatch ]
            for controlTitle, controlMsg in controlList do
                Level.item [] [
                    a [ OnClick ( fun _ -> controlMsg |> dispatch ) ] [
                        h1 [ ClassName "modalControls" ] [ str controlTitle ]
                    ]
                ]
        ]
    ]

// Game Board

let directionArrowImage direction =
    match direction with
    | MovementDirection.Up -> "UpArrow"
    | MovementDirection.Down -> "DownArrow"
    | MovementDirection.Left -> "LeftArrow"
    | MovementDirection.Right -> "RightArrow"

let pivotPointTileView rollDirection positionObject style message dispatch =
    match positionObject with // REVIEW STYLE ON THESE
    | Ball ->
        let ballArrowImage = "./imgs/icons/" + directionArrowImage rollDirection + ".png"
        Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src ballArrowImage ] ] ]
    | Blocker ->
        Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Blocker.png" ] ] ]
    | Goal ->
        Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ]
    | _ ->
        Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Key.png" ] ] ]

let parseLaneDetails details =
    match details with
    | Some laneDetail ->
        laneDetail.Style, laneDetail.Message
    | None ->   
        [], Ignore // don't like ignore

let pivotPointsLaneCreator isRow rollDirection (laneStyle : LaneDetails option) ( rowPositions: LaneObject list ) dispatch =
    let style, message = parseLaneDetails laneStyle
    let laneStyle = if isRow then Style [ Display DisplayOptions.Flex; AlignContent AlignContentOptions.Center; JustifyContent "center" ] else Style []
    div [ laneStyle ] [
        for positionObject in rowPositions do 
            pivotPointTileView rollDirection positionObject style message dispatch
    ]

let laneStyleCenterPositions ceiling position =
    [ for i in 0 .. ceiling - 1 do
        if i < position then Some { Style = [ BackgroundColor "#801515" ]; Message = PivotArrow PivotDirection.Descend }
        elif i > position then Some { Style = [ BackgroundColor "#143054" ]; Message = PivotArrow PivotDirection.Ascend }
        else None ]

let laneStyleCreator ballLaneIndex ceiling = 
    if ballLaneIndex = 0 then floorLane
    elif ballLaneIndex = 7 then ceilingLane
    else laneStyleCenterPositions ceiling ballLaneIndex

let findBallLaneIndex (gridLanes: (LaneObject list) list ) =
    let laneIndexes = 
        [ for i in 0 .. gridLanes.Length - 1 do
            if ( List.contains (Ball) ( gridLanes.Item(i)))
                then Some i
                else None
        ]
    let ballLaneIndex = List.tryFind ( fun x -> x <> None ) laneIndexes
    match ballLaneIndex with
    | Some (Some i) -> i
    | _ -> -1

let laneView isRow ( model : SharedPivotPoint.Model ) ( board : list<list<LaneObject>> ) ( laneStyles : list<option<LaneDetails>> ) ceiling dispatch =
    let laneGridStyle = 
        [ Display DisplayOptions.Flex; AlignContent AlignContentOptions.Center; JustifyContent "center" ]
        |> List.append ( if isRow then [ FlexDirection "column"; ] else [ FlexDirection "row"; ] )
    div [ Style laneGridStyle ] [
        for i in 0 .. ceiling - 1 do
            pivotPointsLaneCreator isRow model.BallDirection (laneStyles.Item(i)) (board.Item(i)) dispatch
    ]

//----------------

let pivotPointsBoardView (model : SharedPivotPoint.Model) dispatch =
    let gridBoard = model.GameBoard
    let ceiling = 8
    match model.BallDirection with

    | MovementDirection.Left
    | MovementDirection.Right ->
        let board = GridGame.getPositionsAsRows gridBoard ceiling
        let ballLaneIndex = findBallLaneIndex board
        let laneStyles = laneStyleCreator ballLaneIndex ceiling
        laneView true model board laneStyles ceiling dispatch

    | MovementDirection.Up
    | MovementDirection.Down ->
        let board = GridGame.getPositionsAsColumns gridBoard ceiling
        let ballLaneIndex = findBallLaneIndex board
        let laneStyles = laneStyleCreator ballLaneIndex ceiling
        laneView false model board laneStyles ceiling dispatch

// modal content container
let pivotPointsModalContent ( model : SharedPivotPoint.Model ) dispatch =
    SharedViewModule.gameModalContent ( 
        match model.GameState with
        | RoundState.Settings -> div [] [ gameRulesAndSettingsView model dispatch ] 
        | Won -> SharedViewModule.roundCompleteContent ( modelPivotPointRoundDetails model )
        | _ -> pivotPointsBoardView model dispatch 
    )

//----------------

// main view
let view ( model : SharedPivotPoint.Model ) dispatch =
    div [] [
        ( SharedViewModule.sharedModalHeader "Pivot Points" QuitGame dispatch )
        ( pivotPointsModalContent model dispatch ) 
        ( gameContentViewControls model controlList dispatch )
    ]