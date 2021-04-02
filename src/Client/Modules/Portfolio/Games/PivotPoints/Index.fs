module PivotPoints

open Shared
open Shared.GridGame

open Elmish

open Browser
open Fable.React
open Fable.React.Props
open Fulma

// collect coins -> spawn one at a time
    
    // - Extras:
    // speeds up like snake as more are picked up
    // certain coins have certain effects (?)
        // turns to blocker
        // speed up round
        // reverse roll direction
        // etc..(?)


type PivotDirection =
    | Ascend
    | Descend

type Msg =
    // game + state
    | GameLoopTick
    | SetGameState of RoundState
    | SetDispatchPointer of float
    // ball movement 
    | RollBall // will be called on certain game ticks to move the balls position
    | PivotBall of PivotDirection // pivot to either ascend or descend
    // score message
    | CollectPoints of int
    // round messages
    | Ignore // I really don't like this, how to return NO message?
    | ResetRound
    | EndRound
    | ExitGameLoop
    | QuitGame



// left is descend positions
// right is ascend positons
    // switch from row to column or vice versa on new direction
    // if column then row else column


// content descriptions
let pivotPointsDescriptions = [
    "- Select Start to begin game." 
    "- Once the ball begins to roll, it can't be stopped, only steered."
    "- Pivot the ball from it's current lane position to either direction of the intersecting lane."
    "- Red will pivot the ball to ascend the intersecting lane (right in a row, down in a column)"
    "- Blue will pivot the ball to descend the intersecting lane (left in a row, up in a column)"
    "- Roll the ball over the flag to collect points."
    "- Avoid crashing into the lane blockers, or it's game over."
    // "- The ball roll interval scales with time / points???"
]

let controlList = [ 
    "Settings", (SetGameState (RoundState.Settings)) 
    "Rules", (SetGameState (Instruction))
]

// RIPPED FROM TILETAP

// NEEDED TO MAKE THE TILES RED OR BLUE FOR PIVOT DIRECTION INDICATION
// let colorForTile tile =
//     match tile.Value with
//     | TapTileValue.Bomb -> "#FF2843"
//     | TapTileValue.Heart -> "#000000"
//     | Minor -> "#000000"
//     | Modest -> "#555555"
//     | Major -> "#ffffff"
//     |> fun tileColor -> Style [ Background tileColor; ]

let getBallRollPositionIndex ballPosition direction =
    match direction with
    | MovementDirection.Up -> (ballPosition - 8)
    | MovementDirection.Down -> (ballPosition + 8)
    | MovementDirection.Left -> (ballPosition - 1)
    | MovementDirection.Right -> (ballPosition + 1)

// REFACTOR ME PLEASE
    // CALLS FOLLOWING MATCH ARE SAME CODE WITH DIFFERENT CONSTRAINTS
let rollBallForward ( model : SharedPivotPoint.Model) =
    let gameBoard = model.GameBoard
    let ballPositionIndex = SharedGoalRoll.getBallPositionIndex gameBoard
    if ballPositionIndex = -1
        then gameBoard
        else
            match model.BallDirection with
            // MovementDirection - used to get the roll position
            | MovementDirection.Up ->
                let ballRollPositionIndex = getBallRollPositionIndex ballPositionIndex MovementDirection.Up
                if (ballRollPositionIndex >= 0)
                    then 
                        if gameBoard.GridPositions.Item (ballPositionIndex - 8) <> Blank 
                            then gameBoard
                            else let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                 let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollPositionIndex
                                 ballRolledGrid
                    else
                        let wrappedBallPosition = ballPositionIndex + 56 
                        if gameBoard.GridPositions.Item (wrappedBallPosition) <> Blank
                            then gameBoard
                            else    let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball wrappedBallPosition
                                    ballRolledGrid
            | MovementDirection.Down ->
                let ballRollPositionIndex = getBallRollPositionIndex ballPositionIndex MovementDirection.Down
                if (ballRollPositionIndex <= 63)
                    then 
                        if gameBoard.GridPositions.Item (ballPositionIndex + 8) <> Blank 
                            then gameBoard
                            else let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                 let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollPositionIndex
                                 ballRolledGrid
                    else 
                        let wrappedBallPosition = ballPositionIndex - 56 
                        if gameBoard.GridPositions.Item (wrappedBallPosition) <> Blank
                            then gameBoard
                            else    let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball wrappedBallPosition
                                    ballRolledGrid
            | MovementDirection.Right ->
                let ballRollPositionIndex = getBallRollPositionIndex ballPositionIndex MovementDirection.Right
                if (((ballRollPositionIndex) % 8) <> 0)
                    then 
                        if gameBoard.GridPositions.Item (ballPositionIndex + 1) <> Blank 
                            then gameBoard
                            else let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                 let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollPositionIndex
                                 ballRolledGrid
                    else
                        let wrappedBallPosition = ballPositionIndex - 7
                        if gameBoard.GridPositions.Item (wrappedBallPosition) <> Blank
                            then gameBoard
                            else    let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball wrappedBallPosition
                                    ballRolledGrid
            | MovementDirection.Left ->
                let ballRollPositionIndex = getBallRollPositionIndex ballPositionIndex MovementDirection.Left
                if (((ballRollPositionIndex + 1) % 8) >= 1)
                    then 
                        if gameBoard.GridPositions.Item (ballPositionIndex - 1) <> Blank 
                            then gameBoard
                            else let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                 let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollPositionIndex
                                 ballRolledGrid
                    else
                        let wrappedBallPosition = ballPositionIndex + 7
                        if gameBoard.GridPositions.Item (wrappedBallPosition) <> Blank
                            then gameBoard
                            else    let ballToBlankGrid = updatePositionWithObject (gameBoard) Blank (ballPositionIndex)
                                    let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball wrappedBallPosition
                                    ballRolledGrid

// GAME LOOP FUNCTIONS

// DUPLICATED FROM TILE TAP: BASIC, MOVE TO SHARED
let stopGameLoop loopFloat =
    window.clearInterval(loopFloat)

// Time drives main game state, as things happen in intervals contained within the main loop
let startGameLoop ( model : SharedPivotPoint.Model ) dispatch =
    if model.DispatchPointer = 0.0
        then
            window.setInterval((fun _ -> dispatch (GameLoopTick)), 250)
            |> fun loopFloat -> dispatch (SetDispatchPointer loopFloat)
        else
            stopGameLoop model.DispatchPointer
            dispatch ( SetDispatchPointer 0.0 )

//----------------

let init(): SharedPivotPoint.Model * Cmd<Msg> =
    SharedPivotPoint.initModel, Cmd.none


let update msg ( model : SharedPivotPoint.Model ) : SharedPivotPoint.Model * Cmd<Msg> = 
    match msg with

    | GameLoopTick ->
        // increase game clock if using one...
        // spawn coin if none on the board
        // roll the ball
        let rollInterval = model.RollInterval + 1
        if rollInterval > 2
            then { model with RollInterval = 0 }, Cmd.ofMsg RollBall
            else { model with RollInterval = rollInterval }, Cmd.none
    | SetDispatchPointer pointer ->
        // RIPPED FROM TILETAP
        if pointer <> 0.0 then Playing else Paused
        |> fun gameRoundState -> { model with GameState = gameRoundState; DispatchPointer = pointer }, Cmd.none
    | SetGameState gameState ->
        { model with GameState = gameState }, Cmd.none
    
    | PivotBall pivotDirection ->
        // direction on model
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


    | RollBall ->
        let rolledBallGrid = rollBallForward model
        if rolledBallGrid = model.GameBoard
            then model, Cmd.ofMsg EndRound
            else { model with GameBoard = rolledBallGrid }, Cmd.none
    
    | ResetRound ->
        model, Cmd.none

    | EndRound ->
        stopGameLoop model.DispatchPointer
        { model with DispatchPointer = 0.0; GameState = Won }, Cmd.none
    
    | ExitGameLoop ->
        model, Cmd.none
    
    | QuitGame ->
        model, Cmd.none
    
    | _ -> model, Cmd.none

// MISC HELPERS

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

//----------------

// COINS
// REWORK TO SEARCH FOR ANY OF A SPECIFIED OBJECT: SHARED
// let tileSpawnPosition activeTilePositionList =
//     // all positions (take a new parameter for gridSizeCeiling?)
//     let allGridPositions = [ 0..63 ] 
//     // filter the activeTilePositions from the totalPositions
//     List.filter ( fun x -> not (List.contains x activeTilePositionList ) ) ( allGridPositions )
//     // if there are available positions, select one randomly
//     |> fun availablePositions -> 
//         if not ( availablePositions.IsEmpty ) 
//             then availablePositions.[SharedTileSort.randomIndex availablePositions.Length] 
//             else ( SharedTileSort.randomIndex ( allGridPositions.Length - 1 ) )

// VIEW FUNCTIONS 

let roundStateToggleString ( model : SharedPivotPoint.Model ) = if ( model.DispatchPointer <> 0.0 ) then "Pause" else "Start"

let roundStateToggle ( model : SharedPivotPoint.Model ) dispatch =
    let toggleString = roundStateToggleString model
    a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ) ] [
        h1 [ ClassName "modalControls" ] [ str ( toggleString ) ] 
    ]

// SETTINGS VIEW

let modalGameSettingsControls ( model : SharedPivotPoint.Model ) dispatch =
    let toggleString = roundStateToggleString model
    Container.container [] [
        div [] [ a [ OnClick ( fun _ -> startGameLoop model dispatch |> ignore ); ] [ str ( toggleString + " Round" ) ] ]
        div [] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Restart Round" ] ]
    ]

let modalGameSettingsView model dispatch =
    Column.column [] [
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ ClassName "modalAltContent" ] [
                        Container.container [] [ modalGameSettingsControls model dispatch ]
                    ]
                ]
            ]
        ]
    ]

// Rules
let modalGameInstructionView model controlList dispatch =
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

// Switch ball to be arrow. Determine which arrow icon to use based on arrow direction

// refactor these into one call that can determine based on the board orientation
type LaneDetails = {
    Style : CSSProp list
    Message : Msg
}

let parseLaneDetails details =
    match details with
    | Some laneDetail ->
        laneDetail.Style, laneDetail.Message
    | None ->   
        [], Ignore

let pivotPointsRowCreator (laneStyle : LaneDetails option) ( rowPositions: LaneObject list ) dispatch =
    let style, message = parseLaneDetails laneStyle
    Level.level [ Level.Level.IsMobile ] [
        for positionObject in rowPositions do // REVIEW STYLE ON THESE
            Tile.child [] [
                match positionObject with
                | Ball -> 
                    Box.box' [ Props [ ClassName "blankTile"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/UpArrow.png"] ] ]
                | Blocker ->
                    Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Blocker.png" ] ] ]
                | Goal ->
                    Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ]
                | _ ->
                    // if position is less than ball position then red with descend else blue with ascend
                    Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Key.png" ] ] ]
            ]
        ]

let pivotPointsColumnCreator (laneStyle : LaneDetails option) ( rowPositions: LaneObject list ) dispatch =
    let style, message = parseLaneDetails laneStyle
    Container.container [] [
        for positionObject in rowPositions do // REVIEW STYLE ON THESE
            Tile.child [] [
                match positionObject with
                | Ball -> 
                    Box.box' [ Props [ ClassName "blankTile"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Ball.png"] ] ]
                | Blocker ->
                    Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Blocker.png" ] ] ]
                | Goal ->
                    Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ]
                | _ ->
                    // if position is less than ball position then red with descend else blue with ascend
                    Box.box' [ Props [ ClassName "genericLaneObject"; Style style; OnClick (fun _ -> message |> dispatch) ] ] [ Image.image [] [ img [ Src "./imgs/icons/Key.png" ] ] ]
            ]
        ]

let pivotPointsBoardView (model : SharedPivotPoint.Model) dispatch =
    let gridBoard = model.GameBoard
    match model.BallDirection with 
    | MovementDirection.Left
    | MovementDirection.Right ->
        let board = GridGame.getPositionsAsRows gridBoard 8
        let ballLaneIndex = findBallLaneIndex board
        // if rowIndex = 0 then 7 = RED
        // if rowIndex = 7 then 0 = BLUE
        // else -1
        Container.container [] [ 
            for i in 0 .. board.Length - 1 do
                let laneStyle  = 
                    if i < ballLaneIndex then Some { Style = [ BackgroundColor "#f00" ]; Message = PivotBall PivotDirection.Descend }
                    elif i > ballLaneIndex then Some { Style = [ BackgroundColor "#00f" ]; Message = PivotBall PivotDirection.Ascend }
                    else None
                pivotPointsRowCreator laneStyle (board.Item(i)) dispatch
        ]
    | MovementDirection.Up
    | MovementDirection.Down ->
        let board = GridGame.getPositionsAsColumns gridBoard 8
        let ballLaneIndex = findBallLaneIndex board
        // if columnIndex = 0 then 7 = RED
        // if columnIndex = 7 then 0 = BLUE
        // else -1
        Container.container [] [ 
            Level.level [] [
                for i in 0 .. board.Length - 1 do
                    let laneStyle  = 
                        if i < ballLaneIndex then Some { Style = [ BackgroundColor "#f00" ]; Message = PivotBall PivotDirection.Descend }
                        elif i > ballLaneIndex then Some { Style = [ BackgroundColor "#00f" ]; Message = PivotBall PivotDirection.Ascend }
                        else None
                    pivotPointsColumnCreator laneStyle (board.Item(i)) dispatch
            ]
        ]


// modal content container
let pivotPointsModalContent ( model : SharedPivotPoint.Model ) dispatch =
    SharedViewModule.gameModalContent ( 
        Column.column [] [
            match model.GameState with
            | RoundState.Settings -> modalGameSettingsView model dispatch
            | Instruction -> SharedViewModule.codeModalInstructionContent pivotPointsDescriptions
            | Won ->
                div [ ClassName "levelCompletedCard" ] [ 
                    Container.container [ Container.Props [ Style [ Padding 20 ] ] ] [
                        str "You crashed!"
                    ]
                    Container.container [ Container.Props [ Style [ FontSize 20; Padding 20] ] ] [
                        h2 [ Style [ FontSize 50; Color "#FF2843" ] ] [ str "Round Stats: "]  
                        div [ Style [ Padding 5; Color "#69A69A" ] ] [ str "over 9000!" ] //( modelValueAsString "Round Score: " model.CompletedRoundDetails.RoundScore ) ]
                        // div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( "Round Timer: " + ( gameTickClock model.GameClock ) + modelValueAsString " / " model.RoundTimer) ]
                    ]
                    // Container.container [ Container.Props [ Style [ FontSize 20; Padding 20 ] ] ] [
                    //     h2 [ Style [ FontSize 50; Color "#FF2843" ] ] [ str "Details: "]
                    //     roundOverString model
                    // ]
                ]
            | _ -> pivotPointsBoardView model dispatch 
        ]
    )

//----------------

// main view
let view ( model : SharedPivotPoint.Model ) dispatch =
    SharedViewModule.sharedViewModal
        true
        ( SharedViewModule.sharedModalHeader "Pivot Points" QuitGame dispatch )
        ( pivotPointsModalContent model dispatch ) 
        ( modalGameInstructionView model controlList dispatch )
