module GoalRoll

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Shared
open Shared.GridGame
open Shared.SharedGoalRoll

// ---------TODO---------
    // - MovementMade (Undo Move)
    // - Roll Ball when next to flag (lvl3)
    // shared view

type Msg =
    | SetGameState of RoundState
    | ResetRound
    | LoadRound of int
    | RollBall of GridGame.MovementDirection
    | CheckSolution
    | QuitGame

// content descriptions
let goalRollDescriptions = [
    "- Use the arrows next to the ball in order to roll it in the desired direction."
    "- Travels in straight lines and stops when it hits a wall or blocked tile."
    "- There must be space for the movement arrow in order to roll."
    "- Have the ball stop on the goal to win."
]

// content selection controls
let controlList = [ 
    "Play", (SetGameState (Playing))
    "Settings", (SetGameState (Settings)) 
]

let gameControls = [
    "Reset Round", ResetRound
    "Level 0", LoadRound 0
    "Level 1", LoadRound 1
    "Level 2", LoadRound 2
    "Level 3", LoadRound 3
]

let modelGoalRollRoundDetails ( model : SharedGoalRoll.Model ) = [
    "You completed level " + string model.LevelIndex + "."
    "It took you " + string model.MovesMade + " number of moves."
]

let levelCeiling = 3

let getBallRollPositionIndex ballPosition direction =
    match direction with
    | Up -> (ballPosition - 8)
    | Down -> (ballPosition + 8)
    | Left -> (ballPosition - 1)
    | Right -> (ballPosition + 1)

let getNormalizedArrowPosition normalizedBallPosition direction =
    match direction with
    | Up -> (normalizedBallPosition - 8)
    | Down -> (normalizedBallPosition + 8)
    | _ -> (normalizedBallPosition % 8)

let checkNormalizedArrowPosition normalizedArrowPosition direction =
    match direction with
    | Up -> normalizedArrowPosition > 0
    | Down -> normalizedArrowPosition <= 64
    | Left -> normalizedArrowPosition <> 1
    | Right -> normalizedArrowPosition <> 0

let gridWithoutMoveArrows positions =
    let thing = List.map (fun x -> match x with | MoveArrow _ -> Blank | _ -> x ) positions.GridPositions
    { GridPositions = thing }

let gridWithMovementArrow positions direction =
    let ballPositionIndex = SharedGoalRoll.getBallPositionIndex positions
    let normalizedBallPositionIndex = ballPositionIndex + 1
    let normalizedArrowPositionIndex = getNormalizedArrowPosition normalizedBallPositionIndex direction
    let validArrowPosition = checkNormalizedArrowPosition normalizedArrowPositionIndex direction
    if validArrowPosition
        then 
            let thing = getBallRollPositionIndex ballPositionIndex direction
            if (checkGridPositionForObject positions thing Blank)
                then updatePositionWithObject positions (MoveArrow direction) thing
                else positions
        else positions

let gridWithGoal positions goalPosition =
    updatePositionWithObject positions Goal goalPosition

// this should really check for some kind of rollable bit to indicate can roll through / over?
let checkDirectionForRollable positions arrowlessGrid ballRollPositionIndex ballPositionIndex =
    if (checkGridPositionForObject arrowlessGrid ballRollPositionIndex Blank) || (checkGridPositionForObject arrowlessGrid ballRollPositionIndex Goal)
        then 
            let ballToBlankGrid = updatePositionWithObject (arrowlessGrid) Blank (ballPositionIndex)
            let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollPositionIndex
            ballRolledGrid
        else positions

let rec rollBall positions direction =
    let ballPositionIndex =  RollableGridGameHelpers.getPiecePositionIndex positions Ball
    let arrowlessGrid = gridWithoutMoveArrows positions
    let ballRollPositionIndex = getBallRollPositionIndex ballPositionIndex direction
    if RollableGridGameHelpers.checkDirectionalBound ballRollPositionIndex direction
        then 
            let grid = checkDirectionForRollable positions arrowlessGrid ballRollPositionIndex ballPositionIndex
            if grid = positions
                then positions
                else rollBall grid direction
        else positions

// --------------------------------------------------------------
//STATE LIFECYCLE

let init (): SharedGoalRoll.Model * Cmd<Msg> =
    SharedGoalRoll.initModel, Cmd.none

let update ( msg: Msg ) ( model: SharedGoalRoll.Model ): SharedGoalRoll.Model * Cmd<Msg> =
    match msg, model with
    | SetGameState gameState, model ->
        if model.GameState = Won
            then { model with GameState = gameState; MovesMade = 0 }, Cmd.none
            else { model with GameState = gameState }, Cmd.none
    | RollBall direction, model ->
        let roundMoves = model.MovesMade + 1
        let boardAfterRoll = rollBall model.CurrentGrid direction
        { model with CurrentGrid = boardAfterRoll; MovesMade = roundMoves }, Cmd.ofMsg CheckSolution
    | LoadRound levelIndex, model ->
        let newRound = SharedGoalRoll.loadRound levelIndex
        let newRoundModel : SharedGoalRoll.Model = { 
            LevelIndex = levelIndex
            InitialGrid = newRound
            CurrentGrid = newRound
            BallPositionIndex = SharedGoalRoll.getBallPositionIndex newRound
            GoalPositionIndex = SharedGoalRoll.getGoalPositionIndex newRound
            GameState = Playing
            MovesMade = 0
        }
        newRoundModel, Cmd.none
    | ResetRound, model ->
        let resetRound = model.InitialGrid
        { model with 
            CurrentGrid = resetRound;
            BallPositionIndex = SharedGoalRoll.getBallPositionIndex resetRound
            GameState = Playing
            MovesMade = 0
        }, Cmd.none 
    | CheckSolution, model ->
        let resetRound = model.InitialGrid
        let resetBallPosition = SharedGoalRoll.getBallPositionIndex resetRound
        if SharedGoalRoll.getBallPositionIndex model.CurrentGrid = model.GoalPositionIndex
            then
                { model with CurrentGrid = resetRound; BallPositionIndex = resetBallPosition; GameState = Won }, Cmd.none
            else
                model, Cmd.none
    | QuitGame, model -> model, Cmd.ofMsg QuitGame

// -------- GOAL ROLL VIEW --------



// main content
// // Assign positions to view elements
let goalRollRowCreator ( rowPositions: LaneObject list ) dispatch =
    div [ Style [ Display DisplayOptions.Flex; AlignContent AlignContentOptions.Center; JustifyContent "center" ] ] [
        for positionObject in rowPositions do
                match positionObject with // RESTYLE THESE
                | Blocker -> 
                    Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Blocker.png" ] ] ]
                | Ball ->
                    Box.box' [ Props [ ClassName "ballLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Ball.png" ] ] ]
                | Goal ->
                    Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ]
                | Heart ->
                    Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Heart.png"] ] ]
                | LaneLock ->
                    Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Lock.png"] ] ]
                | LaneKey ->
                    Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Key.png"] ] ]
                | Bomb ->
                    Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Bomb.png"] ] ]
                | MoveArrow Up -> 
                    Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Up |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/UpArrow.png"] ] ]
                | MoveArrow Down -> 
                    Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Down |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/DownArrow.png"] ] ]
                | MoveArrow Left -> 
                    Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Left |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/LeftArrow.png"] ] ]
                | MoveArrow Right -> 
                    Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Right |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/RightArrow.png"] ] ]
                | _ -> 
                    Box.box' [ Props [ ClassName "blankTile" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Ball.png" ] ] ]
    ]

let gameRulesAndSettingsView model dispatch =
    div [] [
        SharedViewModule.gameInstructionContent goalRollDescriptions
        SharedViewModule.codeModalControlsContent gameControls dispatch
    ]

// Rules
let gameContentViewControls model controlList dispatch =
    div [] [
        Level.level [ Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            for controlTitle, controlMsg in controlList do
                Level.item [] [
                    a [ OnClick ( fun _ -> controlMsg |> dispatch ) ] [
                        h1 [ ClassName "modalControls" ] [ str controlTitle ]
                    ]
                ]
        ]
    ]

// // Given a Goal Roll Level, create the Game Board via rows
let goalRollLevelCreator ( goalRollModel : SharedGoalRoll.Model) dispatch =
    let positions = goalRollModel.CurrentGrid
    // rule or exception? can't roll something without space leading to initiate the roll
    // this also needs to handle if the flag is in an arrows position?
    // VERTICAL MOVEMENTS
    let gridWithUpArrow = gridWithMovementArrow positions Up
    let gridWithDownArrow = gridWithMovementArrow gridWithUpArrow Down
    // HORIZONTAL MOVEMENTS
    let gridWithLeftArrow = gridWithMovementArrow gridWithDownArrow Left
    let gridWithRightArrow = gridWithMovementArrow gridWithLeftArrow Right
    // MAKE SURE GOAL IS PLACED IF ROLLED OVER // UGLY, REWORK
    let gameGrid = 
        if SharedGoalRoll.getBallPositionIndex positions <> goalRollModel.GoalPositionIndex
            then 
                if ( SharedGoalRoll.getGoalPositionIndex positions = -1 ) then gridWithGoal gridWithRightArrow goalRollModel.GoalPositionIndex
                else gridWithRightArrow
        else gridWithRightArrow
    // positions as rows
    let gridRows = getPositionsAsRows gameGrid 8
    div [] [ for row in gridRows do goalRollRowCreator row dispatch ]

// modal content container
let goalRollModalContent ( model : SharedGoalRoll.Model ) dispatch =
    SharedViewModule.gameModalContent (
        match model.GameState with 
        | Settings -> gameRulesAndSettingsView model dispatch
        | Won -> SharedViewModule.roundCompleteContent ( modelGoalRollRoundDetails model )
        | Paused
        | Playing -> goalRollLevelCreator model dispatch
    )

// --------------------------------

// main view
let view ( model : SharedGoalRoll.Model ) dispatch =
    div [] [
        ( SharedViewModule.sharedModalHeader "Goal Roll" QuitGame dispatch )
        ( goalRollModalContent model dispatch )
        SharedViewModule.codeModalFooter controlList dispatch
    ]