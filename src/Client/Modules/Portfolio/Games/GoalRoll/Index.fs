module GoalRoll

open System
open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Shared
open Shared.GridGame
open Shared.SharedGoalRoll
open System.Collections.Generic

// ---------TODO---------
    // - MovementMade (Undo Move)
    // - Roll Ball when next to flag (lvl3)
    // shared view

type Msg =
    | ResetRound
    | LoadRound of int
    | RollBall of GridGame.MovementDirection
    | CheckSolution
    | QuitGame

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
let gridWithGoal positions goalPosition =
    updatePositionWithObject positions Goal goalPosition
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

// this should really check for some kind of rollable bit to indicate can roll through / over 
let checkDirectionForRollable positions arrowlessGrid ballRollPositionIndex ballPositionIndex direction =
    if (checkGridPositionForObject arrowlessGrid ballRollPositionIndex Blank) || (checkGridPositionForObject arrowlessGrid ballRollPositionIndex Goal)
        then 
            let ballToBlankGrid = updatePositionWithObject (arrowlessGrid) Blank (ballPositionIndex)
            let ballRolledGrid = updatePositionWithObject ballToBlankGrid Ball ballRollPositionIndex
            ballRolledGrid
        else positions

// refactor please
let rec rollBallInGridDirection positions direction =
    let arrowlessGrid = gridWithoutMoveArrows positions
    let ballPositionIndex = SharedGoalRoll.getBallPositionIndex arrowlessGrid
    if ballPositionIndex = -1
        then positions
        else
            match direction with
            | Up ->
                let ballRollUpPositionIndex = getBallRollPositionIndex ballPositionIndex Up
                if (ballRollUpPositionIndex >= 0)
                    then 
                        let grid = checkDirectionForRollable positions arrowlessGrid ballRollUpPositionIndex ballPositionIndex direction
                        if grid = positions
                            then positions
                            else rollBallInGridDirection grid direction
                    else positions
            | Down ->
                let ballRollDownPositionIndex = getBallRollPositionIndex ballPositionIndex Down
                if (ballRollDownPositionIndex <= 63)
                    then 
                        let grid = checkDirectionForRollable positions arrowlessGrid ballRollDownPositionIndex ballPositionIndex direction
                        if grid = positions 
                            then positions
                            else rollBallInGridDirection grid direction
                    else positions
            | Right ->
                let ballRollRightPositionIndex = getBallRollPositionIndex ballPositionIndex Right
                if (((ballRollRightPositionIndex) % 8) <> 0)
                    then 
                        let grid = checkDirectionForRollable positions arrowlessGrid ballRollRightPositionIndex ballPositionIndex direction
                        if grid = positions 
                            then positions 
                            else rollBallInGridDirection grid direction
                    else positions
            | Left ->
                let ballRollLeftPositionIndex = getBallRollPositionIndex ballPositionIndex Left
                if (((ballRollLeftPositionIndex + 1) % 8) >= 1)
                    then 
                        let grid = checkDirectionForRollable positions arrowlessGrid ballRollLeftPositionIndex ballPositionIndex direction
                        if grid = positions 
                            then positions 
                            else rollBallInGridDirection grid direction
                    else positions

// --------------------------------------------------------------
//STATE LIFECYCLE

let init (): SharedGoalRoll.Model * Cmd<Msg> =
    SharedGoalRoll.initModel, Cmd.none

let update ( msg: Msg ) ( model: SharedGoalRoll.Model ): SharedGoalRoll.Model * Cmd<Msg> =
    match msg, model with
    | RollBall direction, model ->
        match direction with
        | Up ->
            let boardAfterRoll = rollBallInGridDirection model.CurrentGrid Up
            { model with CurrentGrid = boardAfterRoll }, Cmd.ofMsg CheckSolution
        | Down ->
            let boardAfterRoll = rollBallInGridDirection model.CurrentGrid Down
            { model with CurrentGrid = boardAfterRoll }, Cmd.ofMsg CheckSolution
        | Left ->
            let boardAfterRoll = rollBallInGridDirection model.CurrentGrid Left
            { model with CurrentGrid = boardAfterRoll }, Cmd.ofMsg CheckSolution
        | Right ->
            let boardAfterRoll = rollBallInGridDirection model.CurrentGrid Right
            { model with CurrentGrid = boardAfterRoll }, Cmd.ofMsg CheckSolution
    | LoadRound levelIndex, model ->
        let newRound = SharedGoalRoll.loadRound levelIndex
        let newRoundModel : SharedGoalRoll.Model = { 
            LevelIndex = levelIndex
            InitialGrid = newRound
            CurrentGrid = newRound
            BallPositionIndex = SharedGoalRoll.getBallPositionIndex newRound
            GoalPositionIndex = SharedGoalRoll.getGoalPositionIndex newRound
            GameState = Playing
        }
        newRoundModel, Cmd.none
    | ResetRound, model ->
        let resetRound = model.InitialGrid
        { model with 
            CurrentGrid = resetRound;
            BallPositionIndex = SharedGoalRoll.getBallPositionIndex resetRound
            GameState = Playing
        }, Cmd.none 
    | CheckSolution, model -> 
        if SharedGoalRoll.getBallPositionIndex model.CurrentGrid = model.GoalPositionIndex
            then
                { model with GameState = Won }, Cmd.none
            else
                model, Cmd.none
    | QuitGame, model -> model, Cmd.ofMsg QuitGame

// -------- GOAL ROLL VIEW --------

// content descriptions
let goalRollDescriptions = [
    "- Use the arrows next to the ball in order to roll it in the desired direction."
    "- Travels in straight lines and stops when it hits a wall or blocked tile."
    "- There must be space for the movement arrow in order to roll."
    "- Have the ball stop on the goal to win."
]
// external links
let sourceCodeLinks = [
    "Shared Model", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Shared/Shared.fs"
    "Shared View", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Shared/Index.fs"
    "Client Logic", "https://raw.githubusercontent.com/SeanWilken/WilkenWeb/master/src/Client/Modules/Portfolio/Games/GoalRoll/Index.fs"
]
// content selection controls
let gameControls = [ 
    "Reset Round", ResetRound
    "Level 0", LoadRound 0
    "Level 1", LoadRound 1
    "Level 2", LoadRound 2
    "Level 3", LoadRound 3
]

//header
let goalRollHeader dispatch =
    SharedViewModule.sharedModalHeaderControls "Goal Roll" QuitGame dispatch

// left content
let goalRollLeftModal =
    SharedViewModule.sharedModalLeft goalRollDescriptions sourceCodeLinks

// main content
// // Assign positions to view elements
let goalRollRowCreator ( rowPositions: LaneObject list ) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                | Blocker -> 
                    Tile.child [] [ Box.box' [ Props [ ClassName "blockerLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Blocker.png" ] ] ] ]
                | Ball ->
                    Tile.child [] [ Box.box' [ Props [ ClassName "ballLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Ball.png" ] ] ] ]
                | Goal ->
                    Tile.child [] [ Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Flag.png" ] ] ] ]
                | Heart ->
                    Tile.child [] [ Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Heart.png"] ] ] ]
                | LaneLock ->
                    Tile.child [] [ Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Lock.png"] ] ] ]
                | LaneKey ->
                    Tile.child [] [ Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Key.png"] ] ] ]
                | Bomb ->
                    Tile.child [] [ Box.box' [ Props [ ClassName "genericLaneObject" ] ] [ Image.image [] [ img [ Src "./imgs/icons/Bomb.png"] ] ] ]
                | MoveArrow Up -> 
                    Tile.child [] [ Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Up |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/UpArrow.png"] ] ] ]
                | MoveArrow Down -> 
                    Tile.child [] [ Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Down |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/DownArrow.png"] ] ] ]
                | MoveArrow Left -> 
                    Tile.child [] [ Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Left |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/LeftArrow.png"] ] ] ]
                | MoveArrow Right -> 
                    Tile.child [] [ Box.box' [ Props [ ClassName "movementArrowLaneObject"; OnClick( fun _ -> RollBall Right |> dispatch ) ] ] [ Image.image [] [ img [ Src "./imgs/icons/RightArrow.png"] ] ] ]
                | _ -> 
                    Tile.child [] [ Box.box' [ Props [ ClassName "genericLaneObject" ] ] [] ]
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
    Container.container [] [ for row in gridRows do goalRollRowCreator row dispatch ]
// modal content container
let goalRollModalContent ( model : SharedGoalRoll.Model ) dispatch =
    match model.GameState with 
    | Playing -> goalRollLevelCreator model dispatch
    | Won -> div [ ClassName "levelCompletedCard" ] [ str "Level Completed!!!" ]

// right content controls
let goalRollModalRight dispatch =
    ( SharedViewModule.sharedModalRight gameControls dispatch )

// main view
let view ( model : SharedGoalRoll.Model ) dispatch =
    SharedViewModule.sharedModal ( goalRollHeader dispatch ) ( goalRollLeftModal ) ( goalRollModalContent model dispatch ) ( goalRollModalRight dispatch )
                           
// --------------------------------