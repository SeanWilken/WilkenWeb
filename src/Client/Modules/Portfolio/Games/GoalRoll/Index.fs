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

type Msg =
    | ResetRound
    | LoadRound of int
    | RollBall of GridGame.MovementDirection
    | CheckSolution
    | QuitGame

let init (): SharedGoalRoll.Model * Cmd<Msg> =
    SharedGoalRoll.initModel, Cmd.none

// --------------------------------------------------------------
//STATE LIFECYCLE
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
        let newRound = loadRound levelIndex
        let newRoundModel = { 
            LevelIndex = levelIndex;
            InitialGrid = newRound;
            CurrentGrid = newRound;
            BallPositionIndex = getBallPositionIndex newRound;
            GoalPositionIndex = getGoalPositionIndex newRound;
            GameState = Playing;
        }
        newRoundModel, Cmd.none
    | ResetRound, model ->
        let resetRound = model.InitialGrid
        { model with 
            CurrentGrid = resetRound;
            BallPositionIndex = getBallPositionIndex resetRound;
            GameState = Playing;
        }, Cmd.none 
    | CheckSolution, model -> 
        if getBallPositionIndex model.CurrentGrid = model.GoalPositionIndex
            then
                { model with GameState = Won }, Cmd.none
            else
                model, Cmd.none
    | QuitGame, model -> model, Cmd.ofMsg QuitGame

// -------- GOAL ROLL VIEW --------
// GAME CONTROLS
let levelSelector currentLevel allLevels dispatch =
    Level.item [ Level.Item.HasTextCentered; ] [
        Level.item [] [ p [] [ str "Level Select:" ] ]
        for level in allLevels do
            if level = currentLevel then 
                Level.item [] [ p [] [ str ( sprintf "Lvl %i" level ) ] ]
            else 
                Level.item [] [ 
                    a [ OnClick ( fun _ -> LoadRound level |> dispatch ) ] [ str ( sprintf "Lvl %i" level ) ]
                ]
    ]
let goalRollHeaderControls currentLevelIndex availLevels dispatch =
    Container.container [ Container.Props [ ClassName "gameGridControlBar" ] ] [
        Level.level [] [
            levelSelector currentLevelIndex availLevels dispatch
            Level.item [] [ a [ OnClick ( fun _ -> ResetRound |> dispatch ); ] [ str "Reset Round" ] ]
            // Level.item [] [ a [ OnClick(fun _ -> RandRound |> dispatch ); ] [ str "Random Round" ] ]
            // Level.item [] [ a [ OnClick(fun _ -> RewindMove |> dispatch ); ] [ str "Undo Turn" ] ]
        ]
    ]
// SHARABLE (?)
// Given a list of laneObjects, return as row of tiles with object
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
// SHARABLE (?)
// // Given a Goal Roll Level, create the Game Board via rows
let goalRollLevelCreator goalRollModel dispatch =
    let positions = goalRollModel.CurrentGrid
    // this also needs to handle if the flag is in an arrows position?
    // VERTICAL MOVEMENTS
    let gridWithUpArrow = gridWithMovementArrow positions Up
    let gridWithDownArrow = gridWithMovementArrow gridWithUpArrow Down
    // HORIZONTAL MOVEMENTS
    let gridWithLeftArrow = gridWithMovementArrow gridWithDownArrow Left
    let gridWithRightArrow = gridWithMovementArrow gridWithLeftArrow Right
    // MAKE SURE GOAL IS PLACED IF ROLLED OVER // UGLY, REWORK
    let gameGrid = 
        if getBallPositionIndex positions <> goalRollModel.GoalPositionIndex
            then 
                if ( getGoalPositionIndex positions = -1 ) then gridWithGoal gridWithRightArrow goalRollModel.GoalPositionIndex
                else gridWithRightArrow
        else gridWithRightArrow
    // positions as rows
    // BAKE GRID DIMENSION INTO MODEL IF VAR, FUNC IF STATIC, SIZE???
    let gridRows = getPositionsAsRows gameGrid 8
    Container.container [ Container.Props [ ClassName "gameGridContainer" ] ] [
        for row in gridRows do
            goalRollRowCreator row dispatch
    ]
// Main view function
let view model dispatch =
    Container.container [] [
        SharedModule.backToGallery QuitGame dispatch
        Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
            Container.container [] [
                h1 [] [ str "Goal Roll" ]
                p [] [ str "- Roll the ball to the goal, travels in straight lines and stops when it hits a wall or blocked tile." ]
                p [] [ str "- Travels in straight lines and stops when it hits a wall or blocked tile." ]
                p [] [ str "- Use the arrows next to the ball in order to roll it in the desired direction." ]
            ]
        ]
        goalRollHeaderControls model.LevelIndex [ 0..3 ] dispatch
        match model.GameState with
        | Playing ->
            goalRollLevelCreator model dispatch
        | Won ->
            // Should be mroe exciting
            // Have options to 
                //replay this level
                //load the (next||prev||rand) level
                //share this level // share this game
            div [ ClassName "levelCompletedCard" ] [ str "Level Completed!!!" ]
    ]
                           
// --------------------------------