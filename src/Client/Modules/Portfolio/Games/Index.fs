module CodeGallery

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared

type GallerySection =
    | Gallery
    | GoalRoll
    | TileSort
    | TileTap

type Msg =
    | BackToPortfolio
    | LoadSection of GallerySection
    | GoalRollMsg of GoalRoll.Msg
    | TileTapMsg of TileTap.Msg
    | TileSortMsg of TileSort.Msg

let init(): SharedCodeGallery.Model * Cmd<Msg> =
    SharedCodeGallery.CodeGallery, Cmd.none

let update ( msg: Msg ) ( model: SharedCodeGallery.Model ): SharedCodeGallery.Model * Cmd<Msg> =
    match msg, model with
    // GALLERY
    | LoadSection Gallery, _ ->
        SharedCodeGallery.CodeGallery, Cmd.none
    // GOAL ROLL
    | LoadSection GoalRoll, _ ->
        let goalRollModel, com = GoalRoll.init()
        SharedCodeGallery.GoalRoll  ( goalRollModel ), Cmd.map GoalRollMsg com
    | GoalRollMsg GoalRoll.Msg.QuitGame, _-> // any model ?? was Goal Roll
        SharedCodeGallery.CodeGallery, Cmd.none
    | GoalRollMsg msg, SharedCodeGallery.GoalRoll model ->
        let goalRollModel, com = GoalRoll.update msg model
        SharedCodeGallery.GoalRoll goalRollModel, Cmd.map GoalRollMsg com
    // TILE TAP
    | LoadSection TileTap, _ ->
        let tileSmashModel, com = TileTap.init()
        SharedCodeGallery.TileTap tileSmashModel, Cmd.map TileTapMsg com
    | TileTapMsg TileTap.Msg.QuitGame, SharedCodeGallery.TileTap model ->
        // kill dispatch interval
        TileTap.update (TileTap.Msg.ExitGameLoop) model |> ignore 
        SharedCodeGallery.CodeGallery, Cmd.none
    | TileTapMsg msg, SharedCodeGallery.TileTap model ->
        let tileSmashModel, com = TileTap.update msg model
        SharedCodeGallery.TileTap tileSmashModel, Cmd.map TileTapMsg com
    // TILE SORT
    | LoadSection TileSort, _ ->
        let tileSortModel, com = TileSort.init()
        SharedCodeGallery.TileSort tileSortModel, Cmd.map TileSortMsg com
    | TileSortMsg TileSort.Msg.QuitGame, _ -> // any model ?? was Tile Sort -- issue?
        SharedCodeGallery.CodeGallery, Cmd.none
    | TileSortMsg msg, SharedCodeGallery.TileSort model ->
        let tileSortModel, com = TileSort.update msg model
        SharedCodeGallery.TileSort tileSortModel, Cmd.map TileSortMsg com
    // DEFAULT
    | _, _ -> 
        model, Cmd.none

let CodeGalleryHeader =
    Container.container [ Container.Props [ ClassName "viewTitleCard" ] ] [
        Container.container [] [
            h1 [] [ str "Code Gallery" ]
            h2 [] [ str "Select one of the below to try out or view the source code." ]
        ]
    ]

let makeCodeGalleryEntryItem title description msg dispatch =
    Container.container [ Container.Props [ ClassName "paddedContainer" ] ] [
        Columns.columns [ Columns.IsCentered ] [
            Column.column [ Column.Width ( Screen.All, Column.Is10 ) ] [
                a [ OnClick ( fun _ -> msg |> dispatch ) ] [ 
                    div [ ClassName "selectionTile"] [
                        h1 [] [ str title ]
                        p [] [ str description ] 
                    ]
                ]
            ]
        ]
    ]

let tileSortSelection dispatch =
    makeCodeGalleryEntryItem 
        "Tile Sort"
        "Arrange the tiles in the correct order, with the missing number being the empty."
        (LoadSection TileSort)
        dispatch

let tileTapSelection dispatch =
    makeCodeGalleryEntryItem 
        "Tile Tap"
        "Tap to smash as many tiles as you can while avoiding bombs."
        (LoadSection TileTap)
        dispatch

let goalRollSelection dispatch =
    makeCodeGalleryEntryItem 
        "Goal Roll"
        "Roll the ball in straight line movements to the goal."
        (LoadSection GoalRoll)
        dispatch


let view model dispatch =
    Container.container [] [
        match model with
        | SharedCodeGallery.CodeGallery ->
            Container.container [] [
                SharedViewModule.backToGallery BackToPortfolio dispatch
                CodeGalleryHeader
                goalRollSelection dispatch
                tileSortSelection dispatch
                tileTapSelection dispatch
            ]
        | SharedCodeGallery.GoalRoll model ->
            GoalRoll.view model ( GoalRollMsg >> dispatch )
        | SharedCodeGallery.TileSort model ->
            TileSort.view model ( TileSortMsg >> dispatch )
        | SharedCodeGallery.TileTap model ->
            TileTap.view ( model ) ( TileTapMsg >> dispatch )
    ]