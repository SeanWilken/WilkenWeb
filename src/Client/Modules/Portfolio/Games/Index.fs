module CodeGallery

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared

type Msg =
    | BackToPortfolio
    | LoadSection of SharedCodeGallery.Model
    | GoalRollMsg of GoalRoll.Msg
    | TileSmashMsg of TileSmash.Msg
    | TileSortMsg of TileSort.Msg


let init(): SharedCodeGallery.Model * Cmd<Msg> =
    SharedCodeGallery.CodeGallery, Cmd.none

let update (msg: Msg) (model: SharedCodeGallery.Model): SharedCodeGallery.Model * Cmd<Msg> =
    match msg, model with
    // GALLERY
    | LoadSection SharedCodeGallery.CodeGallery, _ ->
        SharedCodeGallery.CodeGallery, Cmd.none
    // GOAL ROLL
    // why am I passing the model as a message?
    | LoadSection (SharedCodeGallery.GoalRoll msg), _ ->
        let goalRollModel, com = GoalRoll.init()
        SharedCodeGallery.GoalRoll (goalRollModel), Cmd.none
    | GoalRollMsg GoalRoll.Msg.QuitGame, SharedCodeGallery.GoalRoll model ->
        SharedCodeGallery.CodeGallery, Cmd.none
    | GoalRollMsg msg, SharedCodeGallery.GoalRoll model ->
        let goalRollModel, com = GoalRoll.update msg model
        SharedCodeGallery.GoalRoll goalRollModel, Cmd.map GoalRollMsg com
    // TILE SMASH
    | LoadSection (SharedCodeGallery.TileSmash msg), _ ->
        let tileSmashModel, com = TileSmash.init()
        // let tileSmashModel, com = TileSmash.init()
        SharedCodeGallery.TileSmash tileSmashModel, Cmd.none
    | TileSmashMsg TileSmash.Msg.QuitGame, SharedCodeGallery.TileSmash model ->
        SharedCodeGallery.CodeGallery, Cmd.none
    // | TileSmashMsg msg, TileSmash model ->
    //     let tileSmashModel, com = TileSmash.update msg model
    //     TileSmash tileSmashModel, Cmd.map TileSmashMsg com
    // TILE SORT
    | LoadSection (SharedCodeGallery.TileSort msg), _ ->
        let tileSortModel, com = TileSort.init()
        SharedCodeGallery.TileSort tileSortModel, Cmd.none
    | TileSortMsg TileSort.Msg.QuitGame, SharedCodeGallery.TileSort model ->
        SharedCodeGallery.CodeGallery, Cmd.none
    | TileSortMsg msg, SharedCodeGallery.TileSort model ->
        let tileSortModel, com = TileSort.update msg model
        SharedCodeGallery.TileSort tileSortModel, Cmd.map TileSortMsg com
    // DEFAULT HACK
    | _, _ -> 
        model, Cmd.none

let CodeGalleryHeader =
    Container.container [ Container.Props [ ClassName "contentCard" ]] [
            h1 [] [ str "Code Gallery" ]
            p [] [ str "Select one of the below to try out or view the source code." ]
    ]

// GENERIC NOT IMPLEMENTED YET
// iterative generation of gallery item cards
let makeCodeGalleryEntryItem title description dispatch =
    Container.container [ Container.Props [ ClassName "columnContainer"] ] [
        Columns.columns [ Columns.IsCentered ] [
            Column.column [Column.Width (Screen.All, Column.Is8)] [
                a [ OnClick(fun _ -> LoadSection (SharedCodeGallery.TileSort SharedTileSort.getInitialBoard) |> dispatch ) ] [ // NEEDS TO GENERATE ITEM LIKE HEADER CONTROLS // NEEDS SAME CALL TO ITEM?
                    div [ ClassName "selectionTile"] [
                        h1 [] [ str title ]
                        p [] [ str description ] 
                    ]
                ]
            ]
        ]
    ]

// TODO
// MAKE THIS INTO A HELPER FUNCTION!!
// ITERATE THROUGH THESE TO GENERATE
// ODD NUMBERS GENERATE LEFT SIDE BUTTON
let CodeGalleryTileSort dispatch =
    Container.container [ Container.Props [ ClassName "columnContainer"] ] [
        Columns.columns [ Columns.IsCentered ] [
            Column.column [Column.Width (Screen.All, Column.Is8)] [
                a [ OnClick(fun _ -> LoadSection (SharedCodeGallery.TileSort SharedTileSort.getInitialBoard) |> dispatch ) ] [
                    div [ ClassName "selectionTile"] [
                        h1 [] [ str "Tile Sort" ]
                        p [] [ str "Arrange the tiles in the correct order, with the missing number being the empty." ] 
                    ]
                ]
            ]
        ]
    ]

let CodeGalleryGoalRoll dispatch =
    Container.container [ Container.Props [ ClassName "columnContainer" ] ] [
        Columns.columns [ Columns.IsCentered ] [
            Column.column [Column.Width (Screen.All, Column.Is7)] [
                a [ OnClick(fun _ -> LoadSection (SharedCodeGallery.GoalRoll SharedGoalRoll.initModel) |> dispatch ) ] [
                    div [ ClassName "selectionTile" ] [ 
                        h1 [] [ str "Goal Roll" ] 
                        p [] [ str "Roll the ball in straight line movements to the goal." ] 
                    ]
                ]
            ]
        ]
    ]


// TODO
// NEW WIP, JUST HOPPED IN
let CodeGalleryTileSmash dispatch =
    Container.container [ Container.Props [ ClassName "columnContainer"] ] [
        Columns.columns [ Columns.IsCentered ] [
            Column.column [Column.Width (Screen.All, Column.Is8)] [
                a [ OnClick(fun _ -> LoadSection (SharedCodeGallery.TileSmash SharedTileSmash.initModel) |> dispatch ) ] [
                    div [ ClassName "selectionTile"] [
                        h1 [] [ str "Tile Smash" ]
                        p [] [ str "Smash as many tiles as you can while avoiding bombs." ] 
                    ]
                ]
            ]
        ]
    ]

let view model dispatch =
    Container.container [] [
        match model with
        | SharedCodeGallery.CodeGallery ->
            Container.container [] [
                SharedModule.backToGallery BackToPortfolio dispatch
                CodeGalleryHeader
                CodeGalleryTileSort dispatch
                CodeGalleryGoalRoll dispatch
                CodeGalleryTileSmash dispatch
            ]
        | SharedCodeGallery.GoalRoll model ->
            GoalRoll.view model (GoalRollMsg >> dispatch)
        | SharedCodeGallery.TileSort model ->
            TileSort.view model (TileSortMsg >> dispatch)
        | SharedCodeGallery.TileSmash model ->
            TileSmash.view (TileSmashMsg >> dispatch) ////model 
    ]