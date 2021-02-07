module Portfolio

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared

// TODO: RESUME SECTION

type Msg =
    | LoadSection of SharedPortfolioGallery.Model
    | ArtGalleryMsg of ArtGallery.Msg
    | CodeGalleryMsg of CodeGallery.Msg

let init(): SharedPortfolioGallery.Model * Cmd<Msg> =
    SharedPortfolioGallery.PortfolioGallery, Cmd.none

let update ( msg: Msg ) ( model: SharedPortfolioGallery.Model ): SharedPortfolioGallery.Model * Cmd<Msg> =
    match msg, model with
    //PORTFOLIO GALLERY
    | LoadSection SharedPortfolioGallery.PortfolioGallery, _ ->
        SharedPortfolioGallery.PortfolioGallery, Cmd.none
    //ART GALLERY
    | LoadSection ( SharedPortfolioGallery.DesignGallery msg ), _ ->
        SharedPortfolioGallery.DesignGallery msg, Cmd.none
    | ArtGalleryMsg msg, ( SharedPortfolioGallery.DesignGallery model ) -> 
        let artGalleryModel, com = ArtGallery.update msg model
        SharedPortfolioGallery.DesignGallery artGalleryModel, Cmd.map ArtGalleryMsg com
    //CODE GALLERY
    | LoadSection ( SharedPortfolioGallery.CodeGallery msg ), _ ->
        SharedPortfolioGallery.CodeGallery msg, Cmd.none
    | CodeGalleryMsg msg, SharedPortfolioGallery.CodeGallery model ->
        let codeGalleryModel, com = CodeGallery.update msg model
        SharedPortfolioGallery.CodeGallery codeGalleryModel, Cmd.map CodeGalleryMsg com
    | _ -> SharedPortfolioGallery.PortfolioGallery, Cmd.none

// VIEW
let PortfolioHeader =
    Level.level [ Level.Level.Props [ ClassName "portfolioContentCard" ] ] [
        Level.item [] [
            Container.container [] [
                h1 [] [ str "Portfolio" ]
                p [] [ str "Select one of the below to delve deeper into the rabbit hole." ]
            ]
        ]
    ]

let PortfolioSplitView dispatch =
    div [ ClassName "portfolioSectionSelectionContainer" ] [
        Columns.columns [] [
            Column.column [] [
                a [ OnClick ( fun _ -> LoadSection ( SharedPortfolioGallery.CodeGallery SharedCodeGallery.CodeGallery ) |> dispatch ) ] [
                Container.container [ Container.Props [ ClassName "portfolioCodeCard" ] ] [
                        Tile.child [ Tile.Props [ ClassName "contentCardTextBackground" ] ] [ 
                            h1 [] [ str "PLAY OR REVIEW CODE" ]
                            h2 [] [ str "CODE GALLERY" ]
                        ]
                    ]
                ]
            ]
            // TODO PROFESSIONAL RESUME CONDENSED PDF / PAGE VERSION
            Column.column [] [
                a [ OnClick ( fun _ -> LoadSection ( SharedPortfolioGallery.DesignGallery SharedDesignGallery.getInitialModel ) |> dispatch ) ] [
                Container.container [ Container.Props [ ClassName "portfolioDesignCard" ] ] [
                    Tile.child [ Tile.Props [ ClassName "contentCardTextBackground" ] ] [ 
                            h1 [] [ str "CHECK OUT SOME DRAWINGS" ]
                            h2 [] [ str "DESIGN GALLERY" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let view model dispatch =
    div [] [//ClassName "portfolioSectionContainer" 
        match model with
        | SharedPortfolioGallery.PortfolioGallery ->
            div [] [
                PortfolioHeader
                PortfolioSplitView dispatch
            ]
        | SharedPortfolioGallery.DesignGallery model ->
            ArtGallery.view model (ArtGalleryMsg >> dispatch)
        | SharedPortfolioGallery.CodeGallery model ->
            CodeGallery.view model (CodeGalleryMsg >> dispatch)
    ]