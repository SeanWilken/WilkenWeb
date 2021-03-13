module Portfolio

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
    Tile.ancestor [] [
        Tile.parent [] [
            Tile.child [ Tile.Size Tile.Is12 ] [
                    div [ ClassName "viewTitleCard" ] [
                        h1 [] [ str "Portfolio" ]
                        h2 [] [ str "...where to begin..." ]
                    ]
            ]
        ]
    ]

let PortfolioSplitView dispatch =
    div [] [
        Tile.ancestor [] [
            Tile.parent [ Tile.Size Tile.Is6 ] [
                Tile.child [] [
                    a [ OnClick ( fun _ -> LoadSection ( SharedPortfolioGallery.CodeGallery SharedCodeGallery.CodeGallery ) |> dispatch ) ] [
                        div [ ClassName "portfolioCodeCard" ] [
                            div [ ClassName "contentCardTextBackground" ] [
                                h1 [] [ str "CODE" ]
                                h2 [] [ str "CODE" ]
                            ]
                        ]
                    ]
                ]
            ]
            Tile.parent [ Tile.Size Tile.Is6 ] [
                Tile.child [] [
                    a [ OnClick ( fun _ -> LoadSection ( SharedPortfolioGallery.DesignGallery SharedDesignGallery.getInitialModel ) |> dispatch ) ] [
                        div [ ClassName "portfolioDesignCard" ] [
                            div [ ClassName "contentCardTextBackground" ] [ 
                                    h1 [] [ str "DESIGN" ]
                                    h2 [] [ str "DESIGN" ]
                            ]
                        ]       
                    ]       
                ]
            ]
        ]
    ]

let view model dispatch =
    div [] [
        match model with
        | SharedPortfolioGallery.PortfolioGallery ->
            div [] [
                PortfolioHeader
                PortfolioSplitView dispatch
            ]
        | SharedPortfolioGallery.DesignGallery model ->
            ArtGallery.view model ( ArtGalleryMsg >> dispatch )
        | SharedPortfolioGallery.CodeGallery model ->
            CodeGallery.view model ( CodeGalleryMsg >> dispatch )
    ]