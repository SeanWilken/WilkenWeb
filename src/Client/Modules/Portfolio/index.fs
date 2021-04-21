module Portfolio

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared

// TODO: RESUME SECTION
type Msg =
    | LoadSection of SharedWebAppViewSections.AppSection
    | ArtGalleryMsg of ArtGallery.Msg
    | CodeGalleryMsg of CodeGallery.Msg

let portfolioHeaderTitle = "Portfolio"
let portfolioHeaderBlurbs = [ "...where to begin..." ]

let init(): SharedPortfolioGallery.Model * Cmd<Msg> =
    SharedPortfolioGallery.PortfolioGallery, Cmd.none

let update ( msg: Msg ) ( model: SharedPortfolioGallery.Model ): SharedPortfolioGallery.Model * Cmd<Msg> =
    match msg, model with

    //PORTFOLIO GALLERY
    | LoadSection SharedWebAppViewSections.AppSection.PortfolioAppLandingView , _ ->
        SharedPortfolioGallery.PortfolioGallery, Cmd.none
    //ART GALLERY
    | LoadSection SharedWebAppViewSections.AppSection.PortfolioAppDesignView , _ ->
        SharedPortfolioGallery.DesignGallery (SharedDesignGallery.getInitialModel), Cmd.none
    | ArtGalleryMsg msg, ( SharedPortfolioGallery.DesignGallery model ) -> 
        let artGalleryModel, com = ArtGallery.update msg model
        SharedPortfolioGallery.DesignGallery artGalleryModel, Cmd.map ArtGalleryMsg com
    //CODE GALLERY
    | LoadSection SharedWebAppViewSections.AppSection.PortfolioAppCodeView , _ ->
        SharedPortfolioGallery.CodeGallery (SharedCodeGallery.getInitialModel), Cmd.none
    | CodeGalleryMsg msg, SharedPortfolioGallery.CodeGallery model ->
        let codeGalleryModel, com = CodeGallery.update msg model
        SharedPortfolioGallery.CodeGallery codeGalleryModel, Cmd.map CodeGalleryMsg com
    | _ -> SharedPortfolioGallery.PortfolioGallery, Cmd.none

type PortfolioSplitCardStyle =
    | SplitCodeCard
    | SplitDesignCard

let styleCardBySplitStyle cardStyle =
    match cardStyle with
    | SplitCodeCard -> "CODE", "generalPortfolioCodeCard"
    | SplitDesignCard -> "DESIGN", "generalPortfolioDesignCard"

let portfolioChildSplitTile cardStyle msg descrip dispatch =
    let title, style = styleCardBySplitStyle cardStyle
    Tile.child [ Tile.IsVertical ] [
        a [ OnClick ( fun _ -> msg |> dispatch ) ] [
            div [ ClassName style ] [
                div [ ClassName "generalContentCardTextBackground" ] [ h1 [] [ str title ] ]
            ]  
        ]
        div [ ClassName "generalContentCard" ] [ h2 [] [ str descrip ] ]
    ]

let view model dispatch =
    div [] [
        match model with
        | SharedPortfolioGallery.PortfolioGallery ->
            div [] [
                SharedViewModule.sharedSplitView
                    ( SharedViewModule.sharedSplitHeader portfolioHeaderTitle portfolioHeaderBlurbs )
                    ( portfolioChildSplitTile ( SplitCodeCard ) ( LoadSection SharedWebAppViewSections.AppSection.PortfolioAppCodeView ) "Play some games & link to read their github gists." dispatch )
                    ( portfolioChildSplitTile ( SplitDesignCard ) ( LoadSection SharedWebAppViewSections.AppSection.PortfolioAppDesignView ) "Some drawings and designs I've done, or making." dispatch )
            ]
        | SharedPortfolioGallery.DesignGallery model ->
            ArtGallery.view model ( ArtGalleryMsg >> dispatch )
        | SharedPortfolioGallery.CodeGallery model ->
            CodeGallery.view model ( CodeGalleryMsg >> dispatch )
    ]