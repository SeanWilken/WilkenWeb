//ClientApp
module Index

open Elmish
open Shared
open PageRouter


// Section Items
open Welcome
open AboutSection
open Portfolio
open Contact

// TODO
// GLOBAL STATE -> NEED TO BE ABLE TO TRACK INDIVIDUAL STATE ITEMS SUCH AS LEVELS COMPLETED? -> local variables stored in temp file or global state.
// STORE: -> IMPLEMENT PRINTFUL API
// https://www.printful.com/api
// https://www.printful.com/docs
// HELPER FUNCS?

// Represents submodule msg's to be passed along the elmish update dispatch loop
type WebAppMsg =
    | WelcomeMsg of Welcome.Msg
    | AboutMsg of AboutSection.Msg // FOR MODAL INTRODUCTION // TEMPORARY
    | PortfolioMsg of Portfolio.Msg
    | SwitchToOtherApp of string // please wrap me
    | LoadPage of Page
    | ErrorMsg of exn// WIP


// PAGE ROUTER
// if had to hit server
// ------------
// implementation of IPageApi
// this uses the route builder!!
// let pageApi =
//     Remoting.createApi()
//     |> Remoting.withRouteBuilder Route.builder
//     |> Remoting.buildProxy<IPageApi>
// let loadPage path = async {
//     let! page = pageApi.GetPage path
//     return page
// }

// the init has to have same signature and be called from the index html
let init (path: PageRouter.Page option) : SharedWebAppModels.Model * Cmd<WebAppMsg> =
    PageRouter.urlUpdate path SharedWebAppModels.Model.Welcome

let update (msg: WebAppMsg) (model: SharedWebAppModels.Model): SharedWebAppModels.Model * Cmd<WebAppMsg> =
    match msg, model with
    
    // WELCOME PAGE
    | WelcomeMsg msg, SharedWebAppModels.Model.Welcome ->
        model, Cmd.ofMsg (LoadPage Page.About)

    // ABOUT PAGE
    | AboutMsg (NextSection),  model ->
        model, Cmd.ofMsg (LoadPage (Page.Portfolio Landing))
    | AboutMsg (PreviousSection), model ->
        model, Cmd.ofMsg (LoadPage (Page.Welcome))
    | AboutMsg msg, SharedWebAppModels.Model.AboutSection (model) ->
        let thing, com = AboutSection.update msg model
        SharedWebAppModels.AboutSection thing, Cmd.none

    // PORTFOLIO PAGE
    | PortfolioMsg msg, SharedWebAppModels.Portfolio model ->
        match msg, model with
        // PORTFOLIO GALLERY SUB MODULE
        | LoadSection (SharedPortfolioGallery.PortfolioGallery), _ -> 
            SharedWebAppModels.Portfolio model, Cmd.ofMsg (LoadPage (Page.Portfolio Landing))
        // CODE GALLERY SUB MODULE
        | LoadSection (SharedPortfolioGallery.CodeGallery (SharedCodeGallery.CodeGallery)), _ ->
           SharedWebAppModels.Portfolio model, Cmd.ofMsg (LoadPage (Page.Portfolio (Code (CodeSection.Landing))))
        | CodeGalleryMsg CodeGallery.Msg.BackToPortfolio, _ -> 
            SharedWebAppModels.Portfolio model, Cmd.ofMsg (LoadPage (Page.Portfolio Landing))
        // ART GALLERY SUB MODULE
        | LoadSection (SharedPortfolioGallery.DesignGallery ({CurrentPieceIndex = 0}) ), _ ->
           SharedWebAppModels.Portfolio model, Cmd.ofMsg (LoadPage (Page.Portfolio (Design)))
        | ArtGalleryMsg ArtGallery.Msg.BackToPortfolio, _ -> 
            SharedWebAppModels.Portfolio model, Cmd.ofMsg (LoadPage (Page.Portfolio Landing))
        // default case
        | msg, model ->
            let portfolioModel, com = Portfolio.update msg model
            SharedWebAppModels.Portfolio portfolioModel, Cmd.map PortfolioMsg com
    
    // HEADER NAV ROUTING
    | SwitchToOtherApp string, _ ->
        // handles page route for top level
        match string with
        | "AboutSection" -> model, Cmd.ofMsg (LoadPage Page.About)
        | "Portfolio" -> model, Cmd.ofMsg (LoadPage (Page.Portfolio(Landing)))
        | "Contact" -> model, Cmd.ofMsg (LoadPage Page.Contact)
        | "Welcome"
        | _ ->
            model, Cmd.ofMsg (LoadPage Page.Welcome)
    | LoadPage page, model ->
        urlUpdate (Some page) model
    | _ -> model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

open FSharp.Reflection

//TODO:
    // HEADER TITLE AND ITEMS SHOULDN'T SHIFT AROUND WHEN HOVERING MENU SELECTION ITEMS

// Union cases of the different web app sub modules, in order to create elements
// and reference the union type in code.
// let contentAreas = FSharpType.GetUnionCases typeof<WebAppModel>
let contentAreas = FSharpType.GetUnionCases typeof<SharedWebAppModels.Model>

// Blurs out the header depending on the web app's current model
// If the user is inside of something that should be getting all
// of their attention (such as a code example being played, or some art being viewed)
let headerBlurSelector model = 
    match model with
    | SharedWebAppModels.Contact ->
        Container.Props [ ClassName "blurContent" ]
    | SharedWebAppModels.Portfolio model ->
        match model with
        | SharedPortfolioGallery.PortfolioGallery ->
            Container.Props [ ClassName "halfPaddedContainer" ]
        | _ -> 
            Container.Props [ ClassName "blurContent" ]
    | _ ->
        Container.Props [ ClassName "halfPaddedContainer" ]

// Web App Header Nav content 
let headerContent (model: SharedWebAppModels.Model) dispatch =
    Container.container [ headerBlurSelector model ] [
        Level.level [] [
            Level.item [] [
                Image.image [Image.Is128x128] [ img [ Src "./imgs/icons/Logo blue.png"; ] ]
                p [ ClassName "headerTitle" ] [ str "Sean Wilken" ]
            ]
            Level.item [ ] [
                Columns.columns [ Columns.Props [ Style[ FlexDirection "column"; Padding 10; FontFamily "Exo"; FontStyle "italic"; TextAlign TextAlignOptions.Center]; ] ] [ // STYLE THIS??
                    for contentArea in contentAreas do
                        let areaName = contentArea.Name
                        let currentContentSection = 
                            match model with
                            | SharedWebAppModels.Welcome -> "Welcome"
                            | SharedWebAppModels.AboutSection _ -> "AboutSection"
                            | SharedWebAppModels.Portfolio _ -> "Portfolio"
                            | SharedWebAppModels.Contact -> "Contact"
                        let areaStyle = if currentContentSection = areaName then [ Color "#69A69A"; FontWeight 600] else [ Color "#FF2843";] // is-normal-text vs is-danger class refactor
                        a [ ClassName "headerNavLinks"; Style areaStyle; OnClick(fun _ -> SwitchToOtherApp areaName |> dispatch ) ] [
                            str areaName
                        ]
                ]
            ]
        ]
    ]

let view (model : SharedWebAppModels.Model) (dispatch : WebAppMsg -> unit) =
    div [] [
        headerContent model dispatch
        Container.container [ Container.Props [ Style [ Padding 15; ] ] ] [// style hack?
            match model with
            | SharedWebAppModels.AboutSection model -> AboutSection.view model (AboutMsg >> dispatch)
            | SharedWebAppModels.Welcome -> Welcome.view (WelcomeMsg >> dispatch)
            | SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery -> Portfolio.view SharedPortfolioGallery.PortfolioGallery (PortfolioMsg >> dispatch)
            | SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery model) -> Portfolio.view (SharedPortfolioGallery.DesignGallery model) (PortfolioMsg >> dispatch)
            | SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery model) -> Portfolio.view (SharedPortfolioGallery.CodeGallery model) (PortfolioMsg >> dispatch)
            | SharedWebAppModels.Contact -> Contact.view
        ]
    ]