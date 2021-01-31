//ClientApp
module Index

open Elmish
open Fable.Remoting.Client
open Shared
open PageRouter

// URL BROWSER UPDATES
open Elmish.UrlParser
open Elmish.Navigation

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

// Represents which of the web app's subsections is to be displayed
// Welcome -> not much to see here, a landing page with element to drive along user interaction
// AboutSection -> Overview of the purpose of the web app, in this case some details about it's creator
// Portfolio -> Split view landing page to separate categories from one another at a high level
// Contact -> How to get in touch with the entity the web app represents

// ACTUALLY NEED ONE GLOBAL STATE OBJECT TO PERSIST ANY GAME LEVELS COMPLETED TYPE FUNCTIONALITY!!
// Represents submodule msg's to be passed along the elmish update dispatch loop
type WebAppMsg =
    | WelcomeMsg of Welcome.Msg
    | AboutMsg of AboutSection.Msg // FOR MODAL INTRODUCTION // TEMPORARY
    | PortfolioMsg of Portfolio.Msg
    | SwitchToOtherApp of string // please wrap me
    // | LoadPage of string // WIP
    // | PageLoad of SharedWebAppModels.Model // WIP
    | ErrorMsg of exn// WIP

// TODO CLEAN ME TF UP
// TODO GENERIC ROUTER
// TODO TYPES FOR PAGES?
// TODO HELPER FUNCTIONS?

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
    // PageRouter.urlUpdate path SharedWebAppModels.Model.Welcome
    // PageRouter.urlUpdate path SharedWebAppModels.Model.Welcome
    PageRouter.urlUpdate path SharedWebAppModels.Model.Welcome

let update (msg: WebAppMsg) (model: SharedWebAppModels.Model): SharedWebAppModels.Model * Cmd<WebAppMsg> =
    match msg, model with
    
    | WelcomeMsg msg, SharedWebAppModels.Model.Welcome ->
        // model, Cmd.none
        SharedWebAppModels.AboutSection (SharedAboutSection.getInitialModel), Navigation.newUrl(PageRouter.toPath (Some PageRouter.Page.About)) //(PageRouter.fromModelToPath (SharedWebAppModels.AboutSection SharedAboutSection.getInitialModel)) //

    // TEMPORARY
    | AboutMsg msg, SharedWebAppModels.Model.AboutSection (model) ->
        let thing, com = AboutSection.update msg model
        SharedWebAppModels.AboutSection thing, Cmd.none

    | PortfolioMsg msg, SharedWebAppModels.Portfolio model ->
        match msg, model with

        // PORTFOLIO GALLERY SUB MODULE
        | LoadSection (SharedPortfolioGallery.PortfolioGallery), _ -> 
            SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery, Cmd.none
        
        // CODE GALLERY SUB MODULE
        | LoadSection (SharedPortfolioGallery.CodeGallery (SharedCodeGallery.CodeGallery)), _ ->
           SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery (SharedCodeGallery.CodeGallery)), Cmd.none
        | CodeGalleryMsg CodeGallery.Msg.BackToPortfolio, _ -> 
            SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery, Cmd.none

        // ART GALLERY SUB MODULE
        | LoadSection (SharedPortfolioGallery.DesignGallery ({CurrentPieceIndex = 0}) ), _ ->  // TAKE PLAIN MODEL??? COMPILER COMPLAININ'
           SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery (SharedDesignGallery.getInitialModel)), Cmd.none
        | ArtGalleryMsg ArtGallery.Msg.BackToPortfolio, _ -> 
            SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery, Cmd.none
        // default case, bad hack // please fix this
        | msg, model ->
            let portfolioModel, com = Portfolio.update msg model
            SharedWebAppModels.Portfolio portfolioModel, Cmd.map PortfolioMsg com
    
    | SwitchToOtherApp string, _ ->
        // handles page route for top level
        match string with
        | "AboutSection" -> 
            SharedWebAppModels.AboutSection SharedAboutSection.getInitialModel, Navigation.newUrl(PageRouter.toPath (Some PageRouter.Page.About))
        | "Portfolio" -> 
            SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery, Navigation.newUrl(PageRouter.toPath (Some PageRouter.Page.Portfolio))
        | "Contact" -> 
            SharedWebAppModels.Contact, Navigation.newUrl(PageRouter.toPath (Some PageRouter.Page.Contact))
        | "Welcome"
        | _ ->
            SharedWebAppModels.Welcome, Navigation.newUrl(PageRouter.toPath (Some PageRouter.Page.Welcome))
    // SHOULD USE THIS?
    // | LoadPage page, model ->
        // pageModel, Cmd.none    
    // | PageLoad page, _ ->
    //     // page, Navigation.newUrl (PageRouter.fromModelToPath (page))
    //     page, Navigation.newUrl (PageRouter.toPath (Some page))
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
    | SharedWebAppModels.Portfolio model ->
        match model with
        | SharedPortfolioGallery.PortfolioGallery ->
            Container.Props [ Style [Padding 25;] ] // STYLE THIS??
        | _ -> 
            Container.Props [ ClassName "blurContent"; Style [Padding 25;] ] // STYLE THIS??
    | SharedWebAppModels.Contact -> // REMOVE OR KEEP???
        Container.Props [ ClassName "blurContent"; Style [Padding 25;] ] // STYLE THIS??
    | _ ->
        Container.Props [ Style [Padding 25;] ] // STYLE THIS??

// Web App Header Nav content 
let headerContent (model: SharedWebAppModels.Model) dispatch =
    Container.container [ headerBlurSelector model ] [
        Level.level [] [
            Level.item [] [
                p [ ClassName "headerTitle" ] [ str "Sean Wilken" ] // DIFFERENT FONT STYLE
            ]
            Level.item [ ] [
                Columns.columns [ Columns.Props [ Style[ FlexDirection "column"; Padding 10; FontFamily "Exo"; FontStyle "italic"; TextAlign TextAlignOptions.Center]; ] ] [ // STYLE THIS??
                    for contentArea in contentAreas do
                        let areaName = contentArea.Name
                        let currentContentSection = 
                            match model with
                            | SharedWebAppModels.Welcome -> "Welcome"
                            | SharedWebAppModels.AboutSection model -> "AboutSection"
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
        Container.container [ Container.Props [Style [ Padding 15; ] ] ] [ // STYLE THIS??
            match model with
            // TEMPORARY
            | SharedWebAppModels.AboutSection model -> AboutSection.view model (AboutMsg >> dispatch)
            | SharedWebAppModels.Welcome -> Welcome.view (WelcomeMsg >> dispatch)
            | SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery -> Portfolio.view SharedPortfolioGallery.PortfolioGallery (PortfolioMsg >> dispatch)
            | SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery model) -> Portfolio.view (SharedPortfolioGallery.DesignGallery model) (PortfolioMsg >> dispatch)
            | SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery model) -> Portfolio.view (SharedPortfolioGallery.CodeGallery model) (PortfolioMsg >> dispatch)
            | SharedWebAppModels.Contact -> Contact.view
        ]
    ]