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
    | AboutMsg of AboutSection.Msg
    | PortfolioMsg of Portfolio.Msg
    | SwitchToOtherApp of SharedWebAppViewSections.AppSection
    | LoadPage of Page
    | ErrorMsg of exn // WIP?

// DIRECTORY PAGE, DIRECT LINKS FROM ANY MODEL VIEW TO ANOTHER. MODAL VIEW?

// the init has to have same signature and be called from the index html
let init ( path: PageRouter.Page option ) : SharedWebAppModels.Model * Cmd<WebAppMsg> =
    PageRouter.urlUpdate path SharedWebAppModels.Model.Welcome

let update ( msg: WebAppMsg ) ( model: SharedWebAppModels.Model ): SharedWebAppModels.Model * Cmd<WebAppMsg> =
    match msg, model with
    
    // WELCOME PAGE
    | WelcomeMsg ( Welcome.SwitchSection appSection ), SharedWebAppModels.Model.Welcome ->
        model, Cmd.ofMsg ( SwitchToOtherApp appSection )

    // ABOUT PAGE
    | AboutMsg ( AboutSection.SwitchSection appSection ), model ->
        model, Cmd.ofMsg (SwitchToOtherApp appSection)
    | AboutMsg msg, SharedWebAppModels.Model.About ( model ) ->
        let updateModel, com = AboutSection.update msg model
        SharedWebAppModels.About updateModel, Cmd.none

    // PORTFOLIO PAGE
    | PortfolioMsg msg, SharedWebAppModels.Portfolio model ->
        match msg, model with
        // PORTFOLIO GALLERY SUB MODULE
        | LoadSection ( SharedWebAppViewSections.AppSection.PortfolioAppLandingView ), _ ->
            SharedWebAppModels.Portfolio model, Cmd.ofMsg ( LoadPage ( Page.Portfolio Landing ) )
        // CODE GALLERY SUB MODULE
        | LoadSection ( SharedWebAppViewSections.AppSection.PortfolioAppCodeView ), _ ->
           SharedWebAppModels.Portfolio model, Cmd.ofMsg ( LoadPage ( Page.Portfolio ( Code ( CodeSection.Landing ) ) ) )
        | CodeGalleryMsg CodeGallery.Msg.BackToPortfolio, _ -> 
            SharedWebAppModels.Portfolio model, Cmd.ofMsg ( LoadPage ( Page.Portfolio Landing ) )
        // ART GALLERY SUB MODULE
        | LoadSection ( SharedWebAppViewSections.AppSection.PortfolioAppDesignView ), _ ->
           SharedWebAppModels.Portfolio model, Cmd.ofMsg ( LoadPage ( Page.Portfolio ( Design ) ) )
        | ArtGalleryMsg ArtGallery.Msg.BackToPortfolio, _ -> 
            SharedWebAppModels.Portfolio model, Cmd.ofMsg ( LoadPage ( Page.Portfolio Landing ) )
        // default case
        | msg, model ->
            let portfolioModel, com = Portfolio.update msg model
            SharedWebAppModels.Portfolio portfolioModel, Cmd.map PortfolioMsg com
    
    // Page Routing
    | SwitchToOtherApp section, _ ->
        // handles page route for top level
        match section with
        | SharedWebAppViewSections.AppSection.AboutAppView -> model, Cmd.ofMsg ( LoadPage Page.About )
        | SharedWebAppViewSections.AppSection.PortfolioAppLandingView -> model, Cmd.ofMsg ( LoadPage ( Page.Portfolio ( Landing ) ) )
        | SharedWebAppViewSections.AppSection.PortfolioAppCodeView -> model, Cmd.ofMsg ( LoadPage ( Page.Portfolio ( Code ( CodeSection.Landing ) ) ) )
        | SharedWebAppViewSections.AppSection.PortfolioAppDesignView -> model, Cmd.ofMsg ( LoadPage ( Page.Portfolio ( Design ) ) )
        | SharedWebAppViewSections.AppSection.ContactAppView -> model, Cmd.ofMsg ( LoadPage Page.Contact )
        | SharedWebAppViewSections.AppSection.WelcomeAppView
        | _ ->
            model, Cmd.ofMsg ( LoadPage Page.Welcome )

    | LoadPage page, model ->
        urlUpdate ( Some page ) model
    | _ -> model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

open FSharp.Reflection

// takes union case string, returns an app section
let areaStringToAppSection string =
    match string with
    | "About" -> SharedWebAppViewSections.AppSection.AboutAppView
    | "Portfolio" ->  SharedWebAppViewSections.AppSection.PortfolioAppLandingView
    | "Contact" -> SharedWebAppViewSections.AppSection.ContactAppView
    | "Welcome"
    | _ -> SharedWebAppViewSections.AppSection.WelcomeAppView

// returns the current model as an area
let currentWebAppSection model = 
    match model with
    | SharedWebAppModels.Welcome -> SharedWebAppViewSections.AppSection.WelcomeAppView
    | SharedWebAppModels.About _ -> SharedWebAppViewSections.AppSection.AboutAppView
    | SharedWebAppModels.Portfolio _ -> SharedWebAppViewSections.AppSection.PortfolioAppLandingView
    | SharedWebAppModels.Contact -> SharedWebAppViewSections.AppSection.ContactAppView

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
        | SharedPortfolioGallery.PortfolioGallery -> Container.Props []
        | _ -> Container.Props [ ClassName "blurContent" ] //[ Style [ Display DisplayOptions.None ] ] //[ ClassName "blurContent" ]
    | _ -> Container.Props []

// Web App Header Nav content
//TODO: HEADER TITLE AND ITEMS SHOULDN'T SHIFT AROUND WHEN HOVERING MENU SELECTION ITEMS
let headerContent ( model: SharedWebAppModels.Model ) dispatch =
    Columns.columns [ Columns.IsVCentered ] [
        Column.column [] [
            Container.container [ headerBlurSelector model ] [
                Level.level [] [
                    Level.item [] [
                        a [ OnClick ( fun _ -> SwitchToOtherApp SharedWebAppViewSections.AppSection.WelcomeAppView |> dispatch ) ] [
                            Level.item [] [ 
                                Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Flat logo backless.png"; ] ]
                                p [ ClassName "headerTitle" ] [ str "Sean Wilken" ]
                            ]
                        ]
                    ]
                    Level.item [ ] [
                        Columns.columns [ Columns.IsMobile ] [
                            let currentSection = currentWebAppSection model
                            for contentArea in contentAreas do
                                let areaName = contentArea.Name
                                let areaAppSection = areaStringToAppSection areaName
                                let areaClassName = if currentSection = areaAppSection then "selectedNavLink" else "navSectionLink";
                                Column.column [] [ a [ ClassName areaClassName; OnClick( fun _ -> SwitchToOtherApp areaAppSection |> dispatch ) ] [ str areaName ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let view ( model : SharedWebAppModels.Model ) ( dispatch : WebAppMsg -> unit ) =
    div [] [
        headerContent model dispatch
        Container.container [] [
            match model with
            | SharedWebAppModels.About model -> AboutSection.view model ( AboutMsg >> dispatch )
            | SharedWebAppModels.Welcome -> Welcome.view ( WelcomeMsg >> dispatch )
            | SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery -> Portfolio.view SharedPortfolioGallery.PortfolioGallery ( PortfolioMsg >> dispatch )
            | SharedWebAppModels.Portfolio ( SharedPortfolioGallery.DesignGallery model ) -> Portfolio.view ( SharedPortfolioGallery.DesignGallery model ) ( PortfolioMsg >> dispatch )
            | SharedWebAppModels.Portfolio ( SharedPortfolioGallery.CodeGallery model ) -> Portfolio.view ( SharedPortfolioGallery.CodeGallery model ) ( PortfolioMsg >> dispatch )
            | SharedWebAppModels.Contact -> Contact.view
        ]
    ]