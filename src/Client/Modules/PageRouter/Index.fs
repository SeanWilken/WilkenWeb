module PageRouter

open Elmish
open Shared

// URL BROWSER UPDATES
open Elmish.UrlParser
open Elmish.Navigation

type CodeSection =
    | Landing
    | GoalRoll
    | TileSmash
    | TileSort

type PortfolioSection =
    | Landing
    | Code of CodeSection
    | Design //of int // load a design index

type Page =
    | Welcome
    | About
    | Portfolio of PortfolioSection
    | Contact

let checkLevelIsValid levelInt ceiling floor =
    if levelInt > ceiling then ceiling
    elif levelInt < floor then floor
    else levelInt

let toPath =
    function
    | Some About -> "/about"
    | Some ( Portfolio ( Code ( code ) ) ) -> 
        // match code with
        match code with
        | GoalRoll ->
            sprintf "/portfolio-goalRoll" 
        | TileSmash ->
            sprintf "/portfolio-tileSmash" 
        | TileSort ->
            sprintf "/portfolio-tileSort"
        | CodeSection.Landing -> "/portfolio-code"
    | Some ( Portfolio ( Design ) ) ->
        sprintf "/portfolio-design"
    | Some ( Portfolio _ )
    | Some ( Portfolio Landing ) -> "/portfolio"
    | Some Contact -> "/contact"
    | None
    | Some Welcome -> "/welcome"

// router use combinators for better structured paths
let pageParser : Parser< Page -> Page,_ > =
    oneOf
        [
            map Page.Welcome ( s "welcome" )
            map Page.About ( s "about" )
            map ( Page.Portfolio Landing) ( s "portfolio" )
            map ( Page.Portfolio ( Code ( CodeSection.Landing ) ) ) ( s "portfolio-code" )
            map ( Page.Portfolio Design ) ( s "portfolio-design" )
            map ( Page.Portfolio ( Code ( CodeSection.GoalRoll ) ) ) ( s "portfolio-goalRoll" )
            map ( Page.Portfolio ( Code ( CodeSection.TileSort ) ) ) ( s "portfolio-tileSort" )
            map ( Page.Portfolio ( Code ( CodeSection.TileSmash ) ) ) ( s "portfolio-tileSmash" )
            map Page.Contact ( s "contact" )
        ]

let urlParser location = 
    printfn "url is: %A" location
    parsePath pageParser location

let urlUpdate ( result: Page option ) ( model: SharedWebAppModels.Model ) =
    match result with
    | Some Page.About ->
        SharedWebAppModels.AboutSection SharedAboutSection.getInitialModel, Navigation.newUrl ( toPath ( Some About ) )
    | Some ( Page.Portfolio ( Code ( CodeSection.Landing ) ) ) ->
        SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery SharedCodeGallery.CodeGallery), Navigation.newUrl (toPath (Some (Portfolio (Code (CodeSection.Landing)))))
    | Some ( Page.Portfolio ( Code ( CodeSection.GoalRoll ) ) ) ->
        SharedWebAppModels.Portfolio ( SharedPortfolioGallery.CodeGallery ( SharedCodeGallery.GoalRoll SharedGoalRoll.initModel ) ), Navigation.newUrl ( toPath ( Some ( Portfolio ( Code ( CodeSection.GoalRoll ) ) ) ) )
    | Some ( Page.Portfolio ( Code ( CodeSection.TileSort ) ) ) ->
        SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery ( SharedCodeGallery.TileSort SharedTileSort.initModel ) ), Navigation.newUrl ( toPath ( Some ( Portfolio ( Code ( CodeSection.TileSort ) ) ) ) )
    | Some ( Page.Portfolio ( Code ( CodeSection.TileSmash ) ) ) ->
        SharedWebAppModels.Portfolio ( SharedPortfolioGallery.CodeGallery ( SharedCodeGallery.TileSmash SharedTileSmash.initModel ) ), Navigation.newUrl ( toPath ( Some ( Portfolio ( Code ( CodeSection.TileSmash ) ) ) ) )
    | Some ( Page.Portfolio Design ) ->
        SharedWebAppModels.Portfolio ( SharedPortfolioGallery.DesignGallery SharedDesignGallery.getInitialModel ), Navigation.newUrl ( toPath ( Some ( Portfolio Design ) ) )
    | Some ( Page.Portfolio _ ) ->
        SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery, Navigation.newUrl ( toPath ( Some ( Portfolio Landing ) ) )
    | Some Page.Contact ->
        SharedWebAppModels.Contact, Navigation.newUrl ( toPath ( Some Contact ) )
    // SHOULD BE A 404 OR ERROR PAGE
    | None
    | Some Page.Welcome ->
        SharedWebAppModels.Welcome, Navigation.newUrl ( toPath ( Some Welcome ) )