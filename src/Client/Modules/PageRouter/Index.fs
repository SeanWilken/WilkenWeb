module PageRouter

// GARBAGE
// I NEED TO BE REVIEWED THOROUGHLY
// THIS IS VERY DIRTY AND FRAGMENTED CURRENTLY:
    // INDEX.FS USES SHARED AND IT'S OWN COMPONENTS
    // NOBODY USES THIS MODULE!!
// EXISTS BUT IS CURRENTLY NOT BEING USED IN THE MAIN WEB APP

open Elmish
open Fable
// open Index
open Portfolio
open Shared

// IS THIS NEEDED, OR LEAVE IN 
// ------------


// URL BROWSER UPDATES
open Elmish.UrlParser
open Elmish.Navigation


// WHERE DO I WANT THESE TO LIVE?

type CodeSection =
    | GoalRoll
    | TileSmash
    | TileSort

// int * int tuple to represent
// GameID * LevelID
    // GAMEID: 1 = GoalRoll
    // GAMEID: 2 = TileSmash
    // GAMEID: 3 = TileSort
type CodeUrl = CodeUrl of CodeSection * int

type AboutSection =
    | General
    | Personal
    | Professional
    | Resume

type PortfolioSection =
    | Code of CodeUrl
    | Design of int // load a design index

type Page =
    | Welcome
    | About
    | Portfolio
    // | About of AboutSection
    // | Portfolio of PortfolioSection
    | Contact

let toPath =
    function
    | Some About ->
        "/about"
    | Some Portfolio ->
        "/portfolio"
    // | About General ->
    //     "/about"
    // | About Personal ->
    //     "/about/personal"
    // | About Professional ->
    //     "/about/professional"
    // | About Resume ->
    //     "/about/resume"
    // | Portfolio (Code (CodeUrl (codeSection, int))) ->
    //     match codeSection, int with
    //     | GoalRoll, int ->
    //         sprintf "/portfolio/goalRoll/%i" int 
    //     | TileSmash, int ->
    //         sprintf "/portfolio/tileSmash/%i" int 
    //     | TileSort, int ->
    //         sprintf "/portfolio/tileSort/%i" int 
    // | Portfolio (Design int) ->
    //     sprintf "/portfolio/design/%i" int
    | Some Contact ->
        "/contact"
    | Some Welcome ->
        "/welcome"
    | _ ->
        ""

// let fromModelToPath =
//     function
//     | SharedWebAppModels.Welcome ->
//         "/welcome"
//     | SharedWebAppModels.AboutSection _ ->
//         "/about"
//     | SharedWebAppModels.Portfolio (SharedPortfolioGallery.PortfolioGallery) ->
//         "/portfolio"
//     | SharedWebAppModels.Portfolio (SharedPortfolioGallery.CodeGallery _) ->
//         "/portfolio/code"
//     | SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery _) ->
//         "/portfolio/designs"
//     // | SharedWebAppModels.Portfolio (SharedPortfolioGallery.DesignGallery _) ->
//     //     "/portfolio/code"
//     // | About General ->
//     //     "/about"
//     // | About Personal ->
//     //     "/about/personal"
//     // | About Professional ->
//     //     "/about/professional"
//     // | About Resume ->
//     //     "/about/resume"
//     // | Portfolio (Code (CodeUrl (codeSection, int))) ->
//     //     match codeSection, int with
//     //     | GoalRoll, int ->
//     //         sprintf "/portfolio/goalRoll/%i" int 
//     //     | TileSmash, int ->
//     //         sprintf "/portfolio/tileSmash/%i" int 
//     //     | TileSort, int ->
//     //         sprintf "/portfolio/tileSort/%i" int 
//     // | Portfolio (Design int) ->
//     //     sprintf "/portfolio/design/%i" int
//     | SharedWebAppModels.Contact ->
//         "/contact"
//     // | _ ->
//     //     "/welcome"

let pageParser : Parser<Page -> Page,_> =
    oneOf
        [
            map Page.Welcome (s "/welcome")
            map Page.About (s "/about")
            map Page.Portfolio (s "/portfolio")
            map Page.Contact (s "/contact")
        ]

let urlParser location = parsePath pageParser location

let urlUpdate (result: Page option) (model: SharedWebAppModels.Model) =
    match result with
    | Some Page.Welcome ->
        SharedWebAppModels.Welcome, Navigation.modifyUrl (toPath (Some Welcome))
    | Some Page.About ->
        SharedWebAppModels.AboutSection SharedAboutSection.getInitialModel, Navigation.modifyUrl (toPath (Some Page.About))
    | Some Page.Portfolio ->
        SharedWebAppModels.Portfolio SharedPortfolioGallery.PortfolioGallery, Navigation.modifyUrl (toPath (Some Page.Portfolio))
    | Some Page.Contact ->
        SharedWebAppModels.Contact, Navigation.modifyUrl (toPath (Some Page.Contact))
    | _ 
    | None ->
        SharedWebAppModels.Welcome, Navigation.modifyUrl (toPath (Some Page.Welcome))



// REVIEW TO UNDERSTAND!!
// ------------
