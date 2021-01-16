module PageRouter

// GARBAGE
// I NEED TO BE REVIEWED THOROUGHLY
// THIS IS VERY DIRTY AND FRAGMENTED CURRENTLY:
    // INDEX.FS USES SHARED AND IT'S OWN COMPONENTS
    // NOBODY USES THIS MODULE!!
// EXISTS BUT IS CURRENTLY NOT BEING USED IN THE MAIN WEB APP

open Elmish
open Fable
open Index
open Portfolio

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

// let toPath =
//     function
//     | About ->
//         "/about"
//     | Portfolio ->
//         "/portfolio"
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
//     | Contact ->
//         "/contact"
//     | Welcome ->
//         "/home"

let pageParser : Parser<Page -> Page,_> =
    oneOf
        [
            map Page.Welcome (s "welcome")
            map Page.About (s "about")
            map Page.Portfolio (s "portfolio")
            map Page.Contact (s "contact")
        ]

let urlParser location = parsePath pageParser location

// // REQUIRES WEBAPPMODEL
// // WHICH MAKES THIS MODULE APPEAR LOWER THAN INDEX.FS IN THE CLIENT.FSPROJ
// // BAD?
// let urlUpdate (result: Page option) (model: WebAppModel) =
//     match result with
//     | None ->
//         WebAppModel.Welcome, Cmd.none//Navigation.modifyUrl (toPath Page.Welcome)
//     | Some Page.Welcome ->
//         WebAppModel.Welcome, Cmd.none//Navigation.modifyUrl (toPath Page.Welcome)
//     | Some Page.About ->
//         WebAppModel.AboutSection, Cmd.none//Navigation.modifyUrl (toPath Page.About)
//     | Some Page.Portfolio ->
//         WebAppModel.Portfolio PortfolioGallery, Cmd.none//Navigation.modifyUrl (toPath Page.Portfolio)
//     | Some Page.Contact ->
//         WebAppModel.Contact, Cmd.none
//         //Navigation.modifyUrl (toPath Page.Contact)
//     // | Some Page.Welcome ->
//     // | Some Page.Welcome ->



// REVIEW TO UNDERSTAND!!
// ------------
