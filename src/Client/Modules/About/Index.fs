module AboutSection

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared.SharedAboutSection

// TODO    
    // STATS BUTTON TO PULL UP RESUME (ARTSY STYLE) (DOCUMENT STYLE IN CONTACT ME?)
    // ANIMATE CARD SLIDING WITH ARROW SOMEHOW?
    // ADD EXTRA DETAIL SECTIONS (WIP)

    // TODO -> CONTENT SECTIONS -> {LARGE HERO STYLE TILE + QUICK DESCRIPT} -> CLICK TO MAKE HERO CONTENT WITH LONG DESCRIPTION / DETAILS
        // CONTENT SECTIONS IS A SEMI MAJOR REFACTOR, IN THAT SUB NESTED MODULES NEED TO BE CREATED AND HANDLED
        // GENERAL: GENERAL ABOUT ME INFO
        // PERSONAL: About me as individual, short and sweet
            // sailed the carribean, enjoys getting fkn wrkd
            // laid back individual
            // lmao: humble, handsome, talented, intelligent, etc..  <- reference sodapoppin
            // enjoy being creative, 
        // PROFESSIONAL: professional and achievements made in any field
            // working with programming languages for around 5 years
            // worked as developer, helpdesk / support, tester, customer detail gathering and writing custom solutions, custom platform developer / enhancements
            // can focus well alone or work well with groups or other individuals
            // enjoy learning and applying new languages and techniques
            // all icons and designs used were made by me.
            // this site needs to address the fact that all of this doesn't exactly align with what a typical business requires:
                // worked with building custom data processors
                // worked with custom transforms of data to fit to platform requirements (custom solutions, magento, etc..)
                // front end changes by user and business alike
                // Taken / review calls, emails and reports to help resolve customer and business issues
                // deployed the code with the boys on release nights
        // SKILLS & RESUME: Boiled down content sections into a professional resume

    // HOW DO I DRIVE CONTINUOUS USAGE OF THE SITE TO PRGORESS THE USER EXPERIENCE
        // PREVIOUS SECTION ELEMENT -> WELCOME
        // NEXT SECTION ELEMENT -> PORTFOLIO

type Msg =
    | ToggleModal of int

// this should be more structured to be passed to a function for generating the modal content based on the index
// structure in a way the formating can plug in easily for all subsections
// title, main blurb, bullet point list, etc..(?)
let modalContent = [
    "More content about the general nature of this website and myself"
    "More content about the professional aspects of myself and abilities"
    "More content about the personal aspects of myself and qualities"
]

let toggleModal model index =
    let test = if (index <> model.ActiveModalIndex) then { model with ActiveModalIndex = index } else model
    { test with ModalIsActive = not model.ModalIsActive }

let update msg model : Model * Cmd<Msg> =
    match msg with
    | ToggleModal int ->
        let toggled = toggleModal model int
        toggled, Cmd.none

let genericModal model dispatch =
    Modal.modal [ Modal.IsActive model.ModalIsActive ] [ 
        Modal.background [Props [ OnClick (fun _ -> ToggleModal model.ActiveModalIndex |> dispatch) ]] []
        Modal.content [ ] [ 
            Level.level [] [
                Container.container [ Container.Props [ ClassName "aboutModalContentCard" ] ] [
                    Level.level [] [ Level.item [] [ h1 [] [ str "General"] ] ] // this should be the title of the modal content data
                    Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground"] ] [
                        // img?
                        Level.item [ ] [ // this should be the content of the modal content data
                            p [] [ str "I wrote this website as a way to demonstrate some of my skills, processes, and personal traits / interests. As a mainly self-taught computer programmer, I am constantly looking for new and interesting aspects of technology. Check back frquently to see what's new, as I plan to update this with new features, games and content. I wrote all the code from a boilerplate, drew all the icons and designs seen across the website, and am hosting and running continuous deployments for development. Check out the portfolio section for some example demo's, explore some drawings or check out the source code that comprises the different sections and the website itself..." ]
                        ]
                        Level.item [ ] [ p [] [ str (modalContent.Item(model.ActiveModalIndex)) ] ]
                    ]
                ]
            ]
        ]
        Modal.close [ Modal.Close.Size IsLarge; Modal.Close.OnClick (fun _ -> ToggleModal model.ActiveModalIndex |> dispatch) ] [] 
    ]

let mainAbout dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                // ADD / UPDATE PIC
                Tile.child [ ] [ Image.image [] [ img [Src "./imgs/Out for Blood.png"] ] ]
                Tile.child [] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground"] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "General"]
                                    p [] [ str "I wrote this website as a way to demonstrate some of my skills, processes, and personal traits / interests. As a mainly self-taught computer programmer, I am constantly looking for new and interesting aspects of technology. Check back frquently to see what's new, as I plan to update this with new features, games and content. I wrote all the code from a boilerplate, drew all the icons and designs seen across the website, and am hosting and running continuous deployments for development. Check out the portfolio section for some example demo's, explore some drawings or check out the source code that comprises the different sections and the website itself..." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "aboutSectionHoverSelection"] ] [
                            p [ OnClick (fun _ -> ToggleModal 0 |> dispatch )] [ str "Read More" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let secondaryAbout dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                Tile.child [] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground"] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "Professional"]
                                    p [] [ str "I've been working as a software engineer for around 5 years. Through this time, I've worked as a full stack developer, tester, requirement gatherer, technical support assistance. I enjoy learning & discussing new languages, practices and design patterns, thinking critically and creatively to solve issues, etc.. blah blah." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "aboutSectionHoverSelection"] ] [
                            p [ OnClick (fun _ -> ToggleModal 1 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                ]
                // ADD / UPDATE PIC
                Tile.child [] [ Image.image [ ] [ img [Src "./imgs/Misfortune.png"] ] ]
            ]
        ]
    ]

let tertiaryAbout dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                Tile.child [] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground"] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "Personal"]
                                    p [] [ str "I'm a person just like you (unless you are a bot), who enjoys kicking back and relaxing. Check out some IRL shenanigans pics below." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "aboutSectionHoverSelection"] ] [
                            p [ OnClick (fun _ -> ToggleModal 2 |> dispatch )] [ str "Read More" ]
                        ]
                    ]
                    Container.container [ Container.Props [ ClassName "paddedContainer" ] ] [
                        Columns.columns [] [
                            // ADD / UPDATE LIFE PICS
                            Image.image [] [ img [Src "./imgs/Bowing Bubbles.png"] ]
                            Image.image [] [ img [Src "./imgs/Backstabber.png"] ]
                            Image.image [] [ img [Src "./imgs/Misfortune.png"] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let view model dispatch =
    Container.container [ Container.Props [ ClassName "aboutSectionContainer" ] ] [
        mainAbout dispatch
        secondaryAbout dispatch
        tertiaryAbout dispatch
        genericModal model dispatch
    ]