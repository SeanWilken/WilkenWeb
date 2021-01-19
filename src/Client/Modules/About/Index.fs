module AboutSection

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

    // TODO -> CONTENT SECTIONS -> {LARGE HERO STYLE TILE + QUICK DESCRIPT} -> CLICK TO MAKE HERO CONTENT WITH LONG DESCRIPTION / DETAILS
        // CONTENT SECTIONS IS A SEMI MAJOR REFACTOR, IN THAT SUB NESTED MODULES NEED TO BE CREATED AND HANDLED
        // GENERAL: GENERAL ABOUT ME INFO
        // PERSONAL: About me as individual, short and sweet
        // PROFESSIONAL: professional and achievements made in any field
        // SKILLS & RESUME: Boiled down content sections into a professional resume

    // HOW DO I DRIVE CONTINUOUS USAGE OF THE SITE TO PRGORESS THE USER EXPERIENCE
        // PREVIOUS SECTION ELEMENT -> WELCOME
        // NEXT SECTION ELEMENT -> PORTFOLIO

// CHANGE BUTTON LOCATION AND DIRECTION
// ALWAYS BOTTOM LIKE STYLE 3?

// ANIMATE CARD SLIDING WITH ARROW SOMEHOW?
let mainAbout =
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
                            p [] [ str "Read More" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

// professional 
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

// ADD EXTRA DETAIL SECTIONS
// ANIMATE CARD SLIDING WITH ARROW SOMEHOW?
let secondaryAbout =
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
                            p [] [ str "Read More" ] // these will pull up modal!!
                        ]
                    ]
                ]
                // ADD / UPDATE PIC
                Tile.child [] [ Image.image [ ] [ img [Src "./imgs/Misfortune.png"] ] ]
            ]
        ]
    ]

// personal
    // sailed the carribean, enjoys getting fkn wrkd
    // laid back individual
    // lmao: humble, handsome, talented, intelligent, etc.. 
    // enjoy being creative, 

// TODO    
    // STATS BUTTON TO PULL UP RESUME (ARTSY STYLE) (DOCUMENT STYLE IN CONTACT ME?)
    // ANIMATE CARD SLIDING WITH ARROW SOMEHOW?
let tertiaryAbout =
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
                            p [] [ str "Read More" ]
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

let view =
    Container.container [ Container.Props [ ClassName "aboutSectionContainer" ] ] [
        mainAbout
        secondaryAbout
        tertiaryAbout
    ]