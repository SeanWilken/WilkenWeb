module AboutSection

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
        // SKILLS & RESUME: Boiled down content sections into a professional resume

    // HOW DO I DRIVE CONTINUOUS USAGE OF THE SITE TO PRGORESS THE USER EXPERIENCE
       // STYLE AND ANIMATION WISE, NEED A COHESIVE FLOW THROUGH THE SECTIONS OF THE WEBSITE, CURRENT IMPLEMENTATION IS FRAGMENTED IN NAVIGATION COMMANDS (WHERE AND HOW)

type Msg =
    | ToggleModal of int
    | PreviousSection
    | NextSection
    | SwitchModal of int
    // don't break them all down into one single group, browse through each point.

// ADD IMAGES AND THINGS TO MAKE THIS BE A GENERIC LAYOUT PASSED THE CONTENT MODAL / VIEW
// Switched to single string for now, kind of liked how it looked as list iterated and broken
// down into sentences / points. (MAYBE USE BULLET POINTS)
let generalModalContent = {
    Title = "General"
    MainContent =
        """As a mainly self-taught computer programmer, I am constantly looking for new and interesting aspects of technology.
          Check back frquently to see what's new, as I plan to update this with new features, games and content. 
          I wrote all the code from a boilerplate, drew all the icons and designs seen across the website,
          and am hosting and running continuous deployments for development. 
          Check out the portfolio section for some example demo's, explore some drawings or check out the source code that 
          comprises the different sections and the website itself..."""
        (*
         As a mainly self-taught computer programmer, I am constantly looking for new and interesting aspects of technology. Check back frquently to see what's new, as I plan to update this with new features, games and content.
        I wrote all the code from a boilerplate, drew all the icons and graphic designs seen across the website, and am hosting and running continuous deployments for development.
        Check out the portfolio section for some example demo's, explore some drawings or check out the source code that comprises the different sections and the website itself
        *)
    PreviousLabel = "Welcome"
    NextLabel = "Professional"
}
let professionalModalContent = {
    Title = "Professional"
    MainContent =
        (*
        I've worked: with mid and small team sizes, working well with others or alone, with custom solutions, open source projects, many late nights tinkering, 
        fixing bugs, deploying new code and putting out the fires, with clients to come up with solutions for problems and bottlenecks being faced. 
        Source requirements, come up with timelines, architecture and logical solutions for such work and implemented the final custom solutions that get deployed 
        into production environments.
        In that time, I've been a: full-stack developer, tester, help-desk / support, 
        requirement gatherer, custom integration specialist and a lot more..
        I've professionally developed, implemented and maintained things like custom data processors
        / data integrations / themes / websites / projects & solutions, and created many more personal hobby projects (such as this site, Unity projects, etc..) and scripts.
        *)
        """I've worked as a full stack developer, tester, requirement gatherer, technical support assistance. 
        I enjoy learning & discussing new languages, practices and design patterns, thinking critically and creatively to solve issues, etc.. blah blah."""
    PreviousLabel = "General"
    NextLabel = "Personal"
}
let personalModalContent = {
    Title = "Personal"
    MainContent =
        (*
        I'm pretty laid back and enjoy living life in the momement, learning and experiencing new things, overcoming challenges and being creative. 
        Check out some of my drawings & let me know what you think. 
        *)
        """Fun fact: I've sailed the carribean sea back to the states and driven across the United States cross twice..."""
    PreviousLabel = "Professional"
    NextLabel = "Portfolio"
}
let websiteModalContent = {
    Title = "The Site Stack"
    MainContent =
    (*
    ...and am hosting and running continuous 
    deployments for development. .Link to SAFE Stack about.
    *)
        """I wrote all the code from a SAFE Stack boilerplate..."""
    PreviousLabel = "Personal"
    NextLabel = "Portfolio"
}

let aboutModalContentSections = [ generalModalContent; professionalModalContent; personalModalContent ]

// fulma timeline to walk through timeline of events
let toggleModal model index =
    let activeModal = if ( index <> model.ActiveModalIndex ) then { model with ActiveModalIndex = index } else model
    { activeModal with ModalIsActive = not model.ModalIsActive }

let update msg model : Model * Cmd<Msg> =
    match msg with
    | ToggleModal int ->
        let toggled = toggleModal model int
        toggled, Cmd.none
    | SwitchModal int ->
        match int with
        | 1 -> { model with ActiveModalIndex = model.ActiveModalIndex + 1 }, Cmd.none
        | -1 -> { model with ActiveModalIndex = model.ActiveModalIndex - 1}, Cmd.none
        | _ -> model, Cmd.none
    | _ -> model, Cmd.none

let aboutModalCard modalContent = 
    Level.level [] [
        Level.item [] [
            div [ ClassName "aboutContentCard" ] [
                div [ ClassName "contentCardTextBackground" ] [
                        Container.container [] [ p [] [ str modalContent.MainContent ] ]
                    ]
                ]
            ]
        ]

// ALSO USE SHARED MODAL?
let aboutModal model dispatch modalContent =
    Modal.modal [ Modal.IsActive model.ModalIsActive ] [ 
        Modal.background [ Props [ OnClick ( fun _ -> ToggleModal model.ActiveModalIndex |> dispatch ) ] ] []
        Modal.content [ Props [ClassName "modalContent"] ] [
            SharedViewModule.codeModalHeader ( aboutModalContentSections.Item(model.ActiveModalIndex).Title ) ( ToggleModal (model.ActiveModalIndex) ) dispatch
            aboutModalCard modalContent
            // Columns.columns [ Columns.IsVCentered ] [
                // Column.column [ Column.Props [ ClassName "leftCol" ] ] [
                //     let navFunc = ( if ( model.ActiveModalIndex = 0 ) then PreviousSection else SwitchModal (-1) )
                //     // change text if navigating to new submodule?
                //     SharedViewModule.bigNavButton navFunc "PREV" dispatch
                // ]
                // Column.column [] [
                // ]
                // Column.column [ Column.Props [ ClassName "rightCol" ] ] [
                //     let navFunc = ( if ( model.ActiveModalIndex = aboutModalContentSections.Length - 1 ) then NextSection else SwitchModal (1) )
                //     // change text if navigating to new submodule?
                //     SharedViewModule.bigNavButton navFunc "NEXT" dispatch
                // ]
            // ] 
        ]
    ]

let mainAbout dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                Tile.child [] []
                Tile.child [ Tile.Size Tile.Is4 ] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
                            Level.item [] [
                                Tile.child [] [
                                    h1 [] [ str "General" ]
                                    h3 [] [ str "I wrote this website as a way to" ]
                                    h3 [] [ str "demonstrate some of my abilities &" ]
                                    h3 [] [ str "explain a little about me." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
                            p [ OnClick ( fun _ -> ToggleModal 0 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                ]
                Tile.child [ Tile.Size Tile.Is5 ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ]
                Tile.child [] []
            ]
        ]
    ]

let secondaryAbout dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                Tile.child [] []
                Tile.child [Tile.Size Tile.Is4] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "Professional" ]
                                    p [] [ str """I've been working with programming languages for about 5 years. Read More to check out what I've done in that time""" ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
                            p [ OnClick ( fun _ -> ToggleModal 1 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                ]
                // ADD / UPDATE PIC
                Tile.child [ Tile.Size Tile.Is5 ] [ Image.image [ ] [ img [Src "./imgs/Misfortune.png" ] ] ]
                Tile.child [] []
            ]
        ]
    ]

let tertiaryAbout dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                Tile.child [] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "Personal" ]
                                    p [] [ str """I'm a person just like you (unless you're a bot), who enjoys kicking back and relaxing. Check out some IRL shenanigans pics below.""" ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
                            p [ OnClick ( fun _ -> ToggleModal 2 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                    Container.container [ Container.Props [ ClassName "paddedContainer" ] ] [
                        Columns.columns [] [
                            // ADD / UPDATE LIFE PICS
                            Image.image [] [ img [ Src "./imgs/Bowing Bubbles.png" ] ]
                            Image.image [] [ img [ Src "./imgs/Backstabber.png" ] ]
                            Image.image [] [ img [ Src "./imgs/Misfortune.png" ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let general dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                Tile.child [ Tile.Size Tile.Is4 ] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
                            Level.item [] [
                                Tile.child [] [
                                    h1 [] [ str "General" ]
                                    h3 [] [ str "I wrote this website as a way to" ]
                                    h3 [] [ str "demonstrate some of my abilities &" ]
                                    h3 [] [ str "explain a little about me." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
                            p [ OnClick ( fun _ -> ToggleModal 0 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                ]
                Tile.child [ Tile.Size Tile.Is4 ] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "Professional" ]
                                    h3 [] [ str "I've been working with programming languages for about 5 years." ]
                                    h3 [] [ str "Read More to check out what I've done in that time." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
                            p [ OnClick ( fun _ -> ToggleModal 1 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                ]
                Tile.child [ Tile.Size Tile.Is4 ] [ 
                    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
                            Level.item [] [
                                Tile.child [ ] [
                                    h1 [] [ str "Personal" ]
                                    h3 [] [ str "I'm a person just like you (unless you're a bot)." ]
                                    h3 [] [ str "I enjoy kicking back and relaxing, exploring, and experiencing new things." ]
                                ]
                            ]
                        ]
                        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
                            p [ OnClick ( fun _ -> ToggleModal 1 |> dispatch ) ] [ str "Read More" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let view model dispatch =
    div [ ClassName "aboutSectionContainer" ] [
        // general dispatch
        mainAbout dispatch // change what these are
        tertiaryAbout dispatch // change what these are
        secondaryAbout dispatch // change what these are
        aboutModal ( model ) ( dispatch ) ( aboutModalContentSections.Item( model.ActiveModalIndex ) )
    ]