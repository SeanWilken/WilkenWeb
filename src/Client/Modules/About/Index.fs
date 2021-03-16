module AboutSection

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared.SharedAboutSection

// TODO    
    // STATS BUTTON TO PULL UP RESUME (ARTSY STYLE) (DOCUMENT STYLE IN CONTACT ME?)
    // ANIMATE CARD SLIDING WITH ARROW SOMEHOW?

open Shared

type Msg =
    | ToggleModal of int
    | SwitchModal of int
    | SwitchSection of SharedWebAppViewSections.AppSection 

type DirectoryTileDetails = {
    Header : string
    SubHeader : string
    Image : string option
}

type DirectoryButton = {
    ButtonTitle : string
    ButtonMsg : Msg
}

// Code Tile
let codeGalleryDirectoryButtonDetails = Some {
    Header = "Code Gallery"
    SubHeader = "Play or review the code that is this site or it's features.."
    Image = Some "./imgs/Out for Blood.png"
}
let codeGalleryDirectoryButton = Some {
    ButtonTitle = "Code"
    ButtonMsg = SwitchSection ( SharedWebAppViewSections.PortfolioAppCodeView )
}
// Portfolio Landing Tile
let portfolioDirectoryButtonDetails = Some {
    Header = "Portfolio"
    SubHeader = "Check out the code gallery, design gallery or my resume"
    Image = Some "./imgs/Bowing Bubbles.png"
}
let portfolioDirectoryButton = Some {
    ButtonTitle = "Portfolio"
    ButtonMsg = SwitchSection ( SharedWebAppViewSections.PortfolioAppLandingView )
}
// Art Gallery Tile
let artGalleryDirectoryButtonDetails = Some {
    Header = "Design Gallery"
    SubHeader = "I draw things sometimes, some of which I actually kinda like."
    Image = Some "./imgs/Misfortune.png"
}
let artGalleryDirectoryButton = Some {
    ButtonTitle = "Designs"
    ButtonMsg = SwitchSection ( SharedWebAppViewSections.PortfolioAppDesignView )
}

// General Tile Level
let aboutGeneralTileDetails = Some {
    Header = "General"
    SubHeader = "I wrote this website as a way to demonstrate some of my abilities & explain a little about me."
    Image = None
}
let aboutGeneralTileDirectoryButton = Some {
    ButtonTitle = "Read More"
    ButtonMsg = ToggleModal 0
}
let aboutGeneralTileImage = Some "./imgs/Out for Blood.png"
// Professional Tile Level
let aboutProfessionalTileDetails = Some {
    Header = "Professional"
    SubHeader = "I've been working with programming languages for about 5 years. Read More to check out what I've done in that time"
    Image = None
}
let aboutProfessionalTileDirectoryButton = Some {
    ButtonTitle = "Read More"
    ButtonMsg = ToggleModal 1
}
let aboutProfessionalTileImage = Some "./imgs/Misfortune.png"

// Personal Tile Level
let aboutPersonalTileDetails = Some {
    Header = "Personal"
    SubHeader = "I'm a person just like you (unless you're a bot), who enjoys kicking back and relaxing. Check out some IRL shenanigans pics below."
    Image = None
}
let aboutPersonalTileDirectoryButton = Some {
    ButtonTitle = "Read More"
    ButtonMsg = ToggleModal 2
}
// Personal Full Tile Level Images
let aboutPersonalFullTileImages = Some [ 
    "./imgs/Misfortune.png"
    "./imgs/Bowing Bubbles.png"
    "./imgs/Out for Blood.png"
]

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

// Update lifecycle ---------

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

// View -----------

let aboutModalCard modalContent = 
    div [ ClassName "aboutContentCard" ] [
        div [ ClassName "contentCardTextBackground" ] [
            p [] [ str modalContent.MainContent ]
        ]
    ]

let aboutModal model modalContent dispatch =
    SharedViewModule.sharedViewModal
        ( model.ModalIsActive )
        ( SharedViewModule.sharedModalHeader
            ( aboutModalContentSections.Item(model.ActiveModalIndex).Title )
            ( ToggleModal (model.ActiveModalIndex) )
            ( dispatch ) )
        ( aboutModalCard modalContent ) 
        ( span [] [] )


// tile details
let aboutTileDetailView tileDetails =
    match tileDetails with 
    | Some tileDetails ->
        Level.level [ Level.Level.Props [ ClassName "contentCardTextBackground" ] ] [
            Level.item [] [
                Tile.child [] [
                    h2 [] [ str tileDetails.Header ]
                    p [] [ str tileDetails.SubHeader ]
                    match tileDetails.Image with
                    | Some image -> Image.image [] [ img [ Src image ] ]
                    | None -> span [] []
                ]
            ]
        ]
    | None -> span [] []

// tile details action button
let aboutTileButtonView tileButton dispatch =
    match tileButton with
    | Some ( dirButton ) ->
        Level.level [ Level.Level.Props [ ClassName "hoverSelectionElement" ] ] [
            p [ OnClick ( fun _ -> dirButton.ButtonMsg |> dispatch ) ] [ str dirButton.ButtonTitle ]
        ]
    | None -> span [] []

// single tile image
let aboutTileImageView tileImage =
    match tileImage with
    | Some image -> 
        Tile.child [ Tile.Size Tile.Is5 ] [ Image.image [] [ img [ Src image ] ] ]
    | None -> span [] []

//full image level
let aboutTileImagesFullView tileImages =
    match tileImages with
    | Some images ->
        Container.container [ Container.Props [ ClassName "paddedContainer" ] ] [
            Columns.columns [] [
                for image in images do
                    Image.image [] [ img [ Src image ] ]
            ]
        ]
    | None -> span [] []

// share level between details view and image view
let aboutTileDetailsView tileDetails tileButton dispatch =
    if tileDetails = None && tileButton = None 
        then span [] [] 
        else 
            Tile.child [ Tile.Size Tile.Is4 ] [
                Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                    aboutTileDetailView tileDetails
                    aboutTileButtonView tileButton dispatch
                ]
            ]

// separate level for details view and image view
let aboutTileDetailsLevel tileDetails tileButton tileImage dispatch =
    Level.level [] [
        Tile.child [] []
        aboutTileDetailsView tileDetails tileButton dispatch
        aboutTileImageView tileImage
        Tile.child [] []
    ]


let aboutTileDetailsFullView tileDetails tileButton tileImages dispatch =
    Level.level [] [
        Tile.child [] [
            Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
                aboutTileDetailView tileDetails
                aboutTileButtonView tileButton dispatch
            ]
            aboutTileImagesFullView tileImages
        ]
    ]

// change you a lil'
let aboutDirectory dispatch =
    Tile.ancestor [] [
        Tile.parent [] [
            Level.level [] [
                aboutTileDetailsView codeGalleryDirectoryButtonDetails codeGalleryDirectoryButton dispatch
                aboutTileDetailsView portfolioDirectoryButtonDetails portfolioDirectoryButton dispatch
                aboutTileDetailsView artGalleryDirectoryButtonDetails artGalleryDirectoryButton dispatch
            ]
        ]
    ]

let view model dispatch =
    div [ ClassName "aboutSectionContainer" ] [
        aboutModal model ( aboutModalContentSections.Item( model.ActiveModalIndex ) ) dispatch
        aboutTileDetailsLevel aboutGeneralTileDetails aboutGeneralTileDirectoryButton aboutGeneralTileImage dispatch
        aboutTileDetailsFullView aboutPersonalTileDetails aboutPersonalTileDirectoryButton aboutPersonalFullTileImages dispatch
        aboutTileDetailsLevel aboutProfessionalTileDetails aboutProfessionalTileDirectoryButton aboutProfessionalTileImage dispatch
        aboutDirectory dispatch // change you a lil'
    ]