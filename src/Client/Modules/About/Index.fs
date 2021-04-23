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

// General Tile Level
let aboutGeneralTileDetails = Some {
    Header = "General"
    SubHeader = "I wrote, designed, deployed and am hosting this website as a way to demonstrate some of my abilities & explain a little about me, in case you don't already know."
    Image = None
}
let aboutGeneralTileDirectoryButton = Some {
    ButtonTitle = "Read More"
    ButtonMsg = ToggleModal 0
}
let aboutGeneralTileImage = Some "./imgs/Out for Blood.jpeg"

// Personal Tile Level
let aboutPersonalTileDetails = Some {
    Header = "Personal"
    SubHeader = "Who am I? Well I'm a person just like you, unless of course, you're a bot..."
    Image = None
}
let aboutPersonalTileDirectoryButton = Some {
    ButtonTitle = "Read More"
    ButtonMsg = ToggleModal 2
}
// Personal Full Tile Level Images
let aboutPersonalFullTileImages = Some [ 
    "./imgs/Kcuf Em.jpeg"
    "./imgs/Misfortune.jpeg"
]

// Professional Tile Level
let aboutProfessionalTileDetails = Some {
    Header = "Industry"
    SubHeader = "This is what it should look like when play time is over.."
    Image = None
}
let aboutProfessionalTileDirectoryButton = Some {
    ButtonTitle = "Read More"
    ButtonMsg = ToggleModal 1
}
let aboutProfessionalTileImage = Some "./imgs/Caution Very Hot.jpeg"

// Code Tile
let codeGalleryDirectoryButtonDetails = Some {
    Header = "Code Gallery"
    SubHeader = "Check out some simple games and their code.."
    Image = Some "./imgs/Harlot.jpeg"
}
let codeGalleryDirectoryButton = Some {
    ButtonTitle = "Code"
    ButtonMsg = SwitchSection ( SharedWebAppViewSections.PortfolioAppCodeView )
}

// Portfolio Landing Tile
let portfolioDirectoryButtonDetails = Some {
    Header = "Portfolio"
    SubHeader = "Watch your back and check this out."
    Image = Some "./imgs/Backstabber.jpeg"
}
let portfolioDirectoryButton = Some {
    ButtonTitle = "Portfolio"
    ButtonMsg = SwitchSection ( SharedWebAppViewSections.PortfolioAppLandingView )
}

// Art Gallery Tile
let artGalleryDirectoryButtonDetails = Some {
    Header = "Design Gallery"
    SubHeader = "I draw things sometimes, some of which I actually kinda like."
    Image = Some "./imgs/Bowing Bubbles.jpeg"
}
let artGalleryDirectoryButton = Some {
    ButtonTitle = "Designs"
    ButtonMsg = SwitchSection ( SharedWebAppViewSections.PortfolioAppDesignView )
}

// ADD IMAGES AND THINGS TO MAKE THIS BE A GENERIC LAYOUT PASSED THE CONTENT MODAL / VIEW
// Switched to single string for now, kind of liked how it looked as list iterated and broken
// down into sentences / points. (MAYBE USE BULLET POINTS)
let generalModalContent = {
    Title = "General"
    MainContent =
        """I wrote all the code from a basic SAFE stack boilerplate, which gets deployed to Azure via a FAKE script.
          Check out the portfolio section for some example demo's, check out the source code that
          comprises the different sections and the website itself, or take a peak at some drawings...
          Check back frquently to see what's new, as I plan to update this with new features, games and content.
          You can find all the code for the site on my Github if you want to review some sections in depth, get an idea
          of how I leverage the technologies or solve some domain or logistical issues.
          """
        (*
        *)
    PreviousLabel = "Welcome"
    NextLabel = "Industry"
}
let professionalModalContent = {
    Title = "Industry"
    MainContent =
        (*
        I've worked: with mid and small team sizes, working well with others or alone, with custom solutions, open source projects, many late nights tinkering, 
        searching for and fixing bugs, deploying new code and putting out the fires, working with clients to come up with solutions for problems and bottlenecks being faced.
        Source requirements, come up with timelines, architecture and logical solutions for such work and implemented the final custom solutions that get deployed 
        into production environments.
        In that time, I've been a: full-stack developer, tester, help-desk / support, 
        requirement gatherer, custom integration specialist and a lot more..
        I've professionally developed, implemented and maintained things like custom data processors
        / data integrations / themes / websites / projects & solutions, and created many more personal hobby projects (such as this site, Unity projects, etc..) and scripts.
        *)
        """As a mainly self-taught computer programmer, I am constantly looking for new and interesting aspects of technology. 
        I've worked in a variety of positions and dealt with a number of duites that a full stack developer or software engineer may have to go through. 
        With daily duties ranging from addressing clients directly to source requirements or troubleshoot issues, to implementing custom integrations and solutions.
        I enjoy expanding my skill set by learning new languages, practices and design patterns and having the freedom to solve complex issues by thinking critically and creatively."""
    PreviousLabel = "General"
    NextLabel = "Personal"
}
let personalModalContent = {
    Title = "Personal"
    MainContent =
        """I'm pretty laid back and enjoy living life in the momement, learning and experiencing new things. I like being challenged and adapting
        to problems that present themselves along the way. Fun fact: I've sailed across the Carribean Sea back to the states and driven across the United States cross twice..."""
    PreviousLabel = "Industry"
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
    div [ ClassName "generalContentCard" ] [
        div [] [
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
        Level.level [ Level.Level.Props [ ClassName "generalContentCardTextBackground" ] ] [
            Level.item [] [
                Tile.child [] [
                    h1 [ ClassName "welcomeGreeting"; Style [ FontSize "50px" ] ] [ str tileDetails.Header ]
                    h1 [ ClassName "welcomeGreeting"; Style [ FontSize "20px"; FontFamily "Exo"; FontWeight "normal" ] ] [ str tileDetails.SubHeader ]
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
    | Some ( dirButton ) -> SharedViewModule.sharedSwitchSectionButton dirButton.ButtonMsg dirButton.ButtonTitle dispatch
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
            Columns.columns [ Columns.IsVCentered ] [
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
            Tile.child [ Tile.Size Tile.Is4; Tile.Props [ Style [ Margin "auto" ] ] ] [
                Container.container [ Container.Props [ ClassName "centered" ] ] [
                    aboutTileDetailView tileDetails
                    aboutTileButtonView tileButton dispatch
                ]
            ]















// separate level for details view and image view
let aboutTileDetailsLevel tileDetails tileButton tileImage dispatch =
    div [ ClassName "generalContentCard" ] [
        Level.level [] [
            Tile.child [] []
            aboutTileImageView tileImage
            Tile.child [] []
            aboutTileDetailsView tileDetails tileButton dispatch
            Tile.child [] []
        ]
    ]


let aboutTileDetailsFullView tileDetails tileButton tileImages dispatch =
    div [ ClassName "generalContentCard" ] [
        Level.level [] [
            Tile.child [] [
                Container.container [ Container.Props [ ClassName "centered" ] ] [
                    aboutTileDetailView tileDetails
                    aboutTileButtonView tileButton dispatch
                ]
                aboutTileImagesFullView tileImages
            ]
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
    div [] [
        aboutModal model ( aboutModalContentSections.Item( model.ActiveModalIndex ) ) dispatch
        aboutTileDetailsLevel aboutGeneralTileDetails aboutGeneralTileDirectoryButton aboutGeneralTileImage dispatch
        aboutTileDetailsFullView aboutPersonalTileDetails aboutPersonalTileDirectoryButton aboutPersonalFullTileImages dispatch
        aboutTileDetailsLevel aboutProfessionalTileDetails aboutProfessionalTileDirectoryButton aboutProfessionalTileImage dispatch
        aboutDirectory dispatch
    ]