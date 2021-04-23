module Welcome

open Fable.React
open Fable.React.Props
open Fulma

open Shared

// Message to Load next section, rather than using header nav section.
type Msg =
    | SwitchSection of SharedWebAppViewSections.AppSection

// contact / online presence links
 // direct email button, email address, github, --linkedin?, elmStore?

let indexTile imgSrc webAppSection descrip dispatch =
    let sectionButtonTitle = SharedWebAppViewSections.appSectionStringTitle webAppSection
    Tile.parent [ Tile.Size Tile.Is3; Tile.IsVertical ] [
            Image.image [] [ img [ Src imgSrc ] ]
            div [ Style [ Margin "auto"; Padding "5px" ] ] [ str descrip ]
            SharedViewModule.sharedSwitchSectionButton ( SwitchSection ( webAppSection) ) sectionButtonTitle dispatch
    ]

let viewMain dispatch =
        div [ ClassName "generalContentCard"; Style [Width "100%"; Margin "auto" ] ] [
                Container.container [ Container.Props [ Style [ Width "50%"; Margin "auto" ] ] ] [ Image.image [ Image.Props [ Style [ Margin "15px" ] ] ] [ img [ Style [ BorderRadius "50%"]; Src "./imgs/Harlot.jpeg" ] ] ]
                h1 [ ClassName "welcomeGreeting"; Style [ FontSize "60px" ] ] [ str "Welcome." ]
                h2 [ ClassName "welcomeGreeting";  ] [ str "This site's my name." ]
                h2 [ ClassName "welcomeGreeting"; ] [ str "Web development is part of my game." ]
                SharedViewModule.sharedSwitchSectionButton ( SwitchSection ( SharedWebAppViewSections.AboutAppView ) ) "Check it out for yourself." dispatch
        ]

let view2 dispatch =
    div [ ClassName "generalContentCard"; Style [ Padding "50px"; Color "#fff"] ] [
        h1 [ ClassName "welcomeGreeting" ] [ str "Site Index" ]
        Tile.ancestor [] [
            Tile.parent [Tile.IsVertical] [
                Tile.parent [] [
                    indexTile "./imgs/Out for Blood.jpeg" ( SharedWebAppViewSections.AboutAppView ) "About: Learn more about the site and it's purpose." dispatch 
                    indexTile "./imgs/Bowing Bubbles.jpeg" ( SharedWebAppViewSections.PortfolioAppCodeView ) "Code: Check out some mini games or code gists." dispatch
                    indexTile "./imgs/Misfortune.jpeg" ( SharedWebAppViewSections.PortfolioAppDesignView ) "Designs: Check out some drawings I've done recently." dispatch
                    indexTile "./imgs/Backstabber.jpeg" ( SharedWebAppViewSections.ContactAppView ) "Contact: Let's hear it already!" dispatch
                ]
            ]
        ]
    ]
 
// Requires dispatch as send Msg back to top level in order to go to the next page
let view dispatch =
    div [ ClassName "centered" ] [
            viewMain dispatch
            view2 dispatch
    ]