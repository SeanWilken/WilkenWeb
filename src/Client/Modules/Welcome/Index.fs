module Welcome

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma


// Message to Load next section, rather than using header nav section.
type Msg =
    | NextSection
 
// Requires dispatch as send Msg back to top level in order to go to the next page
let view dispatch =
    div [ ClassName "welcomeSectionContainer" ] [
        Tile.ancestor [] [
            Tile.parent [] [ Tile.child [] [] ]
            Tile.parent [ Tile.Size Tile.Is4 ] [
                Tile.child [] [ Container.container [ Container.Props [ ClassName "welcomeHeroImage" ] ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ] ]
            ]
            Tile.parent [ Tile.Size Tile.Is4 ] [
                Columns.columns [ Columns.IsVCentered ] [
                    Container.container [ Container.Props [ ClassName "welcomeContentCard" ] ] [
                        Column.column [] [
                            Tile.child [] [
                                // THIS LOOKS BAD WHEN ON SMALLER DEVICES
                                div [ ClassName "contentCardTextBackground" ] [
                                    h2 [] [ str "Greetings, web traveler." ] 
                                    h1 [] [ str "WELCOME" ] 
                                    p [] [ str "My name is Sean and this is my personal website. Written, designed and hosted by yours truly. Continue on to check it out for yourself." ] 
                                    // SMALL BLURB TO PAD CONTENT? SMALL WEBSITE DESCRIPTION, TOUR BUTTON?
                                    // YOU LOOK A LITTLE LOST, WOULD YOU LIKE SOME HELP?
                                    a [ OnClick ( fun _ -> NextSection |> dispatch ) ] [
                                        Container.container [Container.Props [ ClassName "welcomeNextSectionCard" ] ] [
                                            h1 [] [ str "Learn more about this website and it's creator." ]
                                            h2 [] [ str "Enter the About Section" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            Tile.parent [] [ Tile.child [] [] ]
        ]
    ]