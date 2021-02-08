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
            Tile.parent [ Tile.Size Tile.Is3 ] [
                Tile.child [] [ Container.container [ Container.Props [ ClassName "welcomeHeroImage" ] ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ] ]
            ]
            Tile.parent [ Tile.Size Tile.Is4 ] [
                Columns.columns [ Columns.IsVCentered ] [
                    Container.container [ Container.Props [ ClassName "welcomeContentCard" ] ] [
                        Column.column [] [
                            Tile.child [] [
                                // TODO -- THIS LOOKS BAD WHEN ON SMALLER DEVICES
                                div [ ClassName "contentCardTextBackground" ] [
                                    h2 [] [ str "Greetings, web traveler." ] 
                                    h1 [] [ str "WELCOME" ] 
                                    p [] [ str "My name is Sean and this is my personal website. Written, designed and hosted by yours truly. Continue on to check it out for yourself." ] 
                                    // SMALL BLURB TO PAD CONTENT? SMALL WEBSITE DESCRIPTION, TOUR BUTTON?
                                    Level.level [ Level.Level.Props [ ClassName "aboutSectionHoverSelection"] ] [
                                        p [ OnClick (fun _ -> NextSection |> dispatch )] [ str "Learn More" ]
                                    ]
                                    // old style, may want to use the style from this below the level for text swap on hover.
                                    // a [ OnClick ( fun _ -> NextSection |> dispatch ) ] [
                                    //     Container.container [Container.Props [ ClassName "welcomeNextSectionCard" ] ] [
                                    //         h1 [] [ str "Learn more about this website and it's creator." ]
                                    //         h2 [] [ str "Enter the About Section" ]
                                    //     ]
                                    // ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            Tile.parent [] [ Tile.child [] [] ]
        ]
        Tile.ancestor [] [
            Tile.parent [] [ 
                Tile.child [] []
                Tile.child [ Tile.Size Tile.Is3 ] [
                    div [ ClassName "contentCardTextBackground" ] [
                        // YOU LOOK A LITTLE LOST, WOULD YOU LIKE SOME HELP?
                        Level.level [ Level.Level.Props [ ClassName "aboutSectionHoverSelection"] ] [
                            p [ OnClick (fun _ -> NextSection |> dispatch )] [ str "Show me around!" ]
                        ]
                    ]
                ] 
                Tile.child [] []
            ]
        ]
    ]