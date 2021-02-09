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
// fix mobile offset
let view dispatch =
    div [ ClassName "welcomeSectionContainer" ] [
        Tile.ancestor [] [
            Tile.parent [ Tile.Size Tile.Is2 ] []
            Tile.parent [ Tile.Size Tile.Is4 ] [
                Columns.columns [ Columns.IsVCentered ] [ Tile.child [] [ 
                    Container.container [ Container.Props [ ClassName "welcomeHeroImage" ] ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ] 
                ] ]
            ]
            Tile.parent [ Tile.Size Tile.Is1 ] []
            Tile.parent [ Tile.Size Tile.Is3 ] [
                Columns.columns [ Columns.IsVCentered ] [ 
                    Tile.child [] [
                        Container.container [ Container.Props [ ClassName "welcomeContentCard" ] ] [
                            Column.column [] [
                                div [ ClassName "contentCardTextBackground" ] [
                                    h2 [] [ str "Greetings, web traveler." ] 
                                    h1 [] [ str "WELCOME" ] 
                                    p [] [ str "My name is Sean and this is my personal website. Written, designed and hosted by yours truly. Check it out for yourself." ] 
                                    Level.level [ Level.Level.Props [ ClassName "welcomeNextSectionCard"] ] [
                                        p [ OnClick (fun _ -> NextSection |> dispatch )] [ str "Learn More" ]
                                    ]
                                ]
                            ] 
                        ]
                    ]
                ]
            ]
            Tile.parent [ Tile.Size Tile.Is1 ] []
        ]
        // SMALL BLURB TO PAD CONTENT? SMALL WEBSITE DESCRIPTION, TOUR BUTTON?
        // Tile.ancestor [] [
        //     Tile.parent [] [ 
        //         Tile.child [] []
        //         Tile.child [ Tile.Size Tile.Is3 ] [
        //             div [ ClassName "contentCardTextBackground" ] [
        //                 // YOU LOOK A LITTLE LOST, WOULD YOU LIKE SOME HELP?
        //                 Level.level [ Level.Level.Props [ ClassName "aboutSectionHoverSelection"] ] [
        //                     p [ OnClick (fun _ -> NextSection |> dispatch )] [ str "Show me around!" ]
        //                 ]
        //             ]
        //         ] 
        //         Tile.child [] []
        //     ]
        // ]
    ]