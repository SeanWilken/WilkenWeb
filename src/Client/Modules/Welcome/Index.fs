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
        Level.level [ Level.Level.Props [] ] [
            Level.item [] [
                Tile.ancestor [ Tile.Props [ ClassName "welcomeContentCard" ] ] [
                    Tile.parent [Tile.IsVertical] [
                        Tile.child [] [
                            h2 [] [ str "Welcome" ]
                        ]
                        Tile.child [] [
                            Column.column [] [
                                Container.container [ Container.Props [ ClassName "welcomeHeroImage" ] ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ] 
                            ]
                        ]
                        Tile.child [] [
                            div [ ClassName "contentCardTextBackground" ] [
                                p [ ] [ str "My name is Sean and this is my personal website." ] 
                                p [ ] [ str "Written, designed and hosted by yours truly." ] 
                                p [ ] [ str "Check it out for yourself." ] 
                                Level.level [ Level.Level.Props [ ClassName "welcomeNextSectionCard" ] ] [
                                    p [ OnClick ( fun _ -> NextSection |> dispatch ) ] [ str "Learn More" ]
                                ]
                            ]
                        ]
                    ] 
                    // div [ ClassName "contentCardTextBackground" ] [
                    // ]
                    // ]
                ]
                // Container.container [ Container.Props [ ClassName "welcomeContentCard" ] ] [
                //     h2 [] [ str "Welcome" ]
                //     // div [ ClassName "contentCardTextBackground" ] [
                //     // ]
                //     Column.column [] [
                //         Container.container [ Container.Props [ ClassName "welcomeHeroImage" ] ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ] 
                //     ]
                //     div [ ClassName "contentCardTextBackground" ] [
                //         p [] [ str "My name is Sean and this is my personal website. Written, designed and hosted by yours truly. Check it out for yourself." ] 
                //         Level.level [ Level.Level.Props [ ClassName "welcomeNextSectionCard" ] ] [
                //             p [ OnClick ( fun _ -> NextSection |> dispatch ) ] [ str "Learn More" ]
                //         ]
                //     ]
                //     // ]
                // ]
            ]
        ]
    ]