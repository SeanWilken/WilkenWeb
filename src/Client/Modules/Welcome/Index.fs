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
    Container.container [ Container.Props [ Style [ TextAlign TextAlignOptions.Center; ] ] ] [
        Tile.ancestor [] [
            Tile.parent [Tile.Size Tile.Is6] [
                Tile.child [] [
                    Container.container [ Container.Props [ ClassName "welcomeHeroImage"; ] ] [
                        Image.image [] [ img [ Src "./imgs/Out for Blood.png"; ] ]
                    ]
                ]
            ]
            Tile.parent [Tile.IsVertical;] [
                Container.container [ Container.Props [ ClassName "welcomeContentCard"; Style [Padding 100; Height "100%"] ] ] [
                    Columns.columns [ Columns.IsVCentered; Columns.Props [ Style [  Height "100%";]  ] ] [
                        Column.column [] [
                            div [ClassName "contentCardTextBackground"] [
                                h2 [] [str "Greetings, web traveler."] 
                                h1 [] [str "WELCOME"] 
                                p [] [ str "My name is Sean and this is my personal website. Written, designed and hosted by yours truly. Continue on to check it out for yourself."] 
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Tile.ancestor [] [
            Tile.parent [] [
                Tile.child [Tile.Size Tile.Is12] [
                    a [ OnClick(fun _ -> NextSection |> dispatch ) ] [
                        Container.container [Container.Props [ ClassName "welcomeNextSectionCard"] ] [
                            h1 [] [ str "Learn more about this website and it's creator."]
                            h2 [] [ str "Enter the About Section" ]
                        ]
                    ]
                ]
            ]
        ]
    ]
