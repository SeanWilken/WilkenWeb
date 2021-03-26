module Welcome

open Fable.React
open Fable.React.Props
open Fulma

open Shared

// Message to Load next section, rather than using header nav section.
type Msg =
    | SwitchSection of SharedWebAppViewSections.AppSection 
 
// Requires dispatch as send Msg back to top level in order to go to the next page
let view dispatch =
    div [ ClassName "welcomeSectionContainer" ] [
        Level.level [] [
            Level.item [] [
                Tile.ancestor [ Tile.Props [ ClassName "welcomeContentCard" ] ] [
                    Tile.parent [Tile.IsVertical] [
                        Tile.child [] [ h2 [] [ str "Welcome" ] ]
                        Tile.child [] [
                            Column.column [] [
                                Container.container [ Container.Props [ ClassName "welcomeHeroImage" ] ] [ Image.image [] [ img [ Src "./imgs/Out for Blood.jpeg" ] ] ] 
                            ]
                        ]
                        Tile.child [] [
                            div [ ClassName "contentCardTextBackground" ] [
                                p [ ] [ str "My name is Sean and this is my personal website." ] 
                                p [ ] [ str "Written, designed and hosted by yours truly." ] 
                                Level.level [ Level.Level.Props [ ClassName "welcomeNextSectionCard" ] ] [
                                    p [ OnClick ( fun _ -> SwitchSection ( SharedWebAppViewSections.AboutAppView ) |> dispatch ) ] [ str "Check it out for yourself." ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]