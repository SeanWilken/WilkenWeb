module Contact

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open TileSmash

// CONTACT: -> CONTENT -> {GMAIL / LINKEDIN / (IG / FACEBOOK / TWITTER / ETC..?)}
// Static Page:
// does not depend on the dispatch loop, as currently there are no messages that would need to be submitted into it.
let view =
    div [] [
        Level.level [] [
            Columns.columns [] [
                Column.column [] [
                    Tile.ancestor [ Tile.Props [ ClassName "contactContentCard" ] ] [
                        Tile.parent [] [
                            Tile.child [ Tile.Size Tile.Is5 ] [
                                Column.column [] [ Image.image [] [ img [ Src "./imgs/Misfortune.png" ] ] ]
                            ]
                            Tile.child [ Tile.Size Tile.Is7 ] [
                                div [ ClassName "contentCardTextBackground" ] [
                                    Columns.columns [] [
                                        Column.column [] [ h1 [] [ str "Sean Wilken" ] ]
                                        Column.column [] [ 
                                            p [] [ str """Say hello or drop me a line with your questions or comments
                                                         about the website and it's features. Or for any other reason 
                                                         there might be... Just don't spam me.""" ]                                  
                                        ]
                                        Column.column [] [
                                            a [ Href "mailto: sean.d.wilken@gmail.com" ] [
                                                Level.level [] [ Level.item [] [ 
                                                    Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ] 
                                                    div [] [ str "Sean.D.Wilken@GMail.com" ]
                                                ] ]
                                            ]
                                        ]   
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                Column.column [] [
                    Tile.ancestor [ Tile.Props [ ClassName "contactContentCard" ] ] [
                        Tile.parent [] [
                            Tile.child [ Tile.Size Tile.Is7 ] [
                                div [ ClassName "contentCardTextBackground" ] [
                                    Columns.columns [] [
                                        Column.column [] [ h1 [] [ str "Xero Effort" ] ]
                                        Column.column [] [ p [] [ str "Let's hear what you got to say about the drawings..." ] ]
                                        Column.column [] [ a [ Href "mailto: xeroeffortclub@gmail.com" ] [ 
                                            Level.level [] [
                                                Level.item [] [
                                                    Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ]
                                                    div [] [ str "XeroEffortClub@GMail.com" ] 
                                                ]
                                            ]
                                        ] ]
                                        Column.column [] [ p [] [ str "Coming very soon..." ] ]
                                        // Column.column [] [ p [] [ str "Follow, Like & Share - Xero Effort" ] ]
                                        // Column.column [] [ a [ Href "https://www.instagram.com/xeroeffort/" ] [ 
                                        //     Level.level [] [
                                        //         Level.item [] [
                                        //             Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/IG.png" ] ]
                                        //             div [] [ str "@XeroEffort" ]
                                        //         ]
                                        //     ]
                                        // ] ]
                                    ]
                                ]
                            ]
                            Tile.child [ Tile.Size Tile.Is5 ] [
                                Column.column [] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png" ] ] ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]