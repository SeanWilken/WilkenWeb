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
    Container.container [ Container.Props [ ClassName "" ] ] [
        Level.level [] [
            // columns not aligned / same height, need custom class?
            Columns.columns [] [
                Column.column [] [
                    Container.container [ Container.Props [ ClassName "contactContentCard" ] ] [
                        div [ClassName "contentCardTextBackground"; Style [Padding 25] ] [
                            h1 [] [ str "Sean Wilken"]
                            p [ Style [Margin 10; FontSize 18;] ] [ str "Drop me a line with your questions or comments about the website or it's features. Feel free to also send a friendly hello. Just don't spam me."] // STYLE THIS??
                            a [ Href "mailto: sean.d.wilken@gmail.com" ] [
                                Level.level [] [
                                    Level.item [] [ 
                                        Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png"; ] ] 
                                        div [] [str "Sean.D.Wilken@GMail.com"]
                                    ]
                                ]
                            ]
                            Column.column [] [ Container.container [] [ Image.image [] [ img [ Src "./imgs/Misfortune.png"; ] ] ] ]
                        ]
                    ]
                ]
                // CARDS ARE NOT ALIGNED AS THERE IS DIFFERENT AMOUNTS OF TEXT
                Column.column [] [ 
                    Container.container [ Container.Props [ ClassName "contactContentCard"; Style [Height "100%"] ] ] [// fixes not being aligned properly
                        div [ClassName "contentCardTextBackground"; Style [Padding 25] ] [
                            h1 [] [str "Xero Effort"] 
                            p [ Style [Margin 10; FontSize 18;] ] [ str "Coming very soon, to platforms near you."] // STYLE THIS??
                            // p [ Style [Margin 10; FontSize 18;] ] [ str "Check out Xero Effort & remember:"]
                            // p [ Style [Margin 10; FontSize 18;] ] [ str "Follow, Like & Share"]
                            // a [ Href "https://www.instagram.com/xeroeffort/"; ] [ 
                            //     Level.level [] [
                            //         Level.item [] [
                            //             Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/IG.png"; ] ]
                            //             div [] [ str "@XeroEffort" ]
                            //         ]
                            //     ]
                            // ]
                            a [ Href "mailto: xeroeffortclub@gmail.com" ] [ 
                                Level.level [] [
                                    Level.item [] [
                                        Image.image [Image.Is64x64] [ img [ Src "./imgs/icons/Mail.png"; ] ]
                                        div [] [ str "XeroEffortClub@GMail.com"] 
                                    ]
                                ]
                            ]
                            Column.column [] [ Container.container [] [ Image.image [] [ img [ Src "./imgs/Out for Blood.png"; ] ] ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]