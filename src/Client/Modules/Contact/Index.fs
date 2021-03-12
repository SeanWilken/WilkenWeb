module Contact

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma



    
let ContactHeader =
    Tile.ancestor [] [
        Tile.parent [] [
            Tile.child [ Tile.Size Tile.Is12 ] [
                Container.container [ Container.Props [ ClassName "contactContentCard" ] ] [
                    div [ ClassName "contactCardBackground" ] [ 
                        p [] [ str """Say hello or drop me a line with your questions or comments
                                 about the website and it's features..Whatever it is, 
                                 just please DO NOT spam me.""" ]                                 
                    ]
                ]
            ]
        ]
    ]

let ContactSplitView =
    div [ ClassName "portfolioSectionSelectionContainer" ] [
        Tile.ancestor [] [
            Tile.parent [ Tile.Size Tile.Is6 ] [
                Tile.child [] [
                    Container.container [ Container.CustomClass "contactCard" ] [
                        div [ ClassName "contactCardBackground" ] [ h1 [] [ str "Sean Wilken" ]  ]
                        Level.level [] []
                        div [ ClassName "contactCardBackground" ] [ 
                            Level.level [] [
                                p [] [ str "Got something to say to my face?" ] 
                                a [ Href "mailto: sean.d.wilken@gmail.com" ] [ 
                                    Level.item [] [
                                        Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ]
                                        div [] [ str "Sean.D.Wilken@GMail.com" ] 
                                    ]
                                ]
                            ]
                            Level.level [] [
                                Level.item [] [
                                    p [] [ str "Credentials coming soon to resumes near you..." ] 
                                ]
                                // p [] [ str "Show me off to your boss..." ] 
                                // a [ Href "mailto: sean.d.wilken@gmail.com" ] [ // download resume resource
                                //     Level.item [] [
                                //         Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ] // Generic Person Icon
                                //         div [] [ str "Resume" ] 
                                //     ]
                                // ]
                            ]
                        ]
                    ]
                ]
            ]
            Tile.parent [ Tile.Size Tile.Is6 ] [
                Tile.child [] [
                    Container.container [ Container.CustomClass "contactCard" ] [
                        div [ ClassName "contactCardBackground" ] [ h1 [] [ str "Xero Effort" ]  ]
                        Level.level [] []
                        div [ ClassName "contactCardBackground" ] [ 
                            Level.level [] [
                                Level.item [] [
                                    p [] [ str "Coming very soon to social platforms near you..." ]
                                ]
                                // p [] [ str "Follow, Like & Share. It takes..." ] 
                                // a [ Href "https://www.instagram.com/xeroeffort/" ] [ 
                                //     Level.item [] [
                                //         Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/IG.png" ] ]
                                //         div [] [ str "@XeroEffort" ]
                                //     ]
                                // ]
                            ]
                            Level.level [] [
                                p [] [ str "Complement or complaint..." ] 
                                a [ Href "mailto: xeroeffortclub@gmail.com" ] [ 
                                    Level.item [] [
                                        Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ]
                                        div [] [ str "XeroEffortClub@GMail.com" ] 
                                    ]
                                ]
                            ]
                        ]
                    ]     
                ]
            ]
        ]
    ]

let viewSplit =
    div [] [
        ContactHeader
        ContactSplitView
    ]

let viewColumns =
    Columns.columns [Columns.IsDesktop] [
        // CONTACT HEADER
        Column.column [] [ 
            Tile.ancestor [ Tile.Props [ ClassName "contactContentCard" ] ] [
                div [ ClassName "contactCardBackground" ] [ 
                    p [] [ str """Say hello or drop me a line with your questions or comments
                                 about the website and it's features..Whatever it is, 
                                 just please DO NOT spam me.""" ]                                 
                ]
            ]
        ]
        // SEAN WILKEN
        Column.column [] [
            Container.container [ Container.CustomClass "contactCard" ] [
                div [ ClassName "contactCardBackground" ] [ h1 [] [ str "Sean Wilken" ]  ]
                Level.level [] []
                div [ ClassName "contactCardBackground" ] [ 
                    Level.level [] [
                        p [] [ str "Howdy there." ] 
                        a [ Href "mailto: sean.d.wilken@gmail.com" ] [ 
                            Level.item [] [
                                Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ]
                                div [] [ str "Sean.D.Wilken@GMail.com" ] 
                            ]
                        ]
                    ]
                ]
            ]
        ]
        // XERO EFFORT
        Column.column [] [
            Container.container [ Container.CustomClass "contactCard" ] [
                div [ ClassName "contactCardBackground" ] [ h1 [] [ str "Xero Effort" ]  ]
                Level.level [] []
                div [ ClassName "contactCardBackground" ] [ 
                    Level.level [] [
                        p [] [ str "Coming very soon to social platforms near you..." ]
                        // p [] [ str "Follow, Like & Share" ] 
                        // a [ Href "https://www.instagram.com/xeroeffort/" ] [ 
                        //     Level.item [] [
                        //         Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/IG.png" ] ]
                        //         div [] [ str "@XeroEffort" ]
                        //     ]
                        // ]
                    ]
                    Level.level [] [
                        p [] [ str "Let's hear it.." ] 
                        a [ Href "mailto: xeroeffortclub@gmail.com" ] [ 
                            Level.item [] [
                                Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Mail.png" ] ]
                                div [] [ str "XeroEffortClub@GMail.com" ] 
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

// CONTACT: -> CONTENT -> {GMAIL / LINKEDIN / (IG / FACEBOOK / TWITTER / ETC..?)}
// Static Page:
// does not depend on the dispatch loop, as currently there are no messages that would need to be submitted into it.
let view = 
    viewSplit
    // viewColumns // looks bad on desktop, good on mobile

    // -- OR --


