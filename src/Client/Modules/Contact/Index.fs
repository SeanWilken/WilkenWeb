module Contact

open Fable.React
open Fable.React.Props
open Fulma

type AnchorLink = {
    Hyperlink: string
    LinkIcon: string
    LinkTitle: string
}

let swContactEmailAnchor =
    [   
        "Got something to say?",
        Some { 
            Hyperlink = "mailto: sean.d.wilken@gmail.com"
            LinkIcon = "./imgs/icons/Mail.png"
            LinkTitle = "Sean.D.Wilken@GMail.com" 
        };
        "Credentials coming soon to resumes near you...",
        None;
    ]

let xeContactEmailAnchor = 
    [ 
        "Complement or complaint...",
        Some { 
            Hyperlink = "mailto: xeroeffortclub@gmail.com"
            LinkIcon = "./imgs/icons/Mail.png"
            LinkTitle = "XeroEffortClub@GMail.com" 
        };
        "Coming very soon to social platforms near you...",
        None;
        // "Follow, Like & Share...",
        // Some { 
        //     Hyperlink = "https://www.instagram.com/xeroeffort/"
        //     LinkIcon = "./imgs/icons/IG.png"
        //     LinkTitle = "@XeroEffort" 
        // };
    ]

// should have style different than cards
let contactHeader =
    Tile.ancestor [] [
        Tile.parent [] [
            Tile.child [ Tile.Size Tile.Is12 ] [
                div [ ClassName "viewTitleCard" ] [ 
                    h1 [] [ str "Looking to tell me something?" ]
                    h2 [] [ str "Drop a line to the appropriate entity.." ]
                    h2 [] [ str "Whatever it is, NO spam!" ]
                ]
            ]
        ]
    ]

// style is not to my liking..
// should fill more of the screen vertically when on desktop views....
let childSplitTile title ( tileBullets: ( string * AnchorLink option ) list ) =
    Tile.child [ Tile.IsVertical; Tile.CustomClass "splitCard" ] [ 
        div [ ClassName "viewTitleCard" ] [ 
            h1 [] [ str title ]
            if not tileBullets.IsEmpty then br []
            for tileString, anchorLink in tileBullets do
                p [] [ str tileString ]
                match anchorLink with
                | Some link ->
                    a [ Href link.Hyperlink ] [ 
                        Level.item [] [
                            Image.image [ Image.Is64x64 ] [ img [ Src link.LinkIcon ] ]
                            div [] [ str link.LinkTitle ] 
                        ]
                    ]
                | None -> ()
                br []
        ]
    ]

let contactSplitView =
    div [] [
        Tile.ancestor [] [
            Tile.parent [ Tile.Size Tile.Is6 ] [
                childSplitTile "Sean Wilken" swContactEmailAnchor
            ]
            Tile.parent [ Tile.Size Tile.Is6 ] [
                childSplitTile "Xero Effort" xeContactEmailAnchor
            ]
        ]
    ]

let viewSplit =
    div [] [
        contactHeader
        contactSplitView
    ]

// CONTACT: -> CONTENT -> {GMAIL / LINKEDIN / (IG / FACEBOOK / TWITTER / ETC..?)}
let view = 
    viewSplit