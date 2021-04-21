module Contact

open Fable.React
open Fable.React.Props
open Fulma

type AnchorLink = {
    Hyperlink: string
    LinkIcon: string
    LinkTitle: string
}

let contactHeaderTitle = "Looking to tell me something?"
let contactHeaderBlurbs = [
    "Drop a line to the appropriate entity.."
    "Whatever it is, NO spam!"
]
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

// View -----------

// should fill more of the screen vertically when on desktop views....
let contactChildSplitTile title ( tileBullets: ( string * AnchorLink option ) list ) =
    Tile.child [] [
        div [ ClassName "generalViewTitleCard" ] [
            Container.container [] [
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
    ]

// CONTACT: -> CONTENT -> {GMAIL / LINKEDIN / (IG / FACEBOOK / TWITTER / ETC..?)}
let view = 
    SharedViewModule.sharedSplitView
        ( SharedViewModule.sharedSplitHeader contactHeaderTitle contactHeaderBlurbs )
        ( contactChildSplitTile "Sean Wilken" swContactEmailAnchor )
        ( contactChildSplitTile "Xero Effort" xeContactEmailAnchor )