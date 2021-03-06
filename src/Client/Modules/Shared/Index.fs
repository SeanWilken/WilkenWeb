module SharedViewModule

open Fable.React
open Fable.React.Props
open Fulma

// CodeGallery
let backToGallery dispatchMsg dispatch =
    div [] [
        a [ ClassName "relativeBackButton"; OnClick ( fun _ -> dispatchMsg |> dispatch ) ] [ 
            Image.image [ Image.Is48x48 ] [ img [ Src "./imgs/icons/X-it.png" ] ] 
        ]
    ]

// GRAY OUT OR NON HOVER / SELECTABLE IF AT 0 OR MAX INDEX?
// 'disabled' parameter for the above
// Left & Right Buttons for Art Gallery Modal (..innerWidth >= 900px..)
let bigNavButton clickFunc ( name: string ) dispatch = 
    a [ OnClick ( fun _ -> clickFunc |> dispatch ) ] [
        Container.container [ Container.Props [ ClassName "bigSectionNavigationButton" ] ] [
            Columns.columns [ Columns.Props [ ClassName "sectionNavButtonCols" ] ] [
                for char in name do
                    Column.column [] [ p [] [ str ( string ( char ) ) ] ]
            ]
        ]
    ]

// Modal-----
// Games
let gameModalContent content =
    Column.column [ Column.Props [ ClassName "gameGridContainer" ] ] [ content ]

// About || Games
let sharedModalHeader gameTitle msg dispatch =
    div [] [
        Level.level [ Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            Level.left [] [
                // a [] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
                // span [ ClassName "modalExternalLink" ] [ a [ Href "github" ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Github.png" ] ]; ] ] //p [] [ str "GitHub" ] 
            ]
            Level.item [] [
                div [ ClassName "galleryTitleCard"; ] [ h1 [] [ str gameTitle ] ]
            ]
            Level.right [] [
                a [ OnClick ( fun _ -> msg |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
            ]
        ]
    ]

// Helper for codeModalControlsContent
let codeModalControlSelections controlActions dispatch =
    Column.column [] [
    for actionLabel, actionMsg in controlActions do 
        div [] [
            a [ OnClick ( fun _ -> actionMsg |> dispatch ); ] [ 
                str actionLabel 
            ]
        ]
    ]

// Goal Roll + Tile Sort | TileTap Overridden for special window.setInterval dispatch
let codeModalControlsContent controlList dispatch =
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ ClassName "modalAltContent" ] [
                        Container.container [] [
                            codeModalControlSelections controlList dispatch
                        ]
                    ]
                ]
            ]
        ]

// Games
let codeModalInstructionContent instructionList =
    Column.column [] [
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                div [ ClassName "modalAltContent" ] [
                    Container.container [] [
                        for instruction in instructionList do 
                            p [] [ str instruction ]
                    ]
                ]
            ]
        ]
    ]

// Goal Roll + Tile Sort | TileTap Overridden for special window.setInterval dispatch
let codeModalFooter controlList dispatch =
    div [] [
        Level.level [Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            for controlTitle, controlMsg in controlList do    
                Level.item [] [
                    div [ ClassName "galleryTitleCard"; OnClick ( fun _ -> controlMsg |> dispatch ) ] [ h1 [] [ str controlTitle ] ]
                ]
        ]
    ]

//won view??

open Browser
// Helper function for determining specific viewport; some views were being cut off
let viewPortModalBreak =
    window.innerWidth >= 900.0

// Games || ArtGallery
let sharedViewModal isActive header content footer =
    Modal.modal [ Modal.IsActive isActive ] [
        Modal.background [] []
        if viewPortModalBreak
            then 
                Modal.content [] [ header; content; footer ]
            else 
                div [ ClassName "modalContent" ] [ header; content; footer ]
    ]

// Split View-----
// Portfolio Landing | Contact
let sharedSplitHeader title contentBlurb =
    Tile.ancestor [] [
        Tile.parent [] [
            Tile.child [ Tile.Size Tile.Is12 ] [
                div [ ClassName "viewTitleCard" ] [ 
                    h1 [] [ str title ]
                    for blurb in contentBlurb do
                        h2 [] [ str blurb ]
                ]
            ]
        ]
    ]
// Portfolio Landing | Contact
let sharedSplitView header childLeft childRight =
    div [ Style [ PaddingTop 50 ] ] [
        header
        Tile.ancestor [] [
            // use more verticalSpace on above tablet viewPorts
            // let style = if viewPortModalBreak then Style [ MinHeight 600 ] else Style []
            Tile.parent [ Tile.Size Tile.Is6 ] [ childLeft ]
            Tile.parent [ Tile.Size Tile.Is6 ] [ childRight ]
        ]
    ]