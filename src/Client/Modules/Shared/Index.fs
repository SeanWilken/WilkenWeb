module SharedViewModule

open Fable.React
open Fable.React.Props
open Fulma

// obsolete, use modal background / close button?
let backToGallery dispatchMsg dispatch =
    Level.level [] [
        Level.item [] [ a [ ClassName "backToGallery"; OnClick ( fun _ -> dispatchMsg |> dispatch ) ] [ p [] [ str "Exit" ] ] ]
    ]

// GRAY OUT OR NON HOVER / SELECTABLE IF AT 0 OR MAX INDEX?
let bigNavButton clickFunc ( name: string ) dispatch =
    a [ OnClick ( fun _ -> clickFunc |> dispatch ) ] [
        Container.container [ Container.Props [ ClassName "bigSectionNavigationButton" ] ] [
            Columns.columns [ Columns.Props [ ClassName "sectionNavButtonCols" ] ] [
                for char in name do
                    Column.column [] [ p [] [ str ( string ( char ) ) ] ]
            ]
        ]
    ]

// Game MODAL 2.0

let modalContent content =
    Column.column [] [ content ]

let codeModalHeader gameTitle msg dispatch =
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

let codeModalControlSelections controlActions dispatch =
    Column.column [] [
    for actionLabel, actionMsg in controlActions do 
        div [] [
            a [ OnClick ( fun _ -> actionMsg |> dispatch ); ] [ 
                str actionLabel 
            ]
        ]
    ]

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

let codeModalFooter controlList dispatch =
    div [] [
        Level.level [Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            for controlTitle, controlMsg in controlList do    
                Level.item [] [
                    div [ ClassName "galleryTitleCard"; OnClick ( fun _ -> controlMsg |> dispatch ) ] [ h1 [] [ str controlTitle ] ]
                ]
        ]
    ]

let sharedViewModal header content footer =
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [] []
        div [ ClassName "modalContent" ] [
            header
            content
            footer
        ]
    ]