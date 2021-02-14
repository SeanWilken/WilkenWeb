module SharedViewModule

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma

// obsolete, use modal background / close button?
let backToGallery dispatchMsg dispatch =
    Level.level [] [
        Level.item [] [ a [ ClassName "backToGallery"; OnClick ( fun _ -> dispatchMsg |> dispatch ) ] [ p [] [ str "Exit" ] ] ]
    ]

// GRAY OUT OR NON HOVER / SELECTABLE IF AT 0 OR MAX INDEX?
let bigNavButton clickFunc (name: string) dispatch =
    a [ OnClick ( fun _ -> clickFunc |> dispatch ) ] [
        Container.container [ Container.Props [ ClassName "bigSectionNavigationButton" ] ] [
            Columns.columns [ Columns.Props [ ClassName "sectionNavButtonCols" ] ] [
                for char in name do
                    Column.column [] [ p [] [ str (string (char)) ] ]
            ]
        ]
    ]

let sourceCodeRawGitControls soureCodeLinks =
    div [ClassName "contentCardTextBackground"] [
        p [] [ str "Source Code"]
        Columns.columns [ Columns.IsVCentered ] [
            for sourceCodeTitle, sourceCodeLink in soureCodeLinks do
                Column.column [] [ a [ Href sourceCodeLink ] [ h2 [] [ str sourceCodeTitle ] ] ]
        ]
    ]

// GENERIC MODAL FOR GAMES OR { PRODUCTS (NOT EVEN A WIP YET...) }
// WIP, NEED TO RETHINK AND FIGURE OUT HOW I WANT TO PASS CALLS, TITLES, DATA TO THESE
let checkHeaderString (headerString : string) =
    if headerString.Contains(".jpg") 
        then Column.column [] [ Image.image [Image.Is64x64] [ img [Src ( sprintf "./imgs/products/%s" headerString ) ] ] ]
        else Column.column [] [ h1 [] [ str headerString ] ]

let sharedModalHeaderControls modalTitle closeMsg dispatch = //stringList
    Level.level [] [
        Level.left [] [ h1 [ ClassName "modalTitle" ] [ str modalTitle ] ]
        Level.item [] [] //Columns.columns [] [ for string in stringList do checkHeaderString string ]
        Level.right [] [ a [ ClassName "closeModal"; OnClick ( fun _ -> closeMsg |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ] ]
    ]

let sharedModalLeft descriptionList (linkList: (string * string) list ) =
    div [ClassName "modalLeft"] [
        Container.container [] [
            for description in descriptionList do p [] [ str description ]
            if linkList.Length > 0 then sourceCodeRawGitControls linkList
        ]
    ]

// think about how to do this so it can be generic enough to support single action items, or generate list of action items with title (sizes, colors, level selection, etc..)
// Title -> Title, ( ActionTitle, ActionMsg ) list ->
// type ActionMsg = {
//     ActionTitle : string
//     ActionMsgs : (string * <Msg> ) list
// }

// let checkModalActionITem ( actionTitle : string ) actionMsgs dispatch =
//     if actionMsgs.Length > 1 
//         then Column.column [] [
//             str actionTitle
//             div [] [
//                 for actionTitle, actionMsg in actionMsgs do
//                     a [ OnClick ( fun _ -> action |> dispatch ); ] [ str actionTitle ]
//             ]
//         ]
//         else Column.column [] [
//             a [ OnClick ( fun _ -> actionMsgs.Item(0).actionMsg |> dispatch ); ] [ str actionMsgs.Item(0).actionTitle ]
//         ]

// let sharedModalRight actionDictionary dispatch =
//     Columns.columns [] [
//         for actionTitle, actionMsgs in actionDictionary do
//             checkModalActionITem actionTitle actionMsgs dispatch
//     ]

let sharedModal modalHeader modalLeft modalContent modalRight =
    Modal.modal [ Modal.IsActive true; Modal.Props [ Style [ Width "100%"; Height "100%" ] ] ] [
        Modal.background [] []
        Modal.content [ Props [ ClassName "modalContent" ] ] [
            Tile.ancestor [] [ Tile.parent [Tile.Size Tile.Is12] [ Tile.child [] [ modalHeader ] ] ]
            Tile.ancestor [] [
                Tile.parent [Tile.Size Tile.Is3] [ Columns.columns [Columns.IsVCentered] [ Column.column [] [ Tile.child [] [ modalLeft ] ] ] ]
                Tile.parent [Tile.Size Tile.Is6] [ Tile.child [] [ modalContent ] ]
                Tile.parent [Tile.Size Tile.Is3] [ Columns.columns [Columns.IsVCentered] [ Column.column [] [ Tile.child [] [ modalRight ] ] ] ]
            ]
        ]
    ]