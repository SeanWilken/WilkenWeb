module SharedViewModule

open FSharp
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Shared

// obsolete, use modal background / close button?
let backToGallery dispatchMsg dispatch =
    Level.level [] [
        Level.item [] [ a [ ClassName "backToGallery"; OnClick ( fun _ -> dispatchMsg |> dispatch ) ] [ p [] [ str "Exit" ] ] ]
    ]

// GAME CONTROLS
// let levelSelector currentLevel allLevels dispatch =
//     Level.item [ Level.Item.HasTextCentered; ] [
//         Level.item [] [ p [] [ str "Level Select:" ] ]
//         for level in allLevels do
//             if level = currentLevel then 
//                 Level.item [] [ p [] [ str ( sprintf "Lvl %i" level ) ] ]
//             else 
//                 Level.item [] [ 
//                     a [ OnClick ( fun _ -> LoadRound level |> dispatch ) ] [ str ( sprintf "Lvl %i" level ) ]
//                 ]
//     ]


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

// GENERIC MODAL FOR GAMES OR { PRODUCTS (NOT EVEN A WIP YET...) }
    // REALLY SHOULD BE REFACTORED EVEN FURTHER TO SUPPORT LOADING ART GALLERY MODAL ALSO, MATCH CASE BASED ON MODAL / SECTION IT IS WORKING WITH

// HEADER NAV --------------

// WIP, NEED TO RETHINK AND FIGURE OUT HOW I WANT TO PASS CALLS, TITLES, DATA TO THESE
// ugly throwing it at the wall
let checkHeaderString (headerString : string) =
    if headerString.Contains(".jpg") 
        then Column.column [] [ Image.image [Image.Is64x64] [ img [Src ( sprintf "./imgs/products/%s" headerString ) ] ] ]
        else Column.column [] [ h1 [] [ str headerString ] ]
// OTHER BUTTONS IN HEADERS IS WIP



let sharedModalHeaderControls modalTitle closeMsg dispatch = //stringList
    Tile.parent [Tile.Size Tile.Is12] [ 
        Tile.child [] [ 
            Level.level [] [
                Level.left [] [ h1 [ ClassName "modalTitle" ] [ str modalTitle ] ]
                Level.item [] [] //Columns.columns [] [ for string in stringList do checkHeaderString string ]
                Level.right [] [ a [ ClassName "closeModal"; OnClick ( fun _ -> closeMsg |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ] ]
            ]
        ]
    ]
// END -------------------

// MODAL LEFT SECTION
// REFACTOR GIT CONTROLS INTO LINK CONTAINER -> WORKS WITH MULTIPLE SELECTION OPTIONS
// SHOULD RUN VERTICALLY DOWN, NOT HORIZONTALLY ACROSS
let sourceCodeRawGitControls soureCodeLinks =
    div [ClassName "contentCardTextBackground"] [
        p [] [ str "Source Code"]
        Columns.columns [ Columns.IsVCentered ] [
            for sourceCodeTitle, sourceCodeLink in soureCodeLinks do
                Column.column [] [ a [ Href sourceCodeLink ] [ h2 [] [ str sourceCodeTitle ] ] ]
        ]
    ]
let sharedModalLeft descriptionList (linkList: (string * string) list ) =
    Tile.parent [ Tile.Size Tile.Is3 ] [ 
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ClassName "modalLeft"] [
                        Container.container [] [
                            for description in descriptionList do p [] [ str description ]
                            if linkList.Length > 0 then sourceCodeRawGitControls linkList
                        ]
                    ]
                ]
            ]
        ]
    ]
// END -------------------


// MAIN MODAL CONTENT
let modalContent content =
    Tile.parent [Tile.Size Tile.Is6] [ Level.level [ Level.Level.CustomClass "modalContent" ] [ Level.item [] [ Tile.child [] [ content ] ] ] ]
// END -------------------

// MODAL RIGHT
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
// let optionSelector options dispatch =
//     Container.container [] [
//         p [] [ str "Level Select:" ]
//         for option in options do
//             if level = currentLevel then 
//                 Level.item [] [ p [] [ str ( sprintf "Lvl %i" level ) ] ]
//             else 
//                 a [ OnClick ( fun _ -> LoadRound level |> dispatch ) ] [ str ( sprintf "Lvl %i" level ) ]
//     ]
// match case with one item = actionLabel || more than 1 item = contentCard with title of actionLabel, mini contianer with subActions

let modalRightControls controlActions dispatch =
    Container.container [] [
        for actionLabel, actionMsg in controlActions do div [ ClassName "mainContainer"] [ a [ OnClick ( fun _ -> actionMsg |> dispatch ); ] [ str actionLabel ] ] ]
// END -------------------

// MODAL CONTENT RIGHT
let sharedModalRight actionList dispatch =
    Tile.parent [ Tile.Size Tile.Is3 ] [ 
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ClassName "modalLeft"] [
                        Container.container [] [
                            modalRightControls actionList dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]

// END -------------------

// MODAL
let sharedModal modalHeader modalLeft modalContent modalRight =
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [] []
        Modal.content [ Props [ ClassName "modalContent" ] ] [
            Tile.ancestor [] [ modalHeader ]
            Tile.ancestor [] [
                modalLeft
                modalContent
                modalRight
            ]
        ]
    ]
// END -------------------