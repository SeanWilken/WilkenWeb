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

// REFACTOR GIT CONTROLS INTO LINK CONTAINER -> WORKS WITH MULTIPLE SELECTION OPTIONS
// SHOULD RUN VERTICALLY DOWN, NOT HORIZONTALLY ACROSS
let sourceCodeRawGitControls soureCodeLinks =
    div [ ClassName "contentCardTextBackground" ] [
        p [] [ str "Source Code" ]
        Columns.columns [ Columns.IsVCentered ] [
            for sourceCodeTitle, sourceCodeLink in soureCodeLinks do
                Column.column [] [ a [ Href sourceCodeLink ] [ h2 [] [ str sourceCodeTitle ] ] ]
        ]
    ]

// GENERIC MODAL FOR GAMES OR { PRODUCTS (NOT EVEN A WIP YET...) }
    // REALLY SHOULD BE REFACTORED EVEN FURTHER TO SUPPORT LOADING ART GALLERY MODAL ALSO, MATCH CASE BASED ON MODAL / SECTION IT IS WORKING WITH
    // both sections can use the generic deconstructor of action list to multi option selection (difficulty, level selection, etc..)


// SHARED CONTENT VIEW: CARD STYLE -- START

// CONTENT CARD STYLE (1st iteration)
// content Header
let contentHeaderCard headerTitle sourceCodeLinks descriptions =
    Container.container [ Container.Props [ ClassName "aboutContentCard" ] ] [
        Container.container [] [
            Columns.columns [ Columns.IsVCentered ] [
                Column.column [] [ h1 [] [ str headerTitle ] ] // WAS IN THE CENTER COLUMN
                sourceCodeRawGitControls sourceCodeLinks
                // for sourceTitle, sourceLink in sourceCodeLinks do a [ Href sourceLink ] [ h2 [] [ str sourceTitle ] ]
            ]
            for description in descriptions do p [] [ str description ]
        ]
    ]




// type ContentControlOption =
//     | ActionItem of actionLabel, actionMsg
//     | ActionItemList of actionLabelTitle, ActionItemList

// Title -> ActionMsg
// Title -> [ OptionTitle, OptionMessage; ... ]

// multiselect option
// CURRENTLY ONLY TILE SORT USES 'DIFFICULTY' AS A LEVEL IDENTIFIER
// SOMEHOW NEEDS TO BE GENERATED BY FOR LOOP IN ACTION LIST
// // DIFFICULTY SELECTOR CONTROLS
// let contentDifficultySelector ( currentDifficulty: TileSortDifficulty ) tileDifficulties dispatch =
//     Level.item [ Level.Item.HasTextCentered; ] [
//         Level.item [] [ p [] [ str "Difficulty:"] ]
//         for difficulty in tileDifficulties do
//             let unionCaseDifficultyName = difficulty.Name
//             let unionCaseDifficulty =  decodeDifficultyByString unionCaseDifficultyName
//             if unionCaseDifficulty = currentDifficulty then 
//                 Level.item [ Level.Item.HasTextCentered ] [ p [] [ str unionCaseDifficultyName ] ]
//             else 
//                 Level.item [ Level.Item.HasTextCentered ] [ a [ OnClick ( fun _ -> UpdateDifficulty unionCaseDifficulty |> dispatch ) ] [ str unionCaseDifficultyName ] ]
//     ]
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

// main controls for header, would generate multiselects if necesary with above func... WIP: TODO
let contentHeaderControls actionList dispatch = //difficulty
    Container.container [ Container.Props [ ClassName "gameGridControlBar"] ] [
        Columns.columns [ Columns.IsMultiline; Columns.IsMobile ] [
        // Level.level [Level.Level.IsMobile] [
            // difficultySelector difficulty dispatch
            for actionLabel, actionMsg in actionList do Column.column [] [ a [ OnClick ( fun _ -> actionMsg |> dispatch ) ] [ str actionLabel ] ]
            // for actionLabel, actionMsg in actionList do Level.item [] [ a [ OnClick ( fun _ -> actionMsg |> dispatch ) ] [ str actionLabel ] ]
        ]
    ]

// main card view
let sharedContentCardView headerCard headerControls mainContent dispatch =
    div [] [
        // backToGallery QuitGame dispatch
        headerCard
        headerControls // model.Difficulty dispatch
        mainContent // the content within the actual module will handle game state
    ]

// SHARED CONTENT VIEW: CARD STYLE -- END


// SHARED CONTENT VIEW: MODAL STYLE -- START

// Modal Header
// WIP, NEED TO RETHINK AND FIGURE OUT HOW I WANT TO PASS CALLS, TITLES, DATA TO THESE
// ugly throwing it at the wall
let checkHeaderString ( headerString : string ) =
    if headerString.Contains( ".jpg" ) 
        then Column.column [] [ Image.image [ Image.Is64x64 ] [ img [ Src ( sprintf "./imgs/products/%s" headerString ) ] ] ]
        else Column.column [] [ h1 [] [ str headerString ] ]
        // OTHER BUTTONS IN HEADERS IS WIP
let sharedModalHeaderControls modalTitle closeMsg dispatch = //stringList
    Tile.parent [ Tile.Size Tile.Is12 ] [ 
        Tile.child [] [ 
            Level.level [] [
                Level.left [] [ h1 [ ClassName "modalTitle" ] [ str modalTitle ] ]
                Level.item [] [] //Columns.columns [] [ for string in stringList do checkHeaderString string ]
                Level.right [] [ a [ ClassName "closeModal"; OnClick ( fun _ -> closeMsg |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ] ]
            ]
        ]
    ]

// Modal Left
let sharedModalLeft descriptionList (linkList: (string * string) list ) =
    // Tile.parent [ Tile.Size Tile.Is2 ] [ 
    Column.column [] [
        Columns.columns [ Columns.IsVCentered ] [ 
            Column.column [] [ 
                // Tile.child [] [ 
                    div [ ClassName "modalAltContent" ] [
                        Container.container [] [
                            for description in descriptionList do p [] [ str description ]
                            // if linkList.Length > 0 then sourceCodeRawGitControls linkList
                        ]
                    ]
                // ]
            ]
        ]
    ]

// Main Modal Content (Center)
let modalContent content =
    Column.column [] [ 
            content 
        // Level.level [ Level.Level.CustomClass "modalContent" ] [ Level.item [] [ 
        //     ] ] 
    ]
    // Tile.parent [ Tile.Size Tile.Is6 ] [ Level.level [ Level.Level.CustomClass "modalContent" ] [ Level.item [] [ Tile.child [] [ content ] ] ] ]


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

// Content Right
let modalRightControls controlActions dispatch =
    Column.column [] [
        for actionLabel, actionMsg in controlActions do div [ ClassName "mainContainer" ] [ a [ OnClick ( fun _ -> actionMsg |> dispatch ); ] [ str actionLabel ] ] ]
let sharedModalRight actionList dispatch =
    Tile.parent [ Tile.Size Tile.Is2 ] [ 
        Columns.columns [ Columns.IsVCentered; ] [ 
            Column.column [] [ 
                Tile.child [] [ 
                    div [ ClassName "modalAltContent" ] [
                        Container.container [] [
                            modalRightControls actionList dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
// Main
let sharedModal modalHeader modalLeft modalContent modalRight =
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [] []
        Modal.content [ Props [ ClassName "modalContent" ] ] [
            Tile.ancestor [] [ modalHeader ]
            // Tile.ancestor [  ] [
            Columns.columns [  ] [
                // Tile.parent [ Tile.Size Tile.Is1 ] []
                modalLeft
                modalContent
                modalRight
                // Tile.parent [ Tile.Size Tile.Is1 ] []
            ]
        ]
    ]

// SHARED CONTENT VIEW: MODAL STYLE -- END










// Game MODAL 2.0

let codeModalHeader gameTitle msg dispatch =
    div [] [
        Level.level [Level.Level.IsMobile; Level.Level.Props [ Style [PaddingTop 10] ] ] [
            Level.left [] [
                a [ ClassName ""; ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
                // span [ ClassName "likeOnInsta" ] [ a [ Href "github" ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/Github.png" ] ]; ] ] //p [] [ str "GitHub" ] 
            ]
            Level.item [] [
                div [ ClassName "galleryTitleCard"; ] [ h1 [] [ str gameTitle ] ]
            ]
            // for msg in msgs
            Level.right [] [
                a [ ClassName ""; OnClick ( fun _ -> msg |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
            ]
        ]
    ]

let codeModalControlSelections controlActions dispatch =
    Column.column [] [
    for actionLabel, actionMsg in controlActions do 
        div [ ClassName "mainContainer" ] [ 
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