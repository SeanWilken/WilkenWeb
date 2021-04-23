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


let sharedSwitchSectionButton msg buttonString dispatch =
    button [ ClassName "flatButton"; OnClick ( fun _ -> msg |> dispatch ) ] [ str buttonString ]  

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
            Level.item [] [ div [ ClassName "headerTitle"; ] [ h1 [] [ str gameTitle ] ] ]
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
        Column.column [] [ 
            Columns.columns [ Columns.IsVCentered ] [ 
                Tile.child [] [ 
                    div [ ClassName "modalAltContent" ] [
                        Container.container [] [
                            codeModalControlSelections controlList dispatch
                        ]
                    ]
                ]
            ]
        ]

let roundCompleteContent gameStatDetails = //levelId moveCount
    div [ ClassName "levelCompletedCard" ] [ 
        Container.container [] [ str "Round Over!" ]
        Container.container [ Container.Props [ Style [ FontSize 20 ] ] ] [
            h2 [ Style [ FontSize 40; Color "#FFF" ] ] [ str "Details: "]
            for gameStat in gameStatDetails do
                p [ Style [ Padding 5; Color "#FFF" ] ] [ str gameStat ]
            // div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( "Round ID: " + levelId ) ]
            // div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( "# of Moves: " + moveCount ) ]
        ]
    ]
    // div [ ClassName "levelCompletedCard" ] [ 
    //     Container.container [ Container.Props [] ] [
    //         str "Round Over!"
    //     ]
    //     Container.container [ Container.Props [ Style [ FontSize 20; ] ] ] [
    //         h2 [ Style [ FontSize 40; Color "#FFF" ] ] [ str "Round Stats: " ]  
    //         div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( "Round Score: " + string model.CoinsCollected ) ]
    //         div [ Style [ Padding 5; Color "#69A69A" ] ] [ str ( "Round Timer: " + ( SharedViewModule.gameTickClock model.GameClock ) ) ]
    //     ]
    //     // Container.container [ Container.Props [ Style [ FontSize 20; Padding 20 ] ] ] [
    //     //     h2 [ Style [ FontSize 50; Color "#FF2843" ] ] [ str "Details: "]
    //     //     roundOverString model
    //     // ]
    // ]

open Browser

let stopGameLoop loopFloat = window.clearInterval(loopFloat)

let gameTickClock ticks =
    string (ticks / 4)

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

let gameInstructionContent instructionList =
    div [ ClassName "modalAltContent" ] [
        Container.container [] [
            for instruction in instructionList do 
                p [] [ str instruction ]
        ]
    ]

// Goal Roll + Tile Sort | TileTap Overridden for special window.setInterval dispatch
let codeModalFooter controlList dispatch =
    div [] [
        Level.level [Level.Level.IsMobile; Level.Level.Props [ Style [ PaddingTop 10 ] ] ] [
            for controlTitle, controlMsg in controlList do    
                Level.item [] [
                    div [ ClassName "modalControls"; OnClick ( fun _ -> controlMsg |> dispatch ) ] [ h1 [] [ str controlTitle ] ]
                ]
        ]
    ]

//won view??
let galleryHeaderControls hrefLink hrefImg msg dispatch =
    div [] [
        a [ ClassName "closeModal"; OnClick ( fun _ -> msg |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
        span [ ClassName "modalExternalLink" ] [ a [ Href hrefLink ] [ Image.image [ Image.Is64x64 ] [ img [ Src hrefImg ] ]; ] ]
    ]

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
                div [ ClassName "generalViewTitleCard" ] [ 
                    h1 [] [ str title ]
                    for blurb in contentBlurb do
                        h2 [] [ str blurb ]
                ]
            ]
        ]
    ]
// Portfolio Landing | Contact
let sharedSplitView header childLeft childRight =
    div [ ClassName "paddedContainerHeader" ] [
        header
        Tile.ancestor [ Tile.Props [ Style [ Height "40vh" ] ] ] [
            Tile.parent [ Tile.Size Tile.Is6 ] [ childLeft ]
            Tile.parent [ Tile.Size Tile.Is6 ] [ childRight ]
        ]
    ]