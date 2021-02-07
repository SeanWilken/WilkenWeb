module SharedModule

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

// let levelSelector  allLevels dispatch = //currentLevel not tracked?
//     Level.item [ Level.Item.HasTextCentered; ] [
//         Level.item [] [ p [] [ str "Difficulty:"] ]
//         for level in allLevels do
//             // if level = currentLevel then 
//             //     Level.item [] [ //Level.Item.HasTextCentered;
//             //         p [] [sprintf "Level %i" level] // STYLE THIS?? //Style [Padding 15;]
//             //     ]
//             // else 
//             Level.item [] [ //Level.Item.HasTextCentered;
//                 a [ OnClick(fun _ -> LoadLevel level |> dispatch )] [ str (sprintf "Level %i" level) ] // STYLE THIS?? //Style [Padding 15;]
//             ]
//     ]

// // DIFFICULTY SELECTION & ROUND CONTROLS
// let tileSortHeaderControls difficulty dispatch =
//     Container.container [ Container.Props [ ClassName "gameGridControlBar"] ] [
//         Level.level [] [
//             levelSelector [0..3] dispatch
//             Level.item [] [ a [ OnClick(fun _ -> NewRound |> dispatch ); ] [ str "New Round" ] ] // STYLE THIS?? //Padding 10;
//             Level.item [] [ a [ OnClick(fun _ -> ResetRound |> dispatch ); ] [ str "Reset Round" ] ] // STYLE THIS??
//             Level.item [] [ a [ OnClick(fun _ -> RewindMove |> dispatch ); ] [ str "Undo Turn" ] ] // STYLE THIS??
//         ]
//     ]