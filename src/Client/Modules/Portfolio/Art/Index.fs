module ArtGallery

open FSharp
open Elmish
open System
open System.IO
open Fable.React
open Fable.React.Props
open Fulma

open Shared

// GALLERY POSTCARD APP -> SHARE THIS SITE (POSTCARD FROM THE INTERWEBS)
// RELATED TAGS BROWSER:
    // OTHER IMAGES ALSO FLAGGED FOR SAME STYLE / THEME; SELF DRIVE-RECOMENDATIONS
// PREVIOUS / NEXT BUTTONS FOR BROWSER:
    // OTHER IMAGES ALSO FLAGGED FOR SAME STYLE / THEME; SELF DRIVE-RECOMENDATIONS
// POSTCARD SENDER, SEND A TEMPLATE OF THE GALLERY ART WITH CUSTOM TEXT OR IMAGE AND EMAIL TO A FRIEND
    // SHARE THIS SITE SEND DIGITAL BUSINESS CARD / POSTCARD

// name, description tuple for gallery pieces.
// unable to read out folder file contents, System.IO not compatible with Fable
let galleryPieces = [
    "Out for Blood", "The hunt is on.";
    "BackStabber", "Never saw them comin'";
    "Bowing Bubbles", "What's poppin?";
    "Break Rules", "Not my rule, not my problem...";
    "Misfortune", "It was never in the cards to begin with.";
    // "Prismatic Bust", "Trippy, ain't she?";
    // "Smoke em", "Can't take 'em to the grave, amIright?";
    // "Favorite influencer", "What a bunch of drama queens.";
    // "Bossy", "Call it like you see it.";
]

type Msg =
    | BackToPortfolio
    | SetCurrentPieceIndex of int

let init() =
    SharedDesignGallery.getInitialModel, Cmd.none

let update msg (model : SharedDesignGallery.Model) =
    match msg with
    | SetCurrentPieceIndex msg ->
        let desiredPieceIndex = 
            if model.CurrentPieceIndex + msg < 0 
                then 0
            elif model.CurrentPieceIndex + msg > galleryPieces.Length - 1
                then model.CurrentPieceIndex
            else
                model.CurrentPieceIndex + msg
        {model with CurrentPieceIndex = desiredPieceIndex}, Cmd.none
    | _ ->
        model, Cmd.none

// HELPER FUNCTION TO BREAKDOWN THE GALLERY TUPLE
let getGalleryCardByIndex (index: int) =
    let piece, description = galleryPieces.Item(index)
    piece, description

// GRAY OUT OR NON HOVER / SELECTABLE IF AT 0 OR MAX INDEX?
let bigPrevious dispatch =
    Level.left [] [
        a [ OnClick (fun _ -> SetCurrentPieceIndex (-1) |> dispatch) ] [
            Container.container [ Container.Props [ ClassName "sectionNavigationPreviousButton" ] ] [
                Columns.columns [ Columns.Props [ Style [ FlexDirection "column"; FontFamily "Bungee"; Color "#FF2843"] ] ] [
                        Column.column [] [ p [] [str "P"] ]
                        Column.column [] [ p [] [str "R"] ]
                        Column.column [] [ p [] [str "E"] ]
                        Column.column [] [ p [] [str "V"] ]
                ]
            ]
        ]
    ]

let bigNext dispatch =
    Level.right [] [
        a [ OnClick (fun _ -> SetCurrentPieceIndex (1) |> dispatch) ] [
            Container.container [ Container.Props [ ClassName "sectionNavigationNextButton" ] ] [
                Columns.columns [ Columns.Props [ Style [ FlexDirection "column"; FontFamily "Bungee"; Color "#FF2843"] ] ] [
                    Column.column [] [ p [] [str "N"] ]
                    Column.column [] [ p [] [str "E"] ]
                    Column.column [] [ p [] [str "X"] ]
                    Column.column [] [ p [] [str "T"] ]
                ]
            ]
        ]
    ]

let galleryEntryCard piece description dispatch = 
    div [] [
        Container.container [ Container.Props [ ClassName "galleryImage"; ] ] [
            Image.image [] [ img [ Src ("./imgs/" + piece + ".png") ] ]
        ]
        Level.level [] [
            Tile.child [Tile.IsVertical] [
                Container.container [ Container.Props [ClassName "galleryTitleCard"] ] [
                    h1 [] [ str piece ]
                ]   
                Container.container [ Container.Props [ ClassName "galleryDescriptionCard" ] ] [
                    p [] [ str description ]
                ]
            ]
        ]
    ]

let galleryModal (model: SharedDesignGallery.Model) dispatch =
    Modal.modal [ Modal.IsActive true ] [ 
        Modal.background [ Props [ OnClick (fun _ -> BackToPortfolio |> dispatch) ]] []
        Modal.content [ Props [ Style [ Width "100%"; ]; ] ] [
            Level.level [] [
                bigPrevious dispatch
                Level.item [] [
                    let piece, description = getGalleryCardByIndex model.CurrentPieceIndex
                    galleryEntryCard piece description dispatch
                ]
                bigNext dispatch
            ]
        ]
        Modal.close [ Modal.Close.Props [ Style [Color "#000"; BackgroundColor "#FF2843"]] ; Modal.Close.Size IsLarge; Modal.Close.OnClick (fun _ -> BackToPortfolio |> dispatch) ] [] 
    ]

let view (model: SharedDesignGallery.Model) dispatch =
    Container.container [ Container.Props [ Style [ MaxWidth "100%"; BackgroundColor "rgba(0,0,0,.5)" ] ] ] [
        galleryModal model dispatch
    ]
