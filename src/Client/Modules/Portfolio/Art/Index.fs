module ArtGallery

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Shared

// name, description tuple for gallery pieces.
// unable to read out folder file contents, System.IO not compatible with Fable
let galleryPieces = [
    "Bowing Bubbles", "What's poppin?";
    "Out for Blood", "The hunt is on.";
    "Harlot", "Unholy mother of sin...";
    "Kcuf Em", "Kcuf me? F!#% you.";
    "Misfortune", "It was never in the cards to begin with.";
    "BackStabber", "Never saw them comin'.";
    "Caution Very Hot", "Ya' might get burnt...";
]

type Msg =
    | BackToPortfolio
    | SetCurrentPieceIndex of int

let init () =
    SharedDesignGallery.getInitialModel, Cmd.none

let update msg ( model : SharedDesignGallery.Model ) =
    match msg with
    | SetCurrentPieceIndex msg ->
        let desiredPieceIndex = 
            if model.CurrentPieceIndex + msg < 0 
                then 0
            elif model.CurrentPieceIndex + msg > galleryPieces.Length - 1
                then model.CurrentPieceIndex
            else
                model.CurrentPieceIndex + msg
        { model with CurrentPieceIndex = desiredPieceIndex }, Cmd.none
    | _ ->
        model, Cmd.none

// HELPER FUNCTION TO BREAKDOWN THE GALLERY TUPLE
let getGalleryCardByIndex ( index: int ) =
    let piece, description = galleryPieces.Item ( index )
    piece, description

let galleryEntryHeaderControls dispatch =
    div [] [
        a [ ClassName "closeModal"; OnClick ( fun _ -> BackToPortfolio |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
        span [ ClassName "modalExternalLink" ] [ a [ Href "https://www.instagram.com/xeroeffort/" ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/IG.png" ] ]; ] ]
    ]

let galleryEntryContent piece description =
    div [] [
        div [ ClassName "galleryTitleCard" ] [ h1 [] [ str piece ] ]
        div [ ClassName "galleryImage" ] [ Image.image [] [ img [ Src ( "./imgs/" + piece + ".jpeg" ) ] ] ]
        p [ ClassName "galleryDescriptionCard" ] [ str description ]
    ]

let galleryEntryDesktopFooterControls dispatch =
    div [] [
        div [ ClassName "leftButtonDesktop" ] [ SharedViewModule.bigNavButton ( SetCurrentPieceIndex (-1) ) "PREV" dispatch ]
        div [ ClassName "rightButtonDesktop" ] [ SharedViewModule.bigNavButton ( SetCurrentPieceIndex (1) ) "NEXT" dispatch ]
    ]

let galleryEntryMobileFooterControls dispatch =
    div [] [
        div [ ClassName "leftButton" ] [ a [ OnClick ( fun _ -> (SetCurrentPieceIndex (-1) |> dispatch ) ) ] [ Image.image [ Image.Is64x64 ] [ img [Src "./imgs/icons/LeftNavButton.png"] ] ] ]
        div [ ClassName "rightButton" ] [ a [ OnClick ( fun _ -> (SetCurrentPieceIndex (1) |> dispatch ) ) ] [ Image.image [ Image.Is64x64 ] [ img [Src "./imgs/icons/RightNavButton.png"] ] ] ]
    ]

let view ( model: SharedDesignGallery.Model ) dispatch =
    let piece, description = getGalleryCardByIndex model.CurrentPieceIndex
    SharedViewModule.sharedViewModal
        true
        ( galleryEntryHeaderControls dispatch )
        ( galleryEntryContent piece description ) 
        ( if SharedViewModule.viewPortModalBreak
            then galleryEntryDesktopFooterControls dispatch
            else galleryEntryMobileFooterControls dispatch )
