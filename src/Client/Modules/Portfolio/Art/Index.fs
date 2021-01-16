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

// SECTION HEADER CONTROLS
// HOVER MAKES IT RED? IDK
let previousButton dispatch =
        a [ OnClick(fun _ -> SetCurrentPieceIndex (-1) |> dispatch ); ] [
            Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/LeftNavButton.png"] ] 
        ]
let nextButton dispatch =
        a [ OnClick(fun _ -> SetCurrentPieceIndex (1) |> dispatch ); ] [
            Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/RightNavButton.png"] ] 
        ]

let galleryEntryCard piece description dispatch = 
        Tile.ancestor [] [
            Tile.parent [] [
                Tile.child [] [
                    Container.container [ Container.Props [ ClassName "galleryImage"; ] ] [
                        Image.image [] [ img [ Src ("./imgs/" + piece + ".png") ] ]
                    ]
                ]
            ]
            Tile.parent [Tile.IsVertical; Tile.Size Tile.Is5] [
                Tile.parent [] [
                    Container.container [ Container.Props [ Style [TextAlign TextAlignOptions.Center] ] ] [
                        Level.level [] [
                            Level.item [] [ previousButton dispatch ]
                            Level.item [] [ SharedModule.backToGallery BackToPortfolio dispatch ]
                            Level.item [] [ nextButton dispatch ]
                        ]
                    ]
                ]
                Tile.child [] [
                    Container.container [ Container.Props [ClassName "galleryTitleCard"] ] [
                        Container.container [] [ h1 [] [ str piece ] ]
                    ]
                ]
                Tile.child [] [
                    Container.container [ Container.Props [ ClassName "galleryDescriptionCard" ] ] [
                        p [] [ str description ]
                    ]
                ]
            ]
        ]

let view (model: SharedDesignGallery.Model) dispatch =
    Container.container [] [
            let piece, description = getGalleryCardByIndex model.CurrentPieceIndex
            galleryEntryCard piece description dispatch
        ]
