module ArtGallery

open FSharp
open Elmish
open System
open System.IO
open Fable.React
open Fable.React.Props
open Fulma

open Shared


(*

t shirts  = 20
3/4 || long sleeve 25
sweatshirt = ??
hats // backpack // mug // else...??

// mimic to some degree style of printful store (clone that essentially, with wrapper API to place orders)
let gridProductCard heroImage productSwatches productInfo =
    div [] [
        // will be next or back a picture (if there is more than one picture, if only one alternate picture then flip icon {like the one used for front / back facing camera swap})
        div [] []
        Image.image [] [ img [Src = ""] ]
        div [] [ str "" ]
        div [] [ str "" ]
        div [] [ str "" ]
    ]
// mimic amazon style list
let listProductCard heroImage productSwatches productInfo =
    div [] [
        // will be next or back a picture (if there is more than one picture, if only one alternate picture then flip icon {like the one used for front / back facing camera swap})
        div [] []
        Image.image [] [ img [ Src = "" ] ]
        div [] [ str "" ]
        div [] [ str "" ]
        div [] [ str "" ]
    ]

// mockup of products
    // finalize designs
        // download generated mock ups

// ProductImages -- The image(s) for the product record.
|| string list --> [ "path/to/img.ext"; "path/to/img.ext"; ... ]
    // HERO IMAGE || Index: [0] 
        || The first element in the product image list will be used as the 'HERO IMAGE' for the product.
        || If there are no elements, the default store logo will be used.
    // ALT VIEWS || Index: [1..n] 
        || Any additional images contained within the product image list will be used as alternate views. 
        || If there is only one additional image, the icon for chaning the view should be 'Flip' style.
        || All additional images will be shown in order of assigned index from it's defined record.

// Needs to have higher level of separation in order to have the Option title, along with the list of it's options.

// ProductOptions -- A distinct and required selection of an option to choose between variants.
|| string list --> [ "OptionA"; "OptionB"; ... ] --> || Indexes: [0..n]
    // Color
    // Size
    // Etc....
        // If there is only one option, it will be considered a default and non-selectable option.
        // Otherwise, the options will be listed for user's selection.

// ProductDetails -- Describe the intent, build or purpose of a product.
|| string list --> [ "DescriptivePointA"; "DescriptivePointB"; ... ]

*)


// GALLERY POSTCARD APP -> SHARE THIS SITE (POSTCARD FROM THE INTERWEBS)
// RELATED TAGS BROWSER:
    // OTHER IMAGES ALSO FLAGGED FOR SAME STYLE / THEME; SELF DRIVE-RECOMENDATIONS
// POSTCARD SENDER, SEND A TEMPLATE OF THE GALLERY ART WITH CUSTOM TEXT OR IMAGE AND EMAIL TO A FRIEND
    // SHARE THIS SITE / PRODUCT / IMAGE

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

// HEADER CONTROLS FOR 
    // CHANGE VIEW -> T-SHIRT / LONG SLEEVE / SWEATSHIRT / ETC.... - IF DESIGN EXISTS ON MULTIPLE IMAGES ()
    // LINK TO PRINTFUL STORE OF SELECTED PRODUCT  - IF DESIGN IS SALEABLE / SELECTED GARMENT VIEW

// HELPER FUNCTION TO BREAKDOWN THE GALLERY TUPLE
let getGalleryCardByIndex ( index: int ) =
    let piece, description = galleryPieces.Item ( index )
    piece, description

let galleryEntryCard piece description =
    div [] [
        Container.container [ Container.Props [ ClassName "galleryImage" ] ] [
            Image.image [] [ img [ Src ( "./imgs/" + piece + ".png" ) ] ]
        ]
        Level.level [] [
            Tile.child [ Tile.IsVertical ] [
                Container.container [ Container.Props [ ClassName "galleryTitleCard" ] ] [ h1 [] [ str piece ] ]
                Container.container [ Container.Props [ ClassName "galleryDescriptionCard" ] ] [ p [] [ str description ] ]
            ]
        ]
    ]

let galleryModal ( model: SharedDesignGallery.Model ) dispatch =
    Modal.modal [ Modal.IsActive true ] [ 
        Modal.background [] []
        Modal.content [ Props [ClassName "modalContent"] ] [
            Level.level [] [
                SharedViewModule.bigNavButton (SetCurrentPieceIndex (-1)) "PREV" dispatch
                Level.item [] [
                    let piece, description = getGalleryCardByIndex model.CurrentPieceIndex
                    galleryEntryCard piece description
                ]
                SharedViewModule.bigNavButton (SetCurrentPieceIndex (1)) "NEXT" dispatch
            ]
            a [ ClassName "likeOnInsta" ] [ a [ Href "https://www.instagram.com/xeroeffort/" ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/IG.png" ] ]; p [] [ str "Instagram" ] ] ]
            a [ ClassName "closeModal"; OnClick ( fun _ -> BackToPortfolio |> dispatch ) ] [ Image.image [ Image.Is64x64 ] [ img [ Src "./imgs/icons/X-it.png" ] ] ]
        ]
    ]

// wrap in div to match other submodules, will have side effects as containers aren't styleless.
let view ( model: SharedDesignGallery.Model ) dispatch =
    Container.container [] [ galleryModal model dispatch ]