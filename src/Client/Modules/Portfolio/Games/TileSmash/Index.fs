module TileSmash

open System
open FSharp

open Shared
open GridGame

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

type Msg =
    | QuitGame

let init(): SharedTileSmash.Model * Cmd<Msg> =
    SharedTileSmash.initModel, Cmd.none


let update msg model =
    match msg with
    | QuitGame ->
        model, Cmd.ofMsg QuitGame


let goalRollRowCreator (rowPositions: LaneObject list) dispatch =
    Level.level [] [
        Tile.parent [] [
            for positionObject in rowPositions do
                match positionObject with
                | Blocker -> // TO BECOME BOMB -2 Health 
                    Tile.child [] [
                        Box.box' [ Common.Props [ Style [Border "1px solid white"; Background "#000000"; Width 100; Height 100] ] ] [] // STYLE THIS??
                    ]
                | Goal ->
                    // 3HP
                    // EZ: (3 missed intervals = -1 HP) MD: (2 missed intervals = -1 HP) HD: () XD: (1 miss = -3HP)
                    Tile.child [] [
                        Box.box' [Common.Props [ Style [Border "1px solid black"; Background "#FF2843"; Width 100; Height 100]]] [ Image.image [] [ img [Src "./imgs/icons/Flag.png"]] ] // STYLE THIS??
                    ]
                | _ -> 
                    Tile.child [] [
                        Box.box' [ Common.Props [ Style [Border "1px solid black"; Width 100; Height 100] ] ] [] // STYLE THIS??
                    ]
            ]
    ]

// TILE SORT CONTROLS HEADER
let tileSmashHeader =
    Container.container [ Container.Props [ ClassName "contentCard" ] ] [
            h1 [] [ str "Tile Smash"]
            p [] [ str "- Survival Mode:" ]
            p [] [ str "- Smash the tile before it's timer runs out." ]
            p [] [ str "- Tile timer reaches 0 takes away 1 HP." ]
            p [] [ str "- Smash a Heart to gain 1 HP." ]
            p [] [ str "- Smashing bombs takes away 2 HP."]
            // p [] [ str "- Time Attack: 30 or 60 seconds?: how many can you smash?" ]
        ]

// function to place a new flag or mark as missed if not smashed in the time alotted (have tile scroll color as interval reaches end)
let tileSmashBoardView gridPositions dispatch =
    let board = GridGame.getPositionsAsRows gridPositions 8
    Container.container [] [
        for row in board do
            goalRollRowCreator row dispatch
    ]

let view dispatch =
    Container.container [] [
        SharedModule.backToGallery QuitGame dispatch
        tileSmashHeader
        // match model.GameState with
        Container.container [Container.Props [ClassName "gameGridContainer"] ] [
            tileSmashBoardView (SharedTileSmash.generateEmptyTileSmashGrid SharedTileSmash.gridDimension) dispatch
        ]
    ]