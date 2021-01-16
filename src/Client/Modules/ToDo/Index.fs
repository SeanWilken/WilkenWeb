module SAFETodo

open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    { Todos: Todo list
      Input: string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    // | RemoveTodo of Todo

// At this point, the app should work just as it did before.
// Now, expanding the API and adding a new endpoint is as easy as adding a new field to the API protocol we defined in Shared.fs,
// editing the myApi record in Server.fs with the implementation,
// and finally making calls from the proxy.

// API / SERVER CALLS
// Client HTTP Handler
let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder // Shared Route builder
    |> Remoting.buildProxy<ITodosApi> // Shared interface for Todos

let init(): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = "" }
    // API / SERVER CALLS
    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos // Create Async call to getTodos, return GotTodos (list)
    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | GotTodos todos ->
        { model with Todos = todos }, Cmd.none
    | SetInput value ->
        { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    // | RemoveTodo todo -> // this should really be an Async call
    //     let cmd = Cmd.OfAsync.perform todosApi.removeTodo todo
    //     model, cmd//Cmd.none // not actually calling server

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Content.content [ ] [
            Content.Ol.ol [ ] [
                for todo in model.Todos do
                    li [ ] [
                        str todo.Description
                        // Button.button [ Button.OnClick (fun _ -> RemoveTodo todo |> dispatch) ] [ str "X"]
                    ]
            ]
        ]
        Field.div [ Field.IsGrouped ] [
            Control.p [ Control.IsExpanded ] [
                Input.text [
                  Input.Value model.Input
                  Input.Placeholder "What needs to be done?"
                  Input.OnChange (fun x -> SetInput x.Value |> dispatch) ]
            ]
            Control.p [ ] [
                Button.a [
                    Button.Color IsPrimary
                    Button.Disabled (Todo.isValid model.Input |> not)
                    Button.OnClick (fun _ -> dispatch AddTodo)
                ] [
                    str "Add"
                ]
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    containerBox model dispatch
                ]
            ]
        ]
    ]
