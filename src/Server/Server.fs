module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Shared
open Giraffe.Core
open Giraffe.ResponseWriters

// REMOVE TODOS APP
// ----------------
type Storage () =
    let todos = ResizeArray<_>()

    member __.GetTodos () =
        List.ofSeq todos

    member __.AddTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok ()
        else Error "Invalid todo"

    // NEEDS WORK
    member __.RemoveTodo (todo: Todo) =
        List.ofSeq todos
        |> List.filter (fun x -> x.Id <> todo.Id)
        |> List.ofSeq

// storage instance
let storage = Storage()
storage.AddTodo(Todo.create "Create new SAFE project") |> ignore
storage.AddTodo(Todo.create "Write your app") |> ignore
storage.AddTodo(Todo.create "Ship it !!!") |> ignore

// API / SERVER CALLS
// Server implementation of the shared interface
let todosApi =
    { 
        getTodos = fun () -> async { return storage.GetTodos() }
        addTodo =
            fun todo -> async {
                match storage.AddTodo todo with
                | Ok () -> return todo
                | Error e -> return failwith e
            } 
        removeTodo = fun (todo) -> async { return storage.RemoveTodo todo }
    }
// ----------------

// HANDLE HYDRATION OF MODEL IF URL IS USED AS PRIMARY INPUT???

// let navigatePageFromServer

let getPage pageString = async { // string path variable
    // this should be match case to return the appropriate model
    match pageString with
    | "/about" -> //http://localhost:8085/api/IPageApi/GetPage // "/about"
        return SharedWebAppModels.AboutSection
    | "/contact" ->
        return SharedWebAppModels.Contact
    | "/portfolio" ->
        return (SharedWebAppModels.Portfolio (SharedPortfolioGallery.PortfolioGallery))
    | _
    | "/welcome" ->
        return SharedWebAppModels.Welcome
}

let pageApi =
    {
        GetPage = getPage
    }

let browser = pipeline {
    plug acceptHtml
    plug putSecureBrowserHeaders
    plug fetchSession
    set_header "x-pipeline-type" "Browser"
}

let defaultView = router {
    // get "/" 
    // forward "/" getPage "/welcome"
    // get "/welcome" (getPage "/welcome")//(redirectTo false "/")
    get "/index.html" (redirectTo false "/")
    get "/default.html" (redirectTo false "/")
}





// NEEDS WORK !!
// API / SERVER CALLS
// HTTP Handler to work with ASP .NET pipeline
let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder // use the shared Route.builder
    // |> Remoting.fromValue todosApi // use the todosApi
    |> Remoting.fromValue pageApi // use the pageApi
    |> Remoting.buildHttpHandler // construct the webapp http handler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
