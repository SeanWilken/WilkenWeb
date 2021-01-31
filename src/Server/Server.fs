module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Shared
open Giraffe.Core
open Giraffe.ResponseWriters

// HANDLE HYDRATION OF MODEL IF URL IS USED AS PRIMARY INPUT???

// let navigatePageFromServer

// let getPage pageString = async { // string path variable
//     // this should be match case to return the appropriate model
//     match pageString with
//     | "/about" -> //http://localhost:8085/api/IPageApi/GetPage // "/about"
//         // TEMPORARY
//         return SharedWebAppModels.AboutSection SharedAboutSection.getInitialModel
//     | "/contact" ->
//         return SharedWebAppModels.Contact
//     | "/portfolio" ->
//         return (SharedWebAppModels.Portfolio (SharedPortfolioGallery.PortfolioGallery))
//     | _
//     | "/welcome" ->
//         return SharedWebAppModels.Welcome
// }

// let pageApi =
//     {
//         GetPage = getPage
//     }

let browser = pipeline {
    plug acceptHtml
    plug putSecureBrowserHeaders
    plug fetchSession
    set_header "x-pipeline-type" "Browser"
}

let defaultView = router {
    pipe_through browser
    // get "/welcome" (getPage "/welcome")//(redirectTo false "/")
    get "/index.html" (redirectTo false "/")
    get "/default.html" (redirectTo false "/")
}

let browserRouter = router {
    forward "" defaultView
}



// NEEDS WORK !!
// API / SERVER CALLS
// HTTP Handler to work with ASP .NET pipeline
let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder // use the shared Route.builder
    // |> Remoting.fromValue pageApi // use the pageApi
    |> Remoting.buildHttpHandler // construct the webapp http handler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        // use_router defaultView
        // use_router browserRouter
        memory_cache
        use_static "public"
        use_gzip
    }

run app
