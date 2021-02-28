module App

open Elmish
open Elmish.React
open PageRouter

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
// |> Program.withSubscription TileTap.subscribe
|> Program.toNavigable PageRouter.urlParser PageRouter.urlUpdate
|> Program.run
