module App

open Elmish
open Elmish.React
open Shared

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// SHOULD MOVE THE INDEX MODULE NAME TO SOMETHING MORE APPROPRIATE
Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
// SHOULD BE USING THE PAGEROUTER
|> Program.toNavigable Index.urlParser Index.urlUpdate
|> Program.run
