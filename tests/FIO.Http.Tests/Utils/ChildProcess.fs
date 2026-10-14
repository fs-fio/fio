module FIO.Http.Tests.ChildProcess

open FIO.Http.Tests.Utilities

open FIO.App
open FIO.DSL
open FIO.Http
open FIO.Runtime.WorkStealing
open FIO.Http.SimpleRoutes

open System

let private print (line: string) =
    FIO.attempt
        (fun () ->
            Console.Out.WriteLine line
            Console.Out.Flush())
        id

let private stuckRoutes : Routes<exn> =
    get "/ok" (HttpHandler.text "ok")
    |> Routes.combine (get "/stuck" (fun _ -> (print "entered").FlatMap(fun () -> FIO.never ())))

let private stuckConfig (port: int) (shutdownTimeout: int) =
    ServerConfig.create "127.0.0.1" port |> ServerConfig.withShutdownTimeout shutdownTimeout

let private serve (port: int) (shutdownTimeout: int) =
    use runtime = new WorkStealingRuntime(testConfig)

    match runtime.Run(Server.runServer (stuckConfig port shutdownTimeout) stuckRoutes).UnsafeResult() with
    | Succeeded () -> 0
    | _ -> 1

type private ServerApp(port: int, shutdownTimeout: int) =
    inherit FIOApp<unit, exn>()

    override _.effect =
        Server.runServer (stuckConfig port shutdownTimeout) stuckRoutes

let run (args: string list) : int =
    match args with
    | [ "server"; port; timeout ] -> serve (int port) (int timeout)
    | [ "app-server"; port; timeout ] -> ServerApp(int port, int timeout).Run()
    | other ->
        eprintfn $"Unknown child scenario: {other}"
        2

type ChildProcess = FIO.Tests.Shared.ChildProcessRunner.ChildProcess
