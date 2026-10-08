module FIO.Http.Tests.ChildProcess

open FIO.Http.Tests.Utilities

open FIO.App
open FIO.DSL
open FIO.Http
open FIO.Runtime.WorkStealing
open FIO.Http.SimpleRoutes

open System
open System.Threading
open System.Diagnostics
open System.Collections.Concurrent

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

type private Marker = class end

// Re-runs this test assembly as a child process, for behaviour that needs a process of its own.
type ChildProcess(args: string list) =
    let lines = ConcurrentQueue<string>()

    let host =
        match Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
        | null
        | "" -> "dotnet"
        | path -> path

    let info =
        ProcessStartInfo(host, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)

    do
        info.ArgumentList.Add "exec"
        info.ArgumentList.Add typeof<Marker>.Assembly.Location
        info.ArgumentList.Add "--child"

        for arg in args do
            info.ArgumentList.Add arg

    let proc = Process.Start info

    do
        proc.OutputDataReceived.Add(fun args -> if not (isNull args.Data) then lines.Enqueue args.Data)
        proc.ErrorDataReceived.Add(fun args -> if not (isNull args.Data) then lines.Enqueue $"stderr: {args.Data}")
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()

    member _.Output : string list =
        List.ofSeq lines

    member _.WaitForLine (expected: string) (timeout: TimeSpan) : bool =
        let stopwatch = Stopwatch.StartNew()

        while not (lines |> Seq.contains expected) && not proc.HasExited && stopwatch.Elapsed < timeout do
            Thread.Sleep 20

        lines |> Seq.contains expected

    member _.Signal (name: string) : unit =
        use kill = Process.Start("kill", $"-{name} {proc.Id}")
        kill.WaitForExit()

    member _.WaitForExit (timeout: TimeSpan) : int option =
        if proc.WaitForExit(int timeout.TotalMilliseconds) then
            proc.WaitForExit()
            Some proc.ExitCode
        else
            None

    interface IDisposable with
        member _.Dispose () =
            if not proc.HasExited then
                try proc.Kill true with _ -> ()

            proc.Dispose()
