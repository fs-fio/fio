module FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Default
open FIO.Runtime.WorkStealing

open System
open System.Net
open System.Text
open System.Threading
open System.Net.Sockets
open System.Threading.Tasks
open Microsoft.AspNetCore.Connections

open Expecto

let testConfig = { WorkerConfig.Default with EvaluationWorkers = 2 }

[<CLIMutable>]
type TestMessage = { Id: int; Text: string }

type FailingStream(prefix: byte array) =
    inherit IO.Stream()

    let mutable position = 0

    override _.CanRead = true

    override _.CanSeek = false

    override _.CanWrite = false

    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush () = ()

    override _.Read (buffer: byte array, offset: int, count: int) : int =
        if position >= prefix.Length then
            raise (IO.IOException "The response body's source failed.")

        let copied = min count (prefix.Length - position)
        Array.blit prefix position buffer offset copied
        position <- position + copied
        copied

    override _.Seek (_: int64, _: IO.SeekOrigin) : int64 = raise (NotSupportedException())

    override _.SetLength (_: int64) = raise (NotSupportedException())

    override _.Write (_: byte array, _: int, _: int) = raise (NotSupportedException())

let runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new WorkStealingRuntime(testConfig) :> FIORuntime
    ]

let private disposeRuntime (rt: FIORuntime) =
    match box rt with
    | :? IDisposable as d -> d.Dispose()
    | _ -> ()

let testAllRuntimes name (f: FIORuntime -> unit) =
    testSequenced (
        testList
            name
            [
                for rt in runtimes () ->
                    testCase (rt.GetType().Name) <| fun () ->
                        try
                            f rt
                        finally
                            disposeRuntime rt
            ]
    )

// Async.RunSynchronously's timeout waits for the computation to stop, which a hung fiber never does, so it never fires.
let private runWithin (seconds: float) (runtime: FIORuntime) (effect: FIO<'A, exn>) =
    let fiber = runtime.Run effect
    let result = fiber.Task()

    if not (result.Wait(TimeSpan.FromSeconds seconds)) then
        runtime.Run(fiber.InterruptNow()) |> ignore
        failtest $"The effect did not finish within {seconds} s on {runtime.Name}"

    match result.Result with
    | Succeeded value -> value
    | Failed error -> failtest $"Effect failed: {error}"
    | Interrupted ex -> failtest $"Interrupted: {ex.Message}"

let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, exn>) =
    runWithin 30.0 runtime effect

let makeRequest (method: HttpMethod) (path: string) =
    HttpRequest.create method path

let makeGetRequest (path: string) =
    makeRequest HttpMethod.GET path

let dispatchAndRun (runtime: FIORuntime) (routes: Routes<exn>) (request: HttpRequest) =
    runWithin 10.0 runtime (Routes.dispatch request routes)

let getWhenListening (url: string) =
    use client = new Http.HttpClient()
    let deadline = DateTime.UtcNow.AddSeconds 10.0
    let mutable result = None

    while result.IsNone && DateTime.UtcNow < deadline do
        try
            result <- Some(client.GetStringAsync(url).Result)
        with _ ->
            Thread.Sleep 25

    match result with
    | Some body -> body
    | None -> failtest $"Server at {url} never became ready"

// Kestrel aborts a half-closed connection, so a raw request asks for `Connection: close` and keeps its socket open.
let sendRawRequest (port: int) (request: string) =
    use tcp = new TcpClient()
    tcp.Connect("127.0.0.1", port)
    use stream = tcp.GetStream()

    let bytes = Encoding.ASCII.GetBytes request
    stream.Write(bytes, 0, bytes.Length)
    stream.Flush()

    stream.ReadTimeout <- 5000
    use reader = new IO.StreamReader(stream, Encoding.ASCII)
    try reader.ReadToEnd() with _ -> ""

let findAvailablePort () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    let port = (listener.LocalEndpoint :?> IPEndPoint).Port
    listener.Stop()
    port

// Windows retries a refused loopback connect for two seconds, so a connect that is not quick counts as not listening.
let isListening (port: int) =
    use tcp = new TcpClient()

    try
        tcp.ConnectAsync("127.0.0.1", port).Wait(TimeSpan.FromMilliseconds 500.0) && tcp.Connected
    with _ ->
        false

let tryGet (port: int) (path: string) : string option =
    use client = new Http.HttpClient(Timeout = TimeSpan.FromSeconds 1.0)

    try
        Some(client.GetStringAsync($"http://127.0.0.1:{port}{path}").Result)
    with _ ->
        None

// Kestrel reports a port that something else holds as an IOException around AddressInUseException.
let rec isAddressInUse (error: exn) =
    match error with
    | null -> false
    | :? AddressInUseException -> true
    | :? SocketException as ex when ex.SocketErrorCode = SocketError.AddressAlreadyInUse -> true
    | _ -> isAddressInUse error.InnerException

// A port from findAvailablePort can be taken before the server binds it, so a start that lost its port retries on a fresh one.
let startOnFreePort (runtime: FIORuntime) (start: int -> FIO<'S, exn>) : int * 'S =
    let rec attempt remaining =
        let port = findAvailablePort ()
        let started = runtime.Run(start port).Task()

        if not (started.Wait(TimeSpan.FromSeconds 30.0)) then
            failtest $"The server on port {port} did not start within 30 s"

        match started.Result with
        | Succeeded server -> port, server
        | Failed error when remaining > 0 && isAddressInUse error ->
            Thread.Sleep 50
            attempt (remaining - 1)
        | Failed error -> raise error
        | Interrupted ex -> raise ex

    attempt 10

// The driver ends only once the server it forked has unwound; `ended` says whether the server ended because it lost its port.
let forkServerOnFreePort (runtime: FIORuntime) (interrupt: Task) (probe: string) (serve: int -> FIO<unit, exn>) : int * Fiber<unit, exn> =
    let rec attempt remaining =
        let port = findAvailablePort ()
        let ended = TaskCompletionSource<bool> TaskCreationOptions.RunContinuationsAsynchronously
        let driver =
            fio {
                let! server =
                    (serve port)
                        .TapError(fun error -> FIO.succeedWith (fun () -> ended.TrySetResult(isAddressInUse error) |> ignore))
                        .Ensuring(FIO.succeedWith (fun () -> ended.TrySetResult false |> ignore))
                        .Fork()
                do! FIO.awaitUnitTask (Task.WhenAny(interrupt, ended.Task)) id
                do! server.InterruptNow()
            }
        let fiber = runtime.Run driver
        let deadline = DateTime.UtcNow.AddSeconds 30.0

        while not ended.Task.IsCompleted && (tryGet port probe).IsNone && DateTime.UtcNow < deadline do
            Thread.Sleep 25

        if ended.Task.IsCompleted && ended.Task.Result && remaining > 0 then
            fiber.Task().Wait(TimeSpan.FromSeconds 30.0) |> ignore
            attempt (remaining - 1)
        else
            port, fiber

    attempt 10

// A round that finds its port held by another program returns None and is repeated on a fresh port.
let onFreePort (round: int -> 'T option) : 'T =
    let rec attempt remaining =
        match round (findAvailablePort ()) with
        | Some value -> value
        | None when remaining > 0 -> attempt (remaining - 1)
        | None -> failtest "Every port tried was held by something other than the server under test"

    attempt 10

let private startConfiguredServer (configure: ServerConfig -> ServerConfig) (routes: Routes<exn>) (runtime: DefaultRuntime) =
    startOnFreePort runtime (fun port -> Server.startServerWithRuntime (configure (ServerConfig.create "127.0.0.1" port)) routes runtime)

// Runs the action against a server started through Server, so what a test sees is what a user's server does.
let withConfiguredServer (configure: ServerConfig -> ServerConfig) (routes: Routes<exn>) (action: int -> FIOServer -> 'T) : 'T =
    let runtime = new DefaultRuntime()
    let port, server = startConfiguredServer configure routes runtime

    try
        action port server
    finally
        runtime.Run(Server.stop server).UnsafeResult() |> ignore
        (runtime :> IDisposable).Dispose()

let withTestHttpServer (routes: Routes<exn>) (action: int -> 'T) : 'T =
    withConfiguredServer id routes (fun port _ -> action port)

let withCapturedError (body: unit -> 'T) : 'T * string =
    let original = Console.Error
    use captured = new IO.StringWriter()
    Console.SetError captured

    try
        let result = body ()
        result, captured.ToString()
    finally
        Console.SetError original
