/// <summary>
/// FIOApp framework examples demonstrating various app patterns and features.
/// </summary>
module private FIO.Examples.App

open FIO.DSL
open FIO.App
open FIO.Random
open FIO.Console
open FIO.Environment
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Globalization

/// <summary>
/// Simple welcome app demonstrating console I/O with FIOApp.
/// </summary>
type WelcomeApp() =
    inherit FIOApp<unit, exn>()

    override _.effect: FIO<unit, exn> =
        fio {
            do! Console.printLine ("Hello! What is your name?", id)
            let! name = Console.readLine id
            do! Console.printLine ($"Hello, %s{name}! Welcome to FIO! 🪻💜", id)
        }

/// <summary>
/// Demonstrates parsing user input with error handling using FIOApp.
/// </summary>
type EnterNumberApp() =
    inherit FIOApp<string, exn>()

    override _.effect =
        fio {
            do! Console.print ("Enter a number: ", id)
            let! input = Console.readLine id

            match! FIO.attempt ((fun () -> Int32.TryParse input), id) with
            | true, number -> return $"You entered the number: %i{number}."
            | false, _ -> return! FIO.fail (IOException "You entered an invalid number!")
        }

/// <summary>
/// Demonstrates try-with error handling in computation expressions.
/// </summary>
type TryWithApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                do! FIO.fail 1
                return "Successfully completed!"
            with errorCode ->
                return! FIO.fail errorCode
        }

/// <summary>
/// Demonstrates try-finally for cleanup operations.
/// </summary>
type TryFinallyApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                do! FIO.fail 1
                return "Successfully completed!"
            finally
                Console.printLine ("Running finalizer, always executes", (fun _ -> -2))
        }

/// <summary>
/// Combines try-with and try-finally for complete error handling.
/// </summary>
type TryWithFinallyApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                try
                    do! FIO.fail 1
                    return "Successfully completed!"
                with errorCode ->
                    return! FIO.fail errorCode
            finally
                Console.printLine ("Running finalizer, always executes", (fun _ -> -2))
        }

/// <summary>
/// Demonstrates for loops in computation expressions.
/// </summary>
type ForApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            for number in 1..10 do
                match number % 2 = 0 with
                | true -> do! Console.printLine ($"%i{number} is even!", id)
                | false -> do! Console.printLine ($"%i{number} is odd!", id)
        }

/// <summary>
/// Interactive number guessing game using while loops and random numbers.
/// </summary>
type GuessNumberApp() =
    inherit FIOApp<int, exn>()

    override _.effect =
        fio {
            let! numberToGuess = Random.nextIntRange (1, 101)
            let mutable guess = -1

            while guess <> numberToGuess do
                do! Console.print ("Guess a number: ", id)
                let! input = Console.readLine id

                match Int32.TryParse input with
                | true, parsedInput ->
                    guess <- parsedInput

                    if guess < numberToGuess then
                        do! Console.printLine ("Too low! Try again.", id)
                    elif guess > numberToGuess then
                        do! Console.printLine ("Too high! Try again.", id)
                    else
                        do! Console.printLine ("Congratulations! You guessed the number!", id)
                | _ -> do! Console.printLine ("Invalid input. Please enter a number.", id)

            return guess
        }

/// <summary>
/// Ping-pong message passing using operators and channels.
/// </summary>
type PingPongApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>
    /// Pinger sends a message and waits for a reply using operators.
    /// </summary>
    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Send "ping"
        >>= fun ping ->
            Console.printLine ($"pinger sent: %s{ping}", id)
            >>= fun _ ->
                chan2.Receive()
                >>= fun pong -> Console.printLine ($"pinger received: %s{pong}", id) >>= fun _ -> FIO.unit ()

    /// <summary>
    /// Ponger receives a message and sends a reply using operators.
    /// </summary>
    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Receive()
        >>= fun ping ->
            Console.printLine ($"ponger received: %s{ping}", id)
            >>= fun _ ->
                chan2.Send "pong"
                >>= fun pong -> Console.printLine ($"ponger sent: %s{pong}", id) >>= fun _ -> FIO.unit ()

    override _.effect =
        let chan1 = Channel<string>()
        let chan2 = Channel<string>()
        pinger chan1 chan2 <&&> ponger chan1 chan2

/// <summary>
/// Ping-pong using computation expression syntax.
/// </summary>
type PingPongCEApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>
    /// Pinger sends a message and waits for a reply using computation expression.
    /// </summary>
    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Send "ping"
            do! Console.printLine ($"pinger sent: %s{ping}", id)
            let! pong = chan2.Receive()
            do! Console.printLine ($"pinger received: %s{pong}", id)
        }

    /// <summary>
    /// Ponger receives a message and sends a reply using computation expression.
    /// </summary>
    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Receive()
            do! Console.printLine ($"ponger received: %s{ping}", id)
            let! pong = chan2.Send "pong"
            do! Console.printLine ($"ponger sent: %s{pong}", id)
        }

    override _.effect =
        fio {
            let chan1 = Channel<string>()
            let chan2 = Channel<string>()
            do! pinger chan1 chan2 <&&> ponger chan1 chan2
        }

/// <summary>
/// Message type for typed ping-pong communication.
/// </summary>
type Message =
    /// <summary>Ping message sent by the pinger.</summary>
    | PingMsg
    /// <summary>Pong message sent by the ponger.</summary>
    | PongMsg

/// <summary>
/// Ping-pong with pattern matching on message types.
/// </summary>
type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    /// <summary>
    /// Pinger sends a PingMsg and validates the reply with pattern matching.
    /// </summary>
    let pinger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            let! ping = chan1.Send PingMsg
            do! Console.printLine ($"pinger sent: %A{ping}", _.Message)

            match! chan2.Receive() with
            | PongMsg -> do! Console.printLine ($"pinger received: %A{PongMsg}", _.Message)
            | PingMsg -> return! FIO.fail $"pinger received %A{PingMsg} when %A{PongMsg} was expected!"
        }

    /// <summary>
    /// Ponger receives a PingMsg and replies with a random message type.
    /// </summary>
    let ponger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            match! chan1.Receive() with
            | PingMsg -> do! Console.printLine ($"ponger received: %A{PingMsg}", _.Message)
            | PongMsg -> return! FIO.fail $"ponger received %A{PongMsg} when %A{PingMsg} was expected!"

            let! sentMsg =
                fio {
                    match! Random.nextIntRange (0, 2) with
                    | 0 -> do! chan2.Send PongMsg
                    | _ -> do! chan2.Send PingMsg
                }

            do! Console.printLine ($"ponger sent: %A{sentMsg}", _.Message)
        }

    override _.effect =
        fio {
            let chan1 = Channel<Message>()
            let chan2 = Channel<Message>()
            do! pinger chan1 chan2 <&&> ponger chan1 chan2
        }

/// <summary>
/// Custom error types for demonstrating typed error handling.
/// </summary>
type Error =
    /// <summary>Database operation error.</summary>
    | DbError of bool
    /// <summary>Web service operation error.</summary>
    | WsError of int
    /// <summary>General error with message.</summary>
    | GeneralError of string

/// <summary>
/// Demonstrates typed error handling with CatchAll and error type unification.
/// </summary>
type ErrorHandlingApp() =
    inherit FIOApp<string * char, Error>()

    /// <summary>
    /// Simulates a database read that randomly succeeds or fails.
    /// </summary>
    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt ((fun () -> Random().Next(0, 2)), fun _ -> true)
            if rand = 0 then return "data" else return! FIO.fail false
        }

    /// <summary>
    /// Simulates a web service call that randomly succeeds or fails.
    /// </summary>
    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt ((fun () -> Random().Next(0, 2)), fun _ -> -1)
            if rand = 1 then return 'S' else return! FIO.fail 404
        }

    /// <summary>
    /// Database result with error type unified to Error.
    /// </summary>
    let databaseResult: FIO<string, Error> =
        fio { return! readFromDatabase.CatchAll(fun error -> FIO.fail (DbError error)) }

    /// <summary>
    /// Web service result with error type unified to Error.
    /// </summary>
    let webserviceResult: FIO<char, Error> =
        fio { return! awaitWebservice.CatchAll(fun error -> FIO.fail (WsError error)) }

    override _.effect =
        fio { return! (databaseResult <*> webserviceResult).CatchAll(fun _ -> FIO.succeed ("default", 'D')) }

/// <summary>
/// Demonstrates retry logic with error handling callbacks.
/// </summary>
type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, Error>()

    /// <summary>
    /// Simulates a database read that randomly succeeds or fails.
    /// </summary>
    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt ((fun () -> Random().Next(0, 2)), fun _ -> true)
            if rand = 0 then return "data" else return! FIO.fail false
        }

    /// <summary>
    /// Simulates a web service call that randomly succeeds or fails.
    /// </summary>
    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt ((fun () -> Random().Next(0, 2)), fun _ -> -1)
            if rand = 1 then return 'S' else return! FIO.fail 404
        }

    /// <summary>
    /// Database result with retry logic and error type unification.
    /// </summary>
    let databaseResult: FIO<string, Error> =
        fio {
            let onEachRetry (err, retry, maxRetries) =
                Console
                    .printLine(
                        $"Database read failed with error: %A{err}. Retry attempt %d{retry} of %d{maxRetries}...",
                        id
                    )
                    .CatchAll(fun _ -> FIO.fail false)

            return! readFromDatabase.Retry(4, onEachRetry).CatchAll(fun error -> FIO.fail (DbError error))
        }

    /// <summary>
    /// Web service result with retry logic and error type unification.
    /// </summary>
    let webserviceResult: FIO<char, Error> =
        fio {
            let onEachRetry (err, retry, maxRetries) =
                Console
                    .printLine(
                        $"Webservice read failed with error: %A{err}. Retry attempt %d{retry} of %d{maxRetries}...",
                        id
                    )
                    .CatchAll(fun _ -> FIO.fail 400)

            return! awaitWebservice.Retry(4, onEachRetry).CatchAll(fun error -> FIO.fail (WsError error))
        }

    override _.effect =
        fio { return! (databaseResult <*> webserviceResult).CatchAll(fun _ -> FIO.succeed ("default", 'D')) }

/// <summary>
/// Demonstrates async/task interop with FIO.awaitAsync.
/// </summary>
type AsyncErrorHandlingApp() =
    inherit FIOApp<string * int, Error>()

    /// <summary>
    /// Async task simulating a database read.
    /// </summary>
    let databaseReadTask: Async<string> =
        async {
            do printfn $"Reading from database..."

            if Random().Next(0, 2) = 0 then
                return "data"
            else
                raise (Exception "Database error!")
                return "error data"
        }

    /// <summary>
    /// Async task simulating a web service call.
    /// </summary>
    let webserviceAwaitTask: Async<int> =
        async {
            do printfn $"Awaiting webservice..."

            if Random().Next(0, 2) = 0 then
                return 200
            else
                raise (Exception "Webservice error!")
                return 400
        }

    /// <summary>
    /// Database result with error type unified to Error via CatchAll.
    /// </summary>
    let databaseResult: FIO<string, Error> =
        FIO.awaitAsync(databaseReadTask, id).CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    /// <summary>
    /// Web service result with error type unified to Error via CatchAll.
    /// </summary>
    let webserviceResult: FIO<int, Error> =
        FIO.awaitAsync(webserviceAwaitTask, id).CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    override _.effect = fio { return! databaseResult <&> webserviceResult }

/// <summary>
/// Stress test with 1 million concurrent fibers using channels.
/// </summary>
type HighlyConcurrentApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>
    /// Sender fiber that sends a random message to the channel.
    /// </summary>
    let sender (chan: Channel<int>) id =
        fio {
            let! msg = Random.nextIntRange (100, 501)
            do! chan.Send(msg).Unit()
            do! Console.printLine ($"Sender[%i{id}] sent: %i{msg}", Operators.id)
        }

    /// <summary>
    /// Receiver fiber that consumes messages from the channel.
    /// </summary>
    let rec receiver (chan: Channel<int>) count (max: int) =
        fio {
            if count = 0 then
                let! maxFibers = FIO.succeed (max.ToString("N0", CultureInfo "en-US"))
                do! Console.printLine ($"Successfully received a message from all %s{maxFibers} fibers!", id)
            else
                let! msg = chan.Receive()
                do! Console.printLine ($"Receiver received: %i{msg}", id)
                return! receiver chan (count - 1) max
        }

    /// <summary>
    /// Recursively creates sender fibers running in parallel.
    /// </summary>
    let rec create chan count acc =
        fio {
            if count = 0 then
                return! acc
            else
                let newAcc = sender chan count <&&> acc
                return! create chan (count - 1) newAcc
        }

    override _.effect =
        fio {
            let fiberCount = 1_000_000
            let chan = Channel<int>()
            let acc = sender chan fiberCount <&&> receiver chan fiberCount fiberCount
            return! create chan (fiberCount - 1) acc
        }

/// <summary>
/// Demonstrates FIO.fromGenericTask for wrapping .NET Tasks as fibers.
/// </summary>
type FiberFromTaskApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>
    /// Computes a Fibonacci number inside a .NET Task forked as a fiber.
    /// </summary>
    let fibonacci n =
        FIO.fromGenericTask (
            (fun () ->
                task {
                    let fib (n: int64) =
                        let mutable a = 0L
                        let mutable b = 1L
                        let mutable i = 0L

                        while i < n do
                            let temp = a + b
                            a <- b
                            b <- temp
                            i <- i + 1L

                        a

                    printfn $"Task computing Fibonacci of %i{n}..."
                    let res = fib n
                    printfn $"Fibonacci of %i{n} is %i{res}"
                    return ()
                }),
            id
        )

    override _.effect: FIO<unit, exn> =
        let await (fiber: Fiber<unit, exn>) =
            fio {
                do! fiber.Join()
                return ()
            }

        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! await fiber35 <&&> await fiber40 <&&> await fiber45
        }

/// <summary>
/// Demonstrates FIO.fromGenericTask with result values from Tasks.
/// </summary>
type FiberFromGenericTaskApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>
    /// Computes a Fibonacci number inside a .NET Task returning a result string.
    /// </summary>
    let fibonacci n =
        FIO.fromGenericTask (
            (fun () ->
                task {
                    let fib (n: int64) =
                        let mutable a = 0L
                        let mutable b = 1L
                        let mutable i = 0L

                        while i < n do
                            let temp = a + b
                            a <- b
                            b <- temp
                            i <- i + 1L

                        a

                    printfn $"Task computing Fibonacci of %i{n}..."
                    let res = fib n
                    return $"Fibonacci of %i{n} is %i{res}"
                }),
            id
        )

    override _.effect: FIO<unit, exn> =
        let awaitAndPrint (fiber: Fiber<string, exn>) =
            fio {
                let! res = fiber.Join()
                do! Console.printLine ($"%s{res}", id)
            }

        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! awaitAndPrint fiber35 <&&> awaitAndPrint fiber40 <&&> awaitAndPrint fiber45
        }

/// <summary>
/// Demonstrates passing command-line arguments to FIOApp.
/// </summary>
type CommandLineArgsApp(args: string array) =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            if args.Length = 0 then
                do! Console.printLine ("No command-line arguments provided", id)
                do! Console.printLine ("Try running with: dotnet run -- arg1 arg2 arg3", id)
            else
                do! Console.printLine ($"Received %d{args.Length} argument(s):", id)

                for i = 0 to args.Length - 1 do
                    do! Console.printLine ($"  Arg[%d{i}]: %s{args[i]}", id)
        }

/// <summary>
/// Demonstrates custom ConcurrentRuntime configuration.
/// </summary>
type CustomRuntimeApp() =
    inherit FIOApp<unit, exn>()

    override _.runtime =
        new ConcurrentRuntime { EWC = Environment.ProcessorCount * 2; EWS = 500; BWC = 2 }

    override _.effect =
        fio {
            do! Console.printLine ("Running with custom ConcurrentRuntime configuration:", id)
            do! Console.printLine ($"- Evaluation Workers: %d{Environment.ProcessorCount * 2}", id)
            do! Console.printLine ("- Evaluation Steps: 500", id)
            do! Console.printLine ("- Blocking Workers: 2", id)
        }

/// <summary>
/// Demonstrates shutdown hooks for cleanup on Ctrl+C.
/// </summary>
type ShutdownApp() =
    inherit FIOApp<unit, exn>()

    override _.verbose = true
    override _.onShutdownTimeout = TimeSpan.FromSeconds 5.0

    override _.onShutdown() =
        fio {
            do! Console.printLine ("Shutdown hook: Releasing resource...", id)
            do! FIO.sleep (TimeSpan.FromSeconds 1.0, id)
            do! Console.printLine ("Shutdown hook: Resource released!", id)
        }

    override _.effect =
        fio {
            do! Console.printLine ("Acquiring resource...", id)
            do! Console.printLine ("Resource acquired for 10 seconds! Press Ctrl+C to test shutdown hook.", id)

            for i in 1..10 do
                do! Console.printLine ($" - %d{i}...", id)
                do! FIO.sleep (TimeSpan.FromSeconds 1.0, id)

            do! Console.printLine ("Completed normally (no Ctrl+C)", id)
        }

/// <summary>
/// Demonstrates custom exit codes based on effect results.
/// </summary>
type CustomExitCodeApp() =
    inherit FIOApp<int, int>()

    override _.exitCodeSuccess res =
        printfn $"Success with value: {res}"
        0

    override _.exitCodeError err =
        printfn $"Failed with error code: {err}"
        err

    override _.effect =
        fio {
            do! Console.printLine ("Enter a number (1-5 for custom exit codes, 0 for success): ", fun _ -> 99)
            let! input = Console.readLine (fun _ -> 99)

            match Int32.TryParse input with
            | true, 0 -> return 0
            | true, n when n > 0 && n <= 5 -> return! FIO.fail n
            | _ -> return! FIO.fail 99
        }

/// <summary>
/// Demonstrates Environment module for system information and env vars.
/// </summary>
type EnvironmentApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            do! Console.printLine ("Environment Module Examples:", id)
            do! Console.printLine ("", id)

            // Pure values (no effects)
            do! Console.printLine ($"  ProcessorCount: {Environment.ProcessorCount}", id)
            do! Console.printLine ($"  Is64BitProcess: {Environment.Is64BitProcess}", id)
            do! Console.printLine ($"  Is64BitOperatingSystem: {Environment.Is64BitOperatingSystem}", id)
            do! Console.printLine ("", id)

            // System info effects
            let! cwd = Environment.currentDirectory ()
            do! Console.printLine ($"  CurrentDirectory: {cwd}", id)

            let! machine = Environment.machineName ()
            do! Console.printLine ($"  MachineName: {machine}", id)

            let! user = Environment.userName ()
            do! Console.printLine ($"  UserName: {user}", id)

            let! tempPath = Environment.getTempPath ()
            do! Console.printLine ($"  TempPath: {tempPath}", id)
            do! Console.printLine ("", id)

            // Environment variables
            let! pathOpt = Environment.getOption "PATH"

            match pathOpt with
            | Some path ->
                let truncated =
                    if path.Length > 50 then
                        path.Substring(0, 50) + "..."
                    else
                        path

                do! Console.printLine ($"  PATH: {truncated}", id)
            | None -> do! Console.printLine ("  PATH: (not set)", id)

            let! port = Environment.getOrDefault ("PORT", "8080")
            do! Console.printLine ($"  PORT (or default): {port}", id)

            let! homeSet = Environment.isSet "HOME"
            let! userProfileSet = Environment.isSet "USERPROFILE"
            do! Console.printLine ($"  HOME is set: {homeSet}", id)
            do! Console.printLine ($"  USERPROFILE is set: {userProfileSet}", id)

            let! timeout = Environment.getIntOrDefault ("TIMEOUT_SECONDS", 30)
            do! Console.printLine ($"  TIMEOUT_SECONDS (or default): {timeout}", id)

            let! debug = Environment.getBoolOrDefault ("DEBUG", false)
            do! Console.printLine ($"  DEBUG (or default): {debug}", id)
        }

/// <summary>
/// Demonstrates automatic startup banner display.
/// </summary>
type BannerApp() as this =
    inherit FIOApp<unit, exn>()

    override _.name = "FIO Banner Demo"
    override _.version = "1.0.0"
    override _.showBanner = true

    override _.effect =
        fio {
            do! Console.printLine ("", id)
            do! Console.printLine ("The banner above was automatically displayed!", id)
            do! Console.printLine ($"App name: {this.name}", id)
            do! Console.printLine ($"App version: {this.version}", id)
        }

/// <summary>
/// Demonstrates custom banner override.
/// </summary>
type CustomBannerApp() =
    inherit FIOApp<unit, exn>()

    override _.name = "Custom Banner App"
    override _.version = "2.0.0"
    override _.showBanner = true

    override _.banner =
        """
  ╔═══════════════════════════════════╗
  ║     🚀 Custom Banner App 🚀       ║
  ║         Version 2.0.0             ║
  ║   Powered by FIO Effect System    ║
  ╚═══════════════════════════════════╝
        """

    override _.effect =
        fio { do! Console.printLine ("This app uses a custom banner defined by overriding the 'banner' property.", id) }

/// <summary>
/// List of all app examples for sequential execution.
/// </summary>
let examples =
    [
        nameof WelcomeApp, fun () -> WelcomeApp().Run()
        nameof EnterNumberApp, fun () -> EnterNumberApp().Run()
        nameof TryWithApp, fun () -> TryWithApp().Run()
        nameof TryFinallyApp, fun () -> TryFinallyApp().Run()
        nameof TryWithFinallyApp, fun () -> TryWithFinallyApp().Run()
        nameof ForApp, fun () -> ForApp().Run()
        nameof GuessNumberApp, fun () -> GuessNumberApp().Run()
        nameof PingPongApp, fun () -> PingPongApp().Run()
        nameof PingPongCEApp, fun () -> PingPongCEApp().Run()
        nameof PingPongMatchApp, fun () -> PingPongMatchApp().Run()
        nameof ErrorHandlingApp, fun () -> ErrorHandlingApp().Run()
        nameof ErrorHandlingWithRetryApp, fun () -> ErrorHandlingWithRetryApp().Run()
        nameof AsyncErrorHandlingApp, fun () -> AsyncErrorHandlingApp().Run()
        nameof HighlyConcurrentApp, fun () -> HighlyConcurrentApp().Run()
        nameof FiberFromTaskApp, fun () -> FiberFromTaskApp().Run()
        nameof FiberFromGenericTaskApp, fun () -> FiberFromGenericTaskApp().Run()
        nameof CommandLineArgsApp, fun () -> CommandLineArgsApp([| "arg1"; "arg2"; "test" |]).Run()
        nameof CustomRuntimeApp, fun () -> CustomRuntimeApp().Run()
        nameof ShutdownApp, fun () -> ShutdownApp().Run()
        nameof CustomExitCodeApp, fun () -> CustomExitCodeApp().Run()
        nameof EnvironmentApp, fun () -> EnvironmentApp().Run()
        nameof BannerApp, fun () -> BannerApp().Run()
        nameof CustomBannerApp, fun () -> CustomBannerApp().Run()
    ]

examples
|> List.iteri (fun i (name, example) ->
    printfn $"🔥 Running example: {name}\n"
    let exitCode = example ()
    printfn $"\n🏁 Example '{name}' completed with exit code: %d{exitCode}"

    if i < examples.Length - 1 then
        System.Console.WriteLine "\n⏩ Press Enter to run next example..."
        System.Console.ReadLine() |> ignore)

System.Console.WriteLine "\n✅ All examples completed. Press Enter to exit."
System.Console.ReadLine() |> ignore
