/// <summary>
/// FIOApp framework examples demonstrating various app patterns and features.
/// </summary>
module private FIO.Examples.App

open FIO.DSL
open FIO.App
open FIO.Random
open FIO.Runtime
open FIO.Console
open FIO.Environment
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Globalization

/// <summary>
/// Simple welcome app demonstrating console I/O with SimpleFIOApp.
/// </summary>
type WelcomeApp() =
    inherit SimpleFIOApp()

    override _.effect : FIO<unit, exn> =
        fio {
            do! Console.printLineExn "Hello! What is your name?"
            let! name = Console.readLineExn
            do! Console.printLineExn $"Hello, %s{name}! Welcome to FIO! 🪻💜"
        }

/// <summary>
/// Demonstrates parsing user input with error handling using FIOApp.
/// </summary>
type EnterNumberApp() =
    inherit FIOApp<string, exn>()

    override _.effect =
        fio {
            do! Console.printExn "Enter a number: "
            let! input = Console.readLineExn

            match! FIO.attemptExn(fun () -> Int32.TryParse input) with
            | true, number ->
                return $"You entered the number: %i{number}."
            | false, _ ->
                return! FIO.fail(IOException "You entered an invalid number!")
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
                Console.printLine("Running finalizer, always executes", (fun _ -> -2))
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
                Console.printLine("Running finalizer, always executes", (fun _ -> -2))
        }

/// <summary>
/// Demonstrates for loops in computation expressions.
/// </summary>
type ForApp() =
    inherit SimpleFIOApp()

    override _.effect =
        fio {
            for number in 1..10 do
                match number % 2 = 0 with
                | true ->
                    do! Console.printLineExn $"%i{number} is even!"
                | false -> 
                    do! Console.printLineExn $"%i{number} is odd!"
        }

/// <summary>
/// Interactive number guessing game using while loops and random numbers.
/// </summary>
type GuessNumberApp() =
    inherit FIOApp<int, exn>()

    override _.effect =
        fio {
            let! numberToGuess = Random.nextIntRange(1, 101)
            let mutable guess = -1

            while guess <> numberToGuess do
                do! Console.printExn "Guess a number: "
                let! input = Console.readLineExn

                match Int32.TryParse input with
                | true, parsedInput ->
                    guess <- parsedInput
                    if guess < numberToGuess then
                        do! Console.printLineExn "Too low! Try again."
                    elif guess > numberToGuess then
                        do! Console.printLineExn "Too high! Try again."
                    else
                        do! Console.printLineExn "Congratulations! You guessed the number!"
                | _ ->
                    do! Console.printLineExn "Invalid input. Please enter a number."

            return guess
        }

/// <summary>
/// Ping-pong message passing using operators and channels.
/// </summary>
type PingPongApp() =
    inherit SimpleFIOApp()

    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Send "ping" >>= fun ping ->
        Console.printLineExn $"pinger sent: %s{ping}" >>= fun _ ->
        chan2.Receive() >>= fun pong ->
        Console.printLineExn $"pinger received: %s{pong}" >>= fun _ ->
        FIO.unit()

    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Receive() >>= fun ping ->
        Console.printLineExn $"ponger received: %s{ping}" >>= fun _ ->
        chan2.Send "pong" >>= fun pong ->
        Console.printLineExn $"ponger sent: %s{pong}" >>= fun _ ->
        FIO.unit()
        
    override _.effect =
        let chan1 = Channel<string>()
        let chan2 = Channel<string>()
        pinger chan1 chan2 <&&> ponger chan1 chan2

/// <summary>
/// Ping-pong using computation expression syntax.
/// </summary>
type PingPongCEApp() =
    inherit SimpleFIOApp()

    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Send "ping"
            do! Console.printLineExn $"pinger sent: %s{ping}"
            let! pong = chan2.Receive()
            do! Console.printLineExn $"pinger received: %s{pong}"
        }

    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Receive()
            do! Console.printLineExn $"ponger received: %s{ping}"
            let! pong = chan2.Send "pong"
            do! Console.printLineExn $"ponger sent: %s{pong}"
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
    | PingMsg
    | PongMsg

/// <summary>
/// Ping-pong with pattern matching on message types.
/// </summary>
type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    let pinger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            let! ping = chan1.Send PingMsg
            do! Console.printLine($"pinger sent: %A{ping}", _.Message)
            
            match! chan2.Receive() with
            | PongMsg ->
                do! Console.printLine($"pinger received: %A{PongMsg}", _.Message)
            | PingMsg ->
                return! FIO.fail $"pinger received %A{PingMsg} when %A{PongMsg} was expected!"
        }

    let ponger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            match! chan1.Receive() with
            | PingMsg ->
                do! Console.printLine($"ponger received: %A{PingMsg}", _.Message)
            | PongMsg ->
                return! FIO.fail $"ponger received %A{PongMsg} when %A{PingMsg} was expected!"
            
            let! sentMsg =
                fio {
                    match! Random.nextIntRange(0, 2) with
                    | 0 -> do! chan2.Send PongMsg
                    | _ -> do! chan2.Send PingMsg
                }
            do! Console.printLine($"ponger sent: %A{sentMsg}", _.Message)
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
    | DbError of bool
    | WsError of int
    | GeneralError of string

/// <summary>
/// Demonstrates typed error handling with CatchAll and error type unification.
/// </summary>
type ErrorHandlingApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase : FIO<string, bool> =
        fio {
            let! rand = FIO.attempt((fun () -> Random().Next(0, 2)), fun _ -> true)
            if rand = 0 then
                return "data"
            else
                return! FIO.fail false
        }

    let awaitWebservice : FIO<char, int> =
        fio {
            let! rand = FIO.attempt((fun () -> Random().Next(0, 2)), fun _ -> -1)
            if rand = 1 then
                return 'S'
            else
                return! FIO.fail 404
        }

    let databaseResult : FIO<string, Error> =
        fio {
            return! readFromDatabase.CatchAll(fun error -> FIO.fail(DbError error))
        }

    let webserviceResult : FIO<char, Error> =
        fio {
            return! awaitWebservice.CatchAll(fun error -> FIO.fail(WsError error))
        }

    override _.effect =
        fio {
            return! (databaseResult <*> webserviceResult)
                    .CatchAll(fun _ -> FIO.succeed("default", 'D'))
        }

/// <summary>
/// Demonstrates retry logic with error handling callbacks.
/// </summary>
type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase : FIO<string, bool> =
        fio {
            let! rand = FIO.attempt((fun () -> Random().Next(0, 2)), fun _ -> true)
            if rand = 0 then
                return "data"
            else
                return! FIO.fail false
        }

    let awaitWebservice : FIO<char, int> =
        fio {
            let! rand = FIO.attempt((fun () -> Random().Next(0, 2)), fun _ -> -1)
            if rand = 1 then
                return 'S'
            else
                return! FIO.fail 404
        }

    let databaseResult : FIO<string, Error> =
        fio {
            let onEachRetry (err, retry, maxRetries) =
                Console.printLineExn($"Database read failed with error: %A{err}. Retry attempt %d{retry} of %d{maxRetries}...")
                 .CatchAll(fun _ -> FIO.fail false)
            return! readFromDatabase.Retry(4, onEachRetry)
                .CatchAll(fun error -> FIO.fail (DbError error))
        }

    let webserviceResult : FIO<char, Error> =
        fio {
            let onEachRetry (err, retry, maxRetries) =
                Console.printLineExn($"Webservice read failed with error: %A{err}. Retry attempt %d{retry} of %d{maxRetries}...")
                    .CatchAll(fun _ -> FIO.fail 400)
            return! awaitWebservice.Retry(4, onEachRetry)
                .CatchAll(fun error -> FIO.fail (WsError error))
        }

    override _.effect =
        fio {
            return! (databaseResult <*> webserviceResult)
                    .CatchAll(fun _ -> FIO.succeed ("default", 'D'))
        }

/// <summary>
/// Demonstrates async/task interop with FIO.awaitAsyncExn.
/// </summary>
type AsyncErrorHandlingApp() =
    inherit FIOApp<string * int, Error>()

    let databaseReadTask : Async<string> =
        async {
            do printfn $"Reading from database..."
            if Random().Next(0, 2) = 0 then
                return "data"
            else 
                raise (Exception "Database error!")
                return "error data"
        }

    let webserviceAwaitTask : Async<int> =
        async {
            do printfn $"Awaiting webservice..."
            if Random().Next(0, 2) = 0 then
                return 200
            else 
                raise (Exception "Webservice error!")
                return 400
        }

    let databaseResult : FIO<string, Error> =
        FIO.awaitAsyncExn(databaseReadTask)
            .CatchAll(fun ex -> FIO.fail(GeneralError ex.Message))

    let webserviceResult : FIO<int, Error> =
        FIO.awaitAsyncExn(webserviceAwaitTask)
            .CatchAll(fun ex -> FIO.fail(GeneralError ex.Message))

    override _.effect =
        fio {
            return! databaseResult <&> webserviceResult
        }

/// <summary>
/// Stress test with 1 million concurrent fibers using channels.
/// </summary>
type HighlyConcurrentApp() =
    inherit SimpleFIOApp()

    let sender (chan: Channel<int>) id =
        fio {
            let! msg = Random.nextIntRange(100, 501)
            do! chan.Send(msg).Unit()
            do! Console.printLineExn $"Sender[%i{id}] sent: %i{msg}"
        }

    let rec receiver (chan: Channel<int>) count (max: int) =
        fio {
            if count = 0 then
                let! maxFibers = FIO.succeed(max.ToString("N0", CultureInfo "en-US"))
                do! Console.printLineExn $"Successfully received a message from all %s{maxFibers} fibers!"
            else
                let! msg = chan.Receive()
                do! Console.printLineExn $"Receiver received: %i{msg}"
                return! receiver chan (count - 1) max
        }

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
            let acc =
                sender chan fiberCount
                <&&> receiver chan fiberCount fiberCount
            return! create chan (fiberCount - 1) acc
        }

/// <summary>
/// Demonstrates FIO.fromGenericTaskExn for wrapping .NET Tasks as fibers.
/// </summary>
type FiberFromTaskApp() =
    inherit SimpleFIOApp()

    let fibonacci n =
        FIO.fromGenericTaskExn(fun () ->
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
            })

    override _.effect : FIO<unit, exn> =
        let await (fiber: Fiber<unit, exn>) =
            fio {
                do! fiber.Join()
                return ()
            }
            
        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! await fiber35 <&&>
                await fiber40 <&&>
                await fiber45
        }

/// <summary>
/// Demonstrates FIO.fromGenericTaskExn with result values from Tasks.
/// </summary>
type FiberFromGenericTaskApp() =
    inherit SimpleFIOApp()

    let fibonacci n =
        FIO.fromGenericTaskExn(fun () ->
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
            })

    override _.effect : FIO<unit, exn> =
        let awaitAndPrint (fiber: Fiber<string, exn>) =
            fio {
                 let! res = fiber.Join()
                 do! Console.printLineExn $"%s{res}"
            }
            
        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! awaitAndPrint fiber35 <&&>
                awaitAndPrint fiber40 <&&>
                awaitAndPrint fiber45
        }

/// <summary>
/// Demonstrates passing command-line arguments to FIOApp.
/// </summary>
/// <param name="args">Command-line arguments passed to the application.</param>
type CommandLineArgsApp(args: string array) =
    inherit SimpleFIOApp()

    override _.effect =
        fio {
            if args.Length = 0 then
                do! Console.printLineExn "No command-line arguments provided"
                do! Console.printLineExn "Try running with: dotnet run -- arg1 arg2 arg3"
            else
                do! Console.printLineExn $"Received %d{args.Length} argument(s):"
                for i = 0 to args.Length - 1 do
                    do! Console.printLineExn $"  Arg[%d{i}]: %s{args[i]}"
        }

/// <summary>
/// Demonstrates custom ConcurrentRuntime configuration.
/// </summary>
type CustomRuntimeApp() =
    inherit SimpleFIOApp()

    override _.runtime =
        new ConcurrentRuntime {
            EWC = Environment.ProcessorCount * 2
            EWS = 500
            BWC = 2
        }

    override _.effect =
        fio {
            do! Console.printLineExn "Running with custom ConcurrentRuntime configuration:"
            do! Console.printLineExn $"- Evaluation Workers: %d{Environment.ProcessorCount * 2}"
            do! Console.printLineExn "- Evaluation Steps: 500"
            do! Console.printLineExn "- Blocking Workers: 2"
        }

/// <summary>
/// Demonstrates shutdown hooks for cleanup on Ctrl+C.
/// </summary>
type ShutdownHookApp() =
    inherit SimpleFIOApp()

    let mutable resourceAcquired = false

    override _.effect =
        fio {
            do! Console.printLineExn "Acquiring resource..."
            resourceAcquired <- true
            do! Console.printLineExn "Resource acquired for 10 seconds! Press Ctrl+C to test shutdown hook."
            for i in 1..10 do
                do! Console.printLineExn $" - %d{i}..."
                do! FIO.sleepExn(TimeSpan.FromSeconds 1.0)
            do! Console.printLineExn "Completed normally (no Ctrl+C)"
        }

    override _.shutdownHook () =
        fio {
            if resourceAcquired then
                do! Console.printLineExn "Shutdown hook: Releasing resource..."
                do! FIO.sleepExn(TimeSpan.FromSeconds 1.0)
                do! Console.printLineExn "Shutdown hook: Resource released!"
        }

    override _.shutdownHookTimeout =
        TimeSpan.FromSeconds 5.0

/// <summary>
/// Demonstrates custom exit codes based on effect results.
/// </summary>
type CustomExitCodeApp() =
    inherit FIOApp<int, int>()

    override _.effect =
        fio {
            do! Console.printLine("Enter a number (1-5 for custom exit codes, 0 for success): ", fun _ -> 99)
            let! input = Console.readLine(fun _ -> 99)
            match Int32.TryParse input with
            | true, 0 -> return 0
            | true, n when n > 0 && n <= 5 -> return! FIO.fail n
            | _ -> return! FIO.fail 99
        }

    override _.exitCodeSuccess res =
        printfn $"Success with value: {res}"
        0

    override _.exitCodeError err =
        printfn $"Failed with error code: {err}"
        err

/// <summary>
/// Demonstrates disabling automatic ThreadPool configuration.
/// </summary>
type DisableThreadPoolConfigApp() =
    inherit SimpleFIOApp()

    override _.configureThreadPool() =
        printfn "ThreadPool configuration disabled for this app"
        ()

    override _.effect =
        fio {
            do! Console.printLineExn "Running without automatic ThreadPool configuration"
        }

/// <summary>
/// Demonstrates Environment module for system information and env vars.
/// </summary>
type EnvironmentApp() =
    inherit SimpleFIOApp()

    override _.effect =
        fio {
            do! Console.printLineExn "Environment Module Examples:"
            do! Console.printLineExn ""

            // Pure values (no effects)
            do! Console.printLineExn $"  ProcessorCount: {Environment.ProcessorCount}"
            do! Console.printLineExn $"  Is64BitProcess: {Environment.Is64BitProcess}"
            do! Console.printLineExn $"  Is64BitOperatingSystem: {Environment.Is64BitOperatingSystem}"
            do! Console.printLineExn ""

            // System info effects
            let! cwd = Environment.currentDirectory()
            do! Console.printLineExn $"  CurrentDirectory: {cwd}"

            let! machine = Environment.machineName()
            do! Console.printLineExn $"  MachineName: {machine}"

            let! user = Environment.userName()
            do! Console.printLineExn $"  UserName: {user}"

            let! tempPath = Environment.getTempPath()
            do! Console.printLineExn $"  TempPath: {tempPath}"
            do! Console.printLineExn ""

            // Environment variables
            let! pathOpt = Environment.getOption "PATH"
            match pathOpt with
            | Some path ->
                let truncated = if path.Length > 50 then path.Substring(0, 50) + "..." else path
                do! Console.printLineExn $"  PATH: {truncated}"
            | None ->
                do! Console.printLineExn "  PATH: (not set)"

            let! port = Environment.getOrDefault("PORT", "8080")
            do! Console.printLineExn $"  PORT (or default): {port}"

            let! homeSet = Environment.isSet "HOME"
            let! userProfileSet = Environment.isSet "USERPROFILE"
            do! Console.printLineExn $"  HOME is set: {homeSet}"
            do! Console.printLineExn $"  USERPROFILE is set: {userProfileSet}"

            let! timeout = Environment.getIntOrDefault("TIMEOUT_SECONDS", 30)
            do! Console.printLineExn $"  TIMEOUT_SECONDS (or default): {timeout}"

            let! debug = Environment.getBoolOrDefault("DEBUG", false)
            do! Console.printLineExn $"  DEBUG (or default): {debug}"
        }

/// <summary>
/// Demonstrates automatic startup banner display.
/// </summary>
type BannerApp() as this =
    inherit SimpleFIOApp()

    override _.name = "FIO Banner Demo"
    override _.version = "1.0.0"
    override _.description = "Demonstrates the startup banner feature"
    override _.showBanner = true

    override _.effect =
        fio {
            do! Console.printLineExn ""
            do! Console.printLineExn "The banner above was automatically displayed!"
            do! Console.printLineExn $"App name: {this.name}"
            do! Console.printLineExn $"App version: {this.version}"
            do! Console.printLineExn $"App description: {this.description}"
        }

/// <summary>
/// Demonstrates custom banner override.
/// </summary>
type CustomBannerApp() =
    inherit SimpleFIOApp()

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
        fio {
            do! Console.printLineExn "This app uses a custom banner defined by overriding the 'banner' property."
        }

/// <summary>
/// List of all app examples for sequential execution.
/// </summary>
let examples = [
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
    nameof ShutdownHookApp, fun () -> ShutdownHookApp().Run()
    nameof CustomExitCodeApp, fun () -> CustomExitCodeApp().Run()
    nameof DisableThreadPoolConfigApp, fun () -> DisableThreadPoolConfigApp().Run()
    nameof EnvironmentApp, fun () -> EnvironmentApp().Run()
    nameof BannerApp, fun () -> BannerApp().Run()
    nameof CustomBannerApp, fun () -> CustomBannerApp().Run()
]

examples |> List.iteri (fun i (name, example) ->
    printfn $"🔥 Running example: {name}\n"
    let exitCode = example()
    printfn $"\n🏁 Example '{name}' completed with exit code: %d{exitCode}"
    if i < examples.Length - 1 then
        System.Console.WriteLine "\n⏩ Press Enter to run next example..."
        System.Console.ReadLine() |> ignore)

System.Console.WriteLine "\n✅ All examples completed. Press Enter to exit."
System.Console.ReadLine() |> ignore
