module private FIO.Examples.App

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Net.Http
open System.Globalization

type WelcomeApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            do! Console.printLine "Hello! What is your name?" id
            let! name = Console.readLine id
            do! Console.printLine $"Hello, %s{name}! Welcome to FIO! 🪻💜" id
        }

type EnterNumberApp() =
    inherit FIOApp<string, exn>()

    override _.effect =
        fio {
            do! Console.print "Enter a number: " id
            let! input = Console.readLine id

            match! FIO.attempt (fun () -> Int32.TryParse input) id with
            | true, number -> return $"You entered the number: %i{number}."
            | false, _ -> return! FIO.fail (IOException "You entered an invalid number!")
        }

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

type TryFinallyApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                do! FIO.fail 1
                return "Successfully completed!"
            finally
                Console.printLine "Running finalizer, always executes" (fun _ -> -2)
        }

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
                Console.printLine "Running finalizer, always executes" (fun _ -> -2)
        }

type ForApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            for number in 1..10 do
                match number % 2 = 0 with
                | true -> do! Console.printLine $"%i{number} is even!" id
                | false -> do! Console.printLine $"%i{number} is odd!" id
        }

type GuessNumberApp() =
    inherit FIOApp<int, exn>()

    override _.effect =
        fio {
            let! numberToGuess = FIO.attempt (fun () -> Random.Shared.Next(1, 101)) id
            let mutable guess = -1
            let mutable aborted = false

            while guess <> numberToGuess && not aborted do
                do! Console.print "Guess a number: " id
                let! input = Console.readLine id

                match input with
                | null
                | "" ->
                    aborted <- true
                    do! Console.printLine "No input available (EOF). Aborting the guessing game." id
                | _ ->
                    match Int32.TryParse input with
                    | true, parsedInput ->
                        guess <- parsedInput

                        if guess < numberToGuess then
                            do! Console.printLine "Too low! Try again." id
                        elif guess > numberToGuess then
                            do! Console.printLine "Too high! Try again." id
                        else
                            do! Console.printLine "Congratulations! You guessed the number!" id
                    | _ -> do! Console.printLine "Invalid input. Please enter a number." id

            return guess
        }

type PingPongApp() =
    inherit FIOApp<unit, exn>()

    let pinger (pingChannel: Channel<string>) (pongChannel: Channel<string>) =
        pingChannel.Write "ping" >>= fun ping ->
        Console.printLine $"pinger sent: %s{ping}" id >>= fun _ ->
        pongChannel.Read() >>= fun pong ->
        Console.printLine $"pinger received: %s{pong}" id >>= fun _ ->
        FIO.unit ()

    let ponger (pingChannel: Channel<string>) (pongChannel: Channel<string>) =
        pingChannel.Read() >>= fun ping ->
        Console.printLine $"ponger received: %s{ping}" id >>= fun _ ->
        pongChannel.Write "pong" >>= fun pong ->
        Console.printLine $"ponger sent: %s{pong}" id >>= fun _ ->
        FIO.unit ()

    override _.effect =
        let pingChannel = Channel<string>()
        let pongChannel = Channel<string>()
        pinger pingChannel pongChannel <&&> ponger pingChannel pongChannel

type PingPongCEApp() =
    inherit FIOApp<unit, exn>()

    let pinger (pingChannel: Channel<string>) (pongChannel: Channel<string>) =
        fio {
            let! ping = pingChannel.Write "ping"
            do! Console.printLine $"pinger sent: %s{ping}" id
            let! pong = pongChannel.Read()
            do! Console.printLine $"pinger received: %s{pong}" id
        }

    let ponger (pingChannel: Channel<string>) (pongChannel: Channel<string>) =
        fio {
            let! ping = pingChannel.Read()
            do! Console.printLine $"ponger received: %s{ping}" id
            let! pong = pongChannel.Write "pong"
            do! Console.printLine $"ponger sent: %s{pong}" id
        }

    override _.effect =
        fio {
            let pingChannel = Channel<string>()
            let pongChannel = Channel<string>()
            do! pinger pingChannel pongChannel <&&> ponger pingChannel pongChannel
        }

type Message =
    | Ping
    | Pong

type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    let pinger (pingChannel: Channel<Message>) (pongChannel: Channel<Message>) =
        fio {
            let! ping = pingChannel.Write Ping
            do! Console.printLine $"pinger sent: %A{ping}" _.Message

            match! pongChannel.Read() with
            | Pong -> do! Console.printLine $"pinger received: %A{Pong}" _.Message
            | Ping -> return! FIO.fail $"pinger received %A{Ping} when %A{Pong} was expected!"
        }

    let ponger (pingChannel: Channel<Message>) (pongChannel: Channel<Message>) =
        fio {
            match! pingChannel.Read() with
            | Ping -> do! Console.printLine $"ponger received: %A{Ping}" _.Message
            | Pong -> return! FIO.fail $"ponger received %A{Pong} when %A{Ping} was expected!"

            let! sentMessage =
                fio {
                    match! FIO.attempt (fun () -> Random.Shared.Next(0, 2)) _.Message with
                    | 0 -> return! pongChannel.Write Pong
                    | _ -> return! pongChannel.Write Ping
                }

            do! Console.printLine $"ponger sent: %A{sentMessage}" _.Message
        }

    override _.effect =
        fio {
            let pingChannel = Channel<Message>()
            let pongChannel = Channel<Message>()
            do! pinger pingChannel pongChannel <&&> ponger pingChannel pongChannel
        }

type AppError =
    | DbError of bool
    | WebServiceError of int
    | GeneralError of string

type ErrorHandlingApp() =
    inherit FIOApp<string * char, AppError>()

    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt (fun () -> Random.Shared.Next(0, 2)) (fun _ -> true)
            if rand = 0 then return "data"
            else return! FIO.fail false
        }

    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt (fun () -> Random.Shared.Next(0, 2)) (fun _ -> -1)
            if rand = 1 then return 'S'
            else return! FIO.fail 404
        }

    let databaseResult: FIO<string, AppError> =
        fio {
            return! readFromDatabase
                .CatchAll(fun error -> FIO.fail (DbError error))
        }

    let webserviceResult: FIO<char, AppError> =
        fio {
            return! awaitWebservice
                .CatchAll(fun error -> FIO.fail (WebServiceError error))
        }

    override _.effect =
        fio {
            return! (databaseResult <*> webserviceResult)
                .OrElseSucceed("default", 'D')
        }

type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, AppError>()

    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt (fun () -> Random.Shared.Next(0, 2)) (fun _ -> true)
            if rand = 0 then return "data"
            else return! FIO.fail false
        }

    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt (fun () -> Random.Shared.Next(0, 2)) (fun _ -> -1)
            if rand = 1 then return 'S'
            else return! FIO.fail 404
        }

    let databaseResult: FIO<string, AppError> =
        fio {
            let onEachRetry (error, retry, maxRetries) =
                (Console.printLine $"Database read failed with error: %A{error}. Retry attempt %d{retry} of %d{maxRetries}..." id)
                    .OrElseFail false

            return! (readFromDatabase.Retry 4 onEachRetry)
                .CatchAll(fun error -> FIO.fail (DbError error))
        }

    let webserviceResult: FIO<char, AppError> =
        fio {
            let onEachRetry (error, retry, maxRetries) =
                (Console.printLine $"Webservice read failed with error: %A{error}. Retry attempt %d{retry} of %d{maxRetries}..." id)
                    .OrElseFail 400

            return! (awaitWebservice.Retry 4 onEachRetry)
                .CatchAll(fun error -> FIO.fail (WebServiceError error))
        }

    override _.effect =
        fio {
            return! (databaseResult <*> webserviceResult)
                .OrElseSucceed("default", 'D')
        }

type AsyncErrorHandlingApp() =
    inherit FIOApp<string * int, AppError>()

    let databaseReadTask: Async<string> =
        async {
            do printfn $"Reading from database..."

            if Random.Shared.Next(0, 2) = 0 then
                return "data"
            else
                return raise (Exception "Database error!")
        }

    let webserviceAwaitTask: Async<int> =
        async {
            do printfn $"Awaiting webservice..."

            if Random.Shared.Next(0, 2) = 0 then
                return 200
            else
                return raise (Exception "Webservice error!")
        }

    let databaseResult: FIO<string, AppError> =
        (FIO.awaitAsync databaseReadTask id)
            .CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    let webserviceResult: FIO<int, AppError> =
        (FIO.awaitAsync webserviceAwaitTask id)
            .CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    override _.effect =
        fio {
            return! databaseResult <&> webserviceResult
        }

type HighlyConcurrentApp() =
    inherit FIOApp<unit, exn>()

    let sender (channel: Channel<int>) senderId =
        fio {
            let! message = FIO.attempt (fun () -> Random.Shared.Next(100, 501)) id
            do! channel.Write(message).Unit()
            do! Console.printLine $"Sender[%i{senderId}] sent: %i{message}" id
        }

    let rec receiver (channel: Channel<int>) count (max: int) =
        fio {
            if count = 0 then
                let maxFibers = max.ToString("N0", CultureInfo "en-US")
                do! Console.printLine $"Successfully received a message from all %s{maxFibers} fibers!" id
            else
                let! msg = channel.Read()
                do! Console.printLine $"Receiver received: %i{msg}" id
                return! receiver channel (count - 1) max
        }

    override _.effect =
        fio {
            let fiberCount = 100_000
            let chan = Channel<int>()

            do!
                FIO.forEachParDiscard (seq { 1..fiberCount }) (sender chan)
                <&&> receiver chan fiberCount fiberCount
        }

let fib n =
    let mutable a = 0L
    let mutable b = 1L
    let mutable i = 0L

    while i < n do
        let temp = a + b
        a <- b
        b <- temp
        i <- i + 1L

    a

type FiberFromTaskApp() =
    inherit FIOApp<unit, exn>()

    let fibonacci n =
        FIO.forkTask (fun () ->
            task {
                printfn $"Task computing Fibonacci of %i{n}..."
                let value = fib n
                printfn $"Fibonacci of %i{n} is %i{value}"
                return ()
            })
            id

    override _.effect =
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

type FiberFromGenericTaskApp() =
    inherit FIOApp<unit, exn>()

    let fibonacci n =
        FIO.forkTask (fun () ->
            task {
                printfn $"Task computing Fibonacci of %i{n}..."
                let value = fib n
                return $"Fibonacci of %i{n} is %i{value}"
            })
            id

    override _.effect =
        let awaitAndPrint (fiber: Fiber<string, exn>) =
            fio {
                let! value = fiber.Join()
                do! Console.printLine $"%s{value}" id
            }

        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! awaitAndPrint fiber35 <&&> awaitAndPrint fiber40 <&&> awaitAndPrint fiber45
        }

type CommandLineArgsApp(args: string array) =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            if args.Length = 0 then
                do! Console.printLine "No command-line arguments provided" id
                do! Console.printLine "Try running with: dotnet run -- arg1 arg2 arg3" id
            else
                do! Console.printLine $"Received %d{args.Length} argument(s):" id

                for i = 0 to args.Length - 1 do
                    do! Console.printLine $"  Arg[%d{i}]: %s{args[i]}" id
        }

type CustomRuntimeApp() =
    inherit FIOApp<unit, exn>()

    override _.runtime =
        new ConcurrentRuntime {
            EvaluationWorkers = Environment.ProcessorCount * 2
            EvaluationSteps = 500
            BlockingWorkers = 2
        }

    override _.effect =
        fio {
            do! Console.printLine "Running with custom ConcurrentRuntime configuration:" id
            do! Console.printLine $" - Evaluation Workers: %d{Environment.ProcessorCount * 2}" id
            do! Console.printLine " - Evaluation Steps: 500" id
            do! Console.printLine " - Blocking Workers: 2" id
        }

type ShutdownApp() =
    inherit FIOApp<unit, exn>()

    override _.onShutdownTimeout =
        TimeSpan.FromSeconds 5.0

    override _.onShutdown() =
        fio {
            do! Console.printLine "Shutdown hook: Releasing resource..." id
            do! FIO.sleep (TimeSpan.FromSeconds 1.0) id
            do! Console.printLine "Shutdown hook: Resource released!" id
        }

    override _.effect =
        fio {
            do! Console.printLine "Acquiring resource..." id
            do! Console.printLine "Resource acquired for 10 seconds! Press Ctrl+C to test shutdown hook." id

            for i in 1..10 do
                do! Console.printLine $" - %d{i}..." id
                do! FIO.sleep (TimeSpan.FromSeconds 1.0) id

            do! Console.printLine "Completed normally (no Ctrl+C)" id
        }

type CustomExitCodeApp() =
    inherit FIOApp<int, int>()

    override _.mapExitCode outcome =
        match outcome with
        | AppSucceeded value ->
            printfn $"Success with value: {value}"
            0
        | AppFailed error ->
            printfn $"Failed with error code: {error}"
            error
        | AppInterrupted _ ->
            130
        | AppFatalError _ ->
            2

    override _.effect =
        fio {
            do! Console.printLine "Enter a number (1-5 for custom exit codes, 0 for success): " (fun _ -> 99)
            let! input = Console.readLine (fun _ -> 99)

            match Int32.TryParse input with
            | true, 0 -> return 0
            | true, n when n > 0 && n <= 5 -> return! FIO.fail n
            | _ -> return! FIO.fail 99
        }

type RaceTimeoutApp() =
    inherit FIOApp<unit, exn>()

    let delayed label ms value =
        fio {
            do! FIO.sleep (TimeSpan.FromMilliseconds(float ms)) id
            do! Console.printLine $"{label} finished" id
            return value
        }

    override _.effect =
        fio {
            let! winner = (delayed "fast (100ms)" 100 "fast").Race(delayed "slow (500ms)" 500 "slow")
            do! Console.printLine $"Race winner: {winner}" id

            match! (delayed "long task (800ms)" 800 42).Timeout (TimeSpan.FromMilliseconds 200.0) id with
            | Some value -> do! Console.printLine $"Completed with {value}" id
            | None -> do! Console.printLine "Timed out after 200ms (as expected)" id
        }

type ResourceApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        FIO.acquireReleaseWith
            (FIO.attempt (fun () -> new StreamWriter(Path.GetTempFileName())) id)
            (fun writer ->
                fio {
                    do! FIO.attempt (fun () -> writer.Dispose()) id
                    do! Console.printLine "Resource released (writer disposed)." id
                })
            (fun writer ->
                fio {
                    do! FIO.attempt (fun () -> writer.WriteLine "Hello from FIO!") id
                    do! Console.printLine "Acquired resource and wrote to a temp file." id
                })

type ParallelMapApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            let work n =
                fio {
                    do! FIO.sleep (TimeSpan.FromMilliseconds 100.0) id
                    return n * n
                }

            let! results = FIO.forEachPar [ 1..8 ] work
            do! Console.printLine $"Squares (computed in parallel): %A{results}" id
        }

type AccountMessage =
    | Deposit of int
    | Withdraw of int
    | Balance of Channel<int>

type StatefulActorApp() =
    inherit FIOApp<unit, exn>()

    let account (inbox: Channel<AccountMessage>) =
        let rec loop balance =
            inbox.Read().FlatMap(function
            | Deposit amount -> loop (balance + amount)
            | Withdraw amount -> loop (balance - amount)
            | Balance reply -> reply.Write(balance).Unit().FlatMap(fun () -> loop balance))
        loop 0

    override _.effect =
        fio {
            let inbox = Channel<AccountMessage>()
            let reply = Channel<int>()
            let! actor = (account inbox).Fork()

            do! inbox.Write(Deposit 100).Unit()
            do! inbox.Write(Withdraw 30).Unit()
            do! inbox.Write(Balance reply).Unit()

            let! balance = reply.Read()
            do! Console.printLine $"Account balance: {balance}" id
            do! actor.Interrupt ExplicitInterrupt "done"
        }

type HttpClientApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            use client = new HttpClient()
            let fetch = FIO.awaitTask (client.GetStringAsync "https://example.com") id

            match! fetch.Timeout (TimeSpan.FromSeconds 5.0) id with
            | Some body -> do! Console.printLine $"Fetched {body.Length} characters from example.com" id
            | None -> do! Console.printLine "Request timed out" id
        }

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
        nameof RaceTimeoutApp, fun () -> RaceTimeoutApp().Run()
        nameof ResourceApp, fun () -> ResourceApp().Run()
        nameof ParallelMapApp, fun () -> ParallelMapApp().Run()
        nameof StatefulActorApp, fun () -> StatefulActorApp().Run()
        nameof HttpClientApp, fun () -> HttpClientApp().Run()
    ]

examples |> List.iteri (fun index (name, example) ->
    printfn $"Running example: {name}\n"
    let exitCode = example ()
    printfn $"\nExample '{name}' completed with exit code: %d{exitCode}"

    if index < examples.Length - 1 then
        printfn "\nPress Enter to run next example..."
        Console.ReadLine() |> ignore)

printfn "\nAll examples completed. Press Enter to exit."
Console.ReadLine() |> ignore
