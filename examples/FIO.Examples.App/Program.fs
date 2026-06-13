module private FIO.Examples.App

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Globalization

type WelcomeApp() =
    inherit FIOApp<unit, exn>()

    override _.effect: FIO<unit, exn> =
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

            while guess <> numberToGuess do
                do! Console.print "Guess a number: " id
                let! input = Console.readLine id

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

    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Write "ping"
        >>= fun ping ->
            Console.printLine $"pinger sent: %s{ping}" id
            >>= fun _ ->
                chan2.Read()
                >>= fun pong -> Console.printLine $"pinger received: %s{pong}" id >>= fun _ -> FIO.unit ()

    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Read()
        >>= fun ping ->
            Console.printLine $"ponger received: %s{ping}" id
            >>= fun _ ->
                chan2.Write "pong"
                >>= fun pong -> Console.printLine $"ponger sent: %s{pong}" id >>= fun _ -> FIO.unit ()

    override _.effect =
        let chan1 = Channel<string>()
        let chan2 = Channel<string>()
        pinger chan1 chan2 <&&> ponger chan1 chan2

type PingPongCEApp() =
    inherit FIOApp<unit, exn>()

    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Write "ping"
            do! Console.printLine $"pinger sent: %s{ping}" id
            let! pong = chan2.Read()
            do! Console.printLine $"pinger received: %s{pong}" id
        }

    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Read()
            do! Console.printLine $"ponger received: %s{ping}" id
            let! pong = chan2.Write "pong"
            do! Console.printLine $"ponger sent: %s{pong}" id
        }

    override _.effect =
        fio {
            let chan1 = Channel<string>()
            let chan2 = Channel<string>()
            do! pinger chan1 chan2 <&&> ponger chan1 chan2
        }

type Message =
    | PingMsg
    | PongMsg

type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    let pinger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            let! ping = chan1.Write PingMsg
            do! Console.printLine $"pinger sent: %A{ping}" _.Message

            match! chan2.Read() with
            | PongMsg -> do! Console.printLine $"pinger received: %A{PongMsg}" _.Message
            | PingMsg -> return! FIO.fail $"pinger received %A{PingMsg} when %A{PongMsg} was expected!"
        }

    let ponger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            match! chan1.Read() with
            | PingMsg -> do! Console.printLine $"ponger received: %A{PingMsg}" _.Message
            | PongMsg -> return! FIO.fail $"ponger received %A{PongMsg} when %A{PingMsg} was expected!"

            let! sentMsg =
                fio {
                    match! FIO.attempt (fun () -> Random.Shared.Next(0, 2)) _.Message with
                    | 0 -> do! chan2.Write PongMsg
                    | _ -> do! chan2.Write PingMsg
                }

            do! Console.printLine $"ponger sent: %A{sentMsg}" _.Message
        }

    override _.effect =
        fio {
            let chan1 = Channel<Message>()
            let chan2 = Channel<Message>()
            do! pinger chan1 chan2 <&&> ponger chan1 chan2
        }

type Error =
    | DbError of bool
    | WsError of int
    | GeneralError of string

type ErrorHandlingApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> true)
            if rand = 0 then return "data" else return! FIO.fail false
        }

    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> -1)
            if rand = 1 then return 'S' else return! FIO.fail 404
        }

    let databaseResult: FIO<string, Error> =
        fio { return! readFromDatabase.CatchAll(fun error -> FIO.fail (DbError error)) }

    let webserviceResult: FIO<char, Error> =
        fio { return! awaitWebservice.CatchAll(fun error -> FIO.fail (WsError error)) }

    override _.effect =
        fio { return! (databaseResult <*> webserviceResult).OrElseSucceed ("default", 'D') }

type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> true)
            if rand = 0 then return "data" else return! FIO.fail false
        }

    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> -1)
            if rand = 1 then return 'S' else return! FIO.fail 404
        }

    let databaseResult: FIO<string, Error> =
        fio {
            let onEachRetry (error, retry, maxRetries) =
                (Console.printLine $"Database read failed with error: %A{error}. Retry attempt %d{retry} of %d{maxRetries}..." id)
                    .OrElseFail false

            return! (readFromDatabase.Retry 4 onEachRetry).CatchAll(fun error -> FIO.fail (DbError error))
        }

    let webserviceResult: FIO<char, Error> =
        fio {
            let onEachRetry (error, retry, maxRetries) =
                (Console.printLine $"Webservice read failed with error: %A{error}. Retry attempt %d{retry} of %d{maxRetries}..." id)
                    .OrElseFail 400

            return! (awaitWebservice.Retry 4 onEachRetry).CatchAll(fun error -> FIO.fail (WsError error))
        }

    override _.effect =
        fio { return! (databaseResult <*> webserviceResult).OrElseSucceed ("default", 'D') }

type AsyncErrorHandlingApp() =
    inherit FIOApp<string * int, Error>()

    let databaseReadTask: Async<string> =
        async {
            do printfn $"Reading from database..."

            if Random().Next(0, 2) = 0 then
                return "data"
            else
                raise (Exception "Database error!")
                return "error data"
        }

    let webserviceAwaitTask: Async<int> =
        async {
            do printfn $"Awaiting webservice..."

            if Random().Next(0, 2) = 0 then
                return 200
            else
                raise (Exception "Webservice error!")
                return 400
        }

    let databaseResult: FIO<string, Error> =
        (FIO.awaitAsync databaseReadTask id).CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    let webserviceResult: FIO<int, Error> =
        (FIO.awaitAsync webserviceAwaitTask id).CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    override _.effect = fio { return! databaseResult <&> webserviceResult }

type HighlyConcurrentApp() =
    inherit FIOApp<unit, exn>()

    let sender (chan: Channel<int>) id =
        fio {
            let! msg =
                FIO.attempt
                    (fun () -> Random.Shared.Next(100, 501))
                    Operators.id
            do! chan.Write(msg).Unit()
            do! Console.printLine $"Sender[%i{id}] sent: %i{msg}" Operators.id
        }

    let rec receiver (chan: Channel<int>) count (max: int) =
        fio {
            if count = 0 then
                let! maxFibers = FIO.succeed (max.ToString("N0", CultureInfo "en-US"))
                do! Console.printLine $"Successfully received a message from all %s{maxFibers} fibers!" Operators.id
            else
                let! msg = chan.Read()
                do! Console.printLine $"Receiver received: %i{msg}" Operators.id
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
            let acc = sender chan fiberCount <&&> receiver chan fiberCount fiberCount
            return! create chan (fiberCount - 1) acc
        }

type FiberFromTaskApp() =
    inherit FIOApp<unit, exn>()

    let fibonacci n =
        FIO.forkTask
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
                    let value = fib n
                    printfn $"Fibonacci of %i{n} is %i{value}"
                    return ()
                })
            id

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

type FiberFromGenericTaskApp() =
    inherit FIOApp<unit, exn>()

    let fibonacci n =
        FIO.forkTask
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
                    let value = fib n
                    return $"Fibonacci of %i{n} is %i{value}"
                })
            id

    override _.effect: FIO<unit, exn> =
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

    override _.onShutdownTimeout = TimeSpan.FromSeconds 5.0

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
        | AppInterrupted _ -> 130
        | AppFatalError _ -> 2

    override _.effect =
        fio {
            do! Console.printLine "Enter a number (1-5 for custom exit codes, 0 for success): " (fun _ -> 99)
            let! input = Console.readLine (fun _ -> 99)

            match Int32.TryParse input with
            | true, 0 -> return 0
            | true, n when n > 0 && n <= 5 -> return! FIO.fail n
            | _ -> return! FIO.fail 99
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
