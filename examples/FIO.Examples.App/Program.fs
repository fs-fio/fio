/// <summary>Provides FIOApp framework examples demonstrating app lifecycle, error handling, channel messaging, concurrency, Task interop, runtime configuration, and shutdown hooks.</summary>
module private FIO.Examples.App

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.Runtime.Concurrent

open System
open System.IO
open System.Globalization

/// <summary>Represents a basic FIOApp that reads the user's name from the console and prints a personalized greeting.</summary>
/// <remarks>Demonstrates the simplest FIOApp pattern: override effect with a fio CE that performs console I/O and returns unit.</remarks>
type WelcomeApp() =
    inherit FIOApp<unit, exn>()

    override _.effect: FIO<unit, exn> =
        fio {
            do! Console.printLine "Hello! What is your name?" id
            let! name = Console.readLine id
            do! Console.printLine $"Hello, %s{name}! Welcome to FIO! 🪻💜" id
        }

/// <summary>Represents an FIOApp that parses user input into an integer, returning a success message or failing with an IOException for invalid input.</summary>
/// <remarks>Demonstrates FIO.attempt for wrapping side-effecting .NET code and returning typed errors through FIO.fail when parsing fails.</remarks>
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

/// <summary>Represents an FIOApp that uses try-with inside a fio CE to catch a typed integer error and re-raise it, demonstrating error interception in computation expressions.</summary>
/// <remarks>Shows how try-with in the fio CE corresponds to CatchAll — the with branch receives the typed error value, not a .NET exception.</remarks>
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

/// <summary>Represents an FIOApp that uses try-finally inside a fio CE to guarantee a cleanup effect runs regardless of whether the main effect succeeds or fails.</summary>
/// <remarks>Shows how try-finally in the fio CE corresponds to Ensuring — the finally block runs on success, failure, and interruption.</remarks>
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

/// <summary>Represents an FIOApp that nests try-with inside try-finally, demonstrating how error recovery and guaranteed cleanup compose together in the fio CE.</summary>
/// <remarks>The inner try-with catches and re-raises the error while the outer try-finally ensures the cleanup effect always executes.</remarks>
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

/// <summary>Represents an FIOApp that iterates over a range using a for loop inside the fio CE, printing whether each number is even or odd.</summary>
/// <remarks>Demonstrates that the fio CE supports for loops, sequencing an effectful body for each element in the range.</remarks>
type ForApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            for number in 1..10 do
                match number % 2 = 0 with
                | true -> do! Console.printLine $"%i{number} is even!" id
                | false -> do! Console.printLine $"%i{number} is odd!" id
        }

/// <summary>Represents an interactive FIOApp that generates a random target number and loops until the user guesses it correctly, providing higher/lower feedback.</summary>
/// <remarks>Demonstrates while loops, mutable state, FIO.attempt for wrapping Random.Shared.Next as an FIO effect, and input parsing inside a fio CE for an interactive game loop.</remarks>
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

/// <summary>Represents an FIOApp that exchanges ping-pong messages between two fibers using channels and the &gt;&gt;= bind operator for composition.</summary>
/// <remarks>Demonstrates Channel-based message passing: one fiber sends on chan1 and receives on chan2, while the other does the reverse, running concurrently via &lt;&amp;&amp;&gt;.</remarks>
type PingPongApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>Builds an effect that sends a ping message on chan1, then receives the reply on chan2, using the &gt;&gt;= bind operator for sequential composition.</summary>
    /// <param name="chan1">The channel to send the ping message on.</param>
    /// <param name="chan2">The channel to receive the pong reply from.</param>
    /// <returns>An effect that completes after the ping-pong exchange is printed to the console.</returns>
    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        chan1.Write "ping"
        >>= fun ping ->
            Console.printLine $"pinger sent: %s{ping}" id
            >>= fun _ ->
                chan2.Read()
                >>= fun pong -> Console.printLine $"pinger received: %s{pong}" id >>= fun _ -> FIO.unit ()

    /// <summary>Builds an effect that receives a ping message from chan1, then sends a pong reply on chan2, using the &gt;&gt;= bind operator for sequential composition.</summary>
    /// <param name="chan1">The channel to receive the ping message from.</param>
    /// <param name="chan2">The channel to send the pong reply on.</param>
    /// <returns>An effect that completes after the ping-pong exchange is printed to the console.</returns>
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

/// <summary>Represents an FIOApp that exchanges ping-pong messages between two fibers using channels and fio computation expression syntax.</summary>
/// <remarks>Functionally identical to PingPongApp but uses the fio CE with let! and do! instead of &gt;&gt;= operators, showing the more readable CE style for channel messaging.</remarks>
type PingPongCEApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>Builds an effect that sends a ping on chan1 and awaits the pong reply on chan2, using fio CE syntax with let! and do!.</summary>
    /// <param name="chan1">The channel to send the ping message on.</param>
    /// <param name="chan2">The channel to receive the pong reply from.</param>
    /// <returns>An effect that completes after printing the sent and received messages.</returns>
    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1.Write "ping"
            do! Console.printLine $"pinger sent: %s{ping}" id
            let! pong = chan2.Read()
            do! Console.printLine $"pinger received: %s{pong}" id
        }

    /// <summary>Builds an effect that receives a ping from chan1 and sends a pong reply on chan2, using fio CE syntax with let! and do!.</summary>
    /// <param name="chan1">The channel to receive the ping message from.</param>
    /// <param name="chan2">The channel to send the pong reply on.</param>
    /// <returns>An effect that completes after printing the received and sent messages.</returns>
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

/// <summary>Represents a typed message exchanged between pinger and ponger fibers over channels.</summary>
type Message =
    /// <summary>Represents a ping request sent by the pinger fiber.</summary>
    | PingMsg
    /// <summary>Represents a pong reply sent by the ponger fiber.</summary>
    | PongMsg

/// <summary>Represents an FIOApp that exchanges typed Message values over channels and uses pattern matching to validate that the correct message type was received.</summary>
/// <remarks>Demonstrates match! in the fio CE for pattern matching on channel-received values, and FIO.fail for protocol violations when an unexpected message type arrives.</remarks>
type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    /// <summary>Builds an effect that sends a PingMsg on chan1 and validates the reply from chan2 with pattern matching, failing if a PingMsg is received instead of PongMsg.</summary>
    /// <param name="chan1">The channel to send the PingMsg on.</param>
    /// <param name="chan2">The channel to receive and validate the reply from.</param>
    /// <returns>An effect that completes after the exchange or fails with a protocol-violation string error.</returns>
    let pinger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            let! ping = chan1.Write PingMsg
            do! Console.printLine $"pinger sent: %A{ping}" _.Message

            match! chan2.Read() with
            | PongMsg -> do! Console.printLine $"pinger received: %A{PongMsg}" _.Message
            | PingMsg -> return! FIO.fail $"pinger received %A{PingMsg} when %A{PongMsg} was expected!"
        }

    /// <summary>Builds an effect that receives a PingMsg from chan1, validates it with pattern matching, and replies on chan2 with a randomly chosen message type.</summary>
    /// <param name="chan1">The channel to receive and validate the ping message from.</param>
    /// <param name="chan2">The channel to send the reply message on.</param>
    /// <returns>An effect that completes after replying or fails with a protocol-violation string error.</returns>
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

/// <summary>Represents a typed error hierarchy for demonstrating error type unification across multiple data sources.</summary>
type Error =
    /// <summary>Represents a database operation failure carrying a boolean status flag.</summary>
    | DbError of bool
    /// <summary>Represents a web service failure carrying an integer status code.</summary>
    | WsError of int
    /// <summary>Represents a general failure carrying a descriptive error message string.</summary>
    | GeneralError of string

/// <summary>Represents an FIOApp that composes a database read and web service call with unified error types, falling back to defaults on failure using CatchAll.</summary>
/// <remarks>Demonstrates typed error unification: each data source has its own error type (bool, int), and CatchAll wraps each into the shared Error DU before combining with &lt;*&gt;.</remarks>
type ErrorHandlingApp() =
    inherit FIOApp<string * char, Error>()

    /// <summary>Builds an effect that simulates a database read, randomly succeeding with "data" or failing with a boolean error.</summary>
    /// <returns>An effect that produces a string on success or fails with a bool error value.</returns>
    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> true)
            if rand = 0 then return "data" else return! FIO.fail false
        }

    /// <summary>Builds an effect that simulates a web service call, randomly succeeding with 'S' or failing with an integer status code.</summary>
    /// <returns>An effect that produces a char on success or fails with an int error value.</returns>
    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> -1)
            if rand = 1 then return 'S' else return! FIO.fail 404
        }

    /// <summary>Builds an effect that wraps the database read result, unifying its bool error into the shared Error DU via CatchAll.</summary>
    /// <returns>An effect that produces a string on success or fails with an Error value.</returns>
    let databaseResult: FIO<string, Error> =
        fio { return! readFromDatabase.CatchAll(fun error -> FIO.fail (DbError error)) }

    /// <summary>Builds an effect that wraps the web service result, unifying its int error into the shared Error DU via CatchAll.</summary>
    /// <returns>An effect that produces a char on success or fails with an Error value.</returns>
    let webserviceResult: FIO<char, Error> =
        fio { return! awaitWebservice.CatchAll(fun error -> FIO.fail (WsError error)) }

    override _.effect =
        fio { return! (databaseResult <*> webserviceResult).OrElseSucceed ("default", 'D') }

/// <summary>Represents an FIOApp that retries failing database and web service effects with configurable retry counts and per-retry logging callbacks before falling back to defaults.</summary>
/// <remarks>Demonstrates Retry for automatic retries with a callback that logs each attempt, combined with CatchAll for error type unification and final fallback.</remarks>
type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, Error>()

    /// <summary>Builds an effect that simulates a database read with random success or failure, used as the base effect for retry demonstration.</summary>
    /// <returns>An effect that produces a string on success or fails with a bool error value.</returns>
    let readFromDatabase: FIO<string, bool> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> true)
            if rand = 0 then return "data" else return! FIO.fail false
        }

    /// <summary>Builds an effect that simulates a web service call with random success or failure, used as the base effect for retry demonstration.</summary>
    /// <returns>An effect that produces a char on success or fails with an int error value.</returns>
    let awaitWebservice: FIO<char, int> =
        fio {
            let! rand = FIO.attempt (fun () -> Random().Next(0, 2)) (fun _ -> -1)
            if rand = 1 then return 'S' else return! FIO.fail 404
        }

    /// <summary>Builds an effect that retries the database read up to four times with per-retry logging, then unifies the error type into the shared Error DU.</summary>
    /// <returns>An effect that produces a string on success or fails with an Error value after exhausting retries.</returns>
    let databaseResult: FIO<string, Error> =
        fio {
            let onEachRetry (error, retry, maxRetries) =
                (Console.printLine $"Database read failed with error: %A{error}. Retry attempt %d{retry} of %d{maxRetries}..." id)
                    .OrElseFail false

            return! (readFromDatabase.Retry 4 onEachRetry).CatchAll(fun error -> FIO.fail (DbError error))
        }

    /// <summary>Builds an effect that retries the web service call up to four times with per-retry logging, then unifies the error type into the shared Error DU.</summary>
    /// <returns>An effect that produces a char on success or fails with an Error value after exhausting retries.</returns>
    let webserviceResult: FIO<char, Error> =
        fio {
            let onEachRetry (error, retry, maxRetries) =
                (Console.printLine $"Webservice read failed with error: %A{error}. Retry attempt %d{retry} of %d{maxRetries}..." id)
                    .OrElseFail 400

            return! (awaitWebservice.Retry 4 onEachRetry).CatchAll(fun error -> FIO.fail (WsError error))
        }

    override _.effect =
        fio { return! (databaseResult <*> webserviceResult).OrElseSucceed ("default", 'D') }

/// <summary>Represents an FIOApp that wraps F# Async computations into FIO effects using FIO.awaitAsync, demonstrating Task/Async interop with typed error handling.</summary>
/// <remarks>Shows how to bridge existing async code into the FIO effect system: FIO.awaitAsync lifts an Async into an FIO, and CatchAll converts thrown exceptions into the typed Error DU.</remarks>
type AsyncErrorHandlingApp() =
    inherit FIOApp<string * int, Error>()

    /// <summary>Creates an F# Async computation that simulates a database read, randomly returning data or raising an exception.</summary>
    /// <returns>An Async that produces a string on success or raises an Exception on failure.</returns>
    let databaseReadTask: Async<string> =
        async {
            do printfn $"Reading from database..."

            if Random().Next(0, 2) = 0 then
                return "data"
            else
                raise (Exception "Database error!")
                return "error data"
        }

    /// <summary>Creates an F# Async computation that simulates a web service call, randomly returning a status code or raising an exception.</summary>
    /// <returns>An Async that produces an int status code on success or raises an Exception on failure.</returns>
    let webserviceAwaitTask: Async<int> =
        async {
            do printfn $"Awaiting webservice..."

            if Random().Next(0, 2) = 0 then
                return 200
            else
                raise (Exception "Webservice error!")
                return 400
        }

    /// <summary>Builds an effect that lifts the database Async into FIO and unifies its exception into the Error DU via CatchAll.</summary>
    /// <returns>An effect that produces a string on success or fails with an Error value.</returns>
    let databaseResult: FIO<string, Error> =
        (FIO.awaitAsync databaseReadTask id).CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    /// <summary>Builds an effect that lifts the web service Async into FIO and unifies its exception into the Error DU via CatchAll.</summary>
    /// <returns>An effect that produces an int status code on success or fails with an Error value.</returns>
    let webserviceResult: FIO<int, Error> =
        (FIO.awaitAsync webserviceAwaitTask id).CatchAll(fun ex -> FIO.fail (GeneralError ex.Message))

    override _.effect = fio { return! databaseResult <&> webserviceResult }

/// <summary>Represents an FIOApp stress test that spawns one million concurrent sender fibers communicating with a single receiver fiber through a shared channel.</summary>
/// <remarks>Demonstrates FIO's lightweight fiber scalability: each sender writes a random integer to the channel, and the receiver consumes all messages sequentially.</remarks>
type HighlyConcurrentApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>Builds an effect that generates a random integer and sends it on the channel, printing the sent value to the console.</summary>
    /// <param name="chan">The channel to send the random message on.</param>
    /// <param name="id">The numeric identifier of this sender fiber for display purposes.</param>
    /// <returns>An effect that completes after sending one message and printing a confirmation.</returns>
    let sender (chan: Channel<int>) id =
        fio {
            let! msg =
                FIO.attempt
                    (fun () -> Random.Shared.Next(100, 501))
                    Operators.id
            do! chan.Write(msg).Unit()
            do! Console.printLine $"Sender[%i{id}] sent: %i{msg}" Operators.id
        }

    /// <summary>Builds a recursive effect that receives messages from the channel one at a time until all expected messages have arrived.</summary>
    /// <param name="chan">The channel to receive messages from.</param>
    /// <param name="count">The number of remaining messages to receive before completion.</param>
    /// <param name="max">The total number of fibers, used for the completion summary message.</param>
    /// <returns>An effect that completes after receiving all expected messages.</returns>
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

    /// <summary>Builds a recursive effect that composes sender fibers in parallel using &lt;&amp;&amp;&gt;, accumulating them into a single combined effect.</summary>
    /// <param name="chan">The channel that each created sender fiber will write to.</param>
    /// <param name="count">The number of remaining sender fibers to create.</param>
    /// <param name="acc">The accumulated effect combining all previously created senders with the receiver.</param>
    /// <returns>An effect that runs all sender fibers and the receiver concurrently.</returns>
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

/// <summary>Represents an FIOApp that wraps .NET Tasks as FIO fibers using FIO.forkTask, demonstrating how to run Task-based computations within the fiber runtime.</summary>
/// <remarks>Uses FIO.forkTask to fork each Fibonacci computation as a managed fiber, then joins all three concurrently with &lt;&amp;&amp;&gt;.</remarks>
type FiberFromTaskApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>Builds an effect that computes a Fibonacci number inside a .NET Task, wrapped as a forkable FIO fiber via FIO.forkTask.</summary>
    /// <param name="n">The Fibonacci index to compute.</param>
    /// <returns>An effect that forks the Task and produces a Fiber that can be joined to await completion.</returns>
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

/// <summary>Represents an FIOApp that wraps .NET Tasks returning string results as FIO fibers, demonstrating how to retrieve typed results from Task-based computations.</summary>
/// <remarks>Similar to FiberFromTaskApp but the Tasks return result strings, showing how FIO.forkTask preserves the Task's return type through the fiber.</remarks>
type FiberFromGenericTaskApp() =
    inherit FIOApp<unit, exn>()

    /// <summary>Builds an effect that computes a Fibonacci number inside a .NET Task returning a formatted result string, wrapped as a forkable FIO fiber.</summary>
    /// <param name="n">The Fibonacci index to compute.</param>
    /// <returns>An effect that forks the Task and produces a Fiber whose join result is the formatted Fibonacci string.</returns>
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

/// <summary>Represents an FIOApp that accepts command-line arguments via its constructor and prints each argument with its index.</summary>
/// <remarks>Demonstrates how to pass external configuration into an FIOApp by accepting parameters in the constructor and referencing them in the effect.</remarks>
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

/// <summary>Represents an FIOApp that overrides the runtime property to supply a custom ConcurrentRuntime with tuned worker configuration parameters.</summary>
/// <remarks>Demonstrates runtime customization: EvaluationWorkers controls evaluation worker count, EvaluationSteps sets steps per work item before rescheduling, and BlockingWorkers sets blocking worker count.</remarks>
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

/// <summary>Represents an FIOApp that registers a shutdown hook via onShutdown, demonstrating cleanup behavior when the app is interrupted with Ctrl+C.</summary>
/// <remarks>The onShutdown effect runs when the process receives a termination signal, with onShutdownTimeout limiting how long cleanup may take before forced exit.</remarks>
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

/// <summary>Represents an FIOApp that overrides <c>mapExitCode</c> to translate effect outcomes into custom process exit codes.</summary>
/// <remarks>Demonstrates exit-code customization via the unified <c>AppOutcome</c> mapping: the effect returns an int on success or fails with an int, and the override translates each outcome to the process exit code.</remarks>
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

/// <summary>Provides the registry of all FIOApp example names paired with their runner functions for sequential interactive execution.</summary>
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
