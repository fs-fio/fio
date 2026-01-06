module private FSharp.FIO.Examples.App

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO
open FSharp.FIO.Runtime
open FSharp.FIO.Sockets
open FSharp.FIO.Runtime.Concurrent
open FSharp.FIO.WebSockets

open System
open System.IO
open System.Net
open System.Text.Json
open System.Net.Sockets
open System.Globalization
open System.Net.WebSockets

type WelcomeApp() =
    inherit SimpleFIOApp()

    override _.effect : FIO<unit, exn> =
        fio {
            do! Console.PrintLine "Hello! What is your name?"
            let! name = Console.ReadLine
            do! Console.PrintLine $"Hello, %s{name}! Welcome to FIO! ????"
        }

type EnterNumberApp() =
    inherit FIOApp<string, exn>()

    override _.effect =
        fio {
            do! Console.Print "Enter a number: "
            let! input = Console.ReadLine

            match! FIO.Attempt(fun () -> Int32.TryParse input) with
            | true, number ->
                return $"You entered the number: %i{number}."
            | false, _ ->
                return! FIO.Fail(IOException "You entered an invalid number!")
        }

type TryWithApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                do! FIO.Fail 1
                return "Successfully completed!"
            with errorCode ->
                return! FIO.Fail errorCode
        }

type TryFinallyApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                do! FIO.Fail 1
                return "Successfully completed!"
            finally
                Console.PrintLineMapError("Running finalizer, always executes", (fun _ -> -2))
        }

type TryWithFinallyApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                try
                    do! FIO.Fail 1
                    return "Successfully completed!"
                with errorCode ->
                    return! FIO.Fail errorCode
            finally
                Console.PrintLineMapError("Running finalizer, always executes", (fun _ -> -2))
        }

type ForApp() =
    inherit SimpleFIOApp()

    override _.effect =
        fio {
            for number in 1..10 do
                match number % 2 = 0 with
                | true ->
                    do! Console.PrintLine $"%i{number} is even!"
                | false -> 
                    do! Console.PrintLine $"%i{number} is odd!"
        }

type GuessNumberApp() =
    inherit FIOApp<int, exn>()

    override _.effect =
        fio {
            let! numberToGuess = FIO.Attempt(fun () -> Random().Next(1, 100))
            let mutable guess = -1

            while guess <> numberToGuess do
                do! Console.Print "Guess a number: "
                let! input = Console.ReadLine

                match Int32.TryParse input with
                | true, parsedInput ->
                    guess <- parsedInput
                    if guess < numberToGuess then
                        do! Console.PrintLine "Too low! Try again."
                    elif guess > numberToGuess then
                        do! Console.PrintLine "Too high! Try again."
                    else
                        do! Console.PrintLine "Congratulations! You guessed the number!"
                | _ ->
                    do! Console.PrintLine "Invalid input. Please enter a number."

            return guess
        }

type PingPongApp() =
    inherit SimpleFIOApp()

    let pinger (chan1: string channel) (chan2: string channel) =
        chan1.Send "ping" >>= fun ping ->
        Console.PrintLine $"pinger sent: %s{ping}" >>= fun _ ->
        chan2.Receive() >>= fun pong ->
        Console.PrintLine $"pinger received: %s{pong}" >>= fun _ ->
        FIO.Unit()

    let ponger (chan1: string channel) (chan2: string channel) =
        chan1.Receive() >>= fun ping ->
        Console.PrintLine $"ponger received: %s{ping}" >>= fun _ ->
        chan2.Send "pong" >>= fun pong ->
        Console.PrintLine $"ponger sent: %s{pong}" >>= fun _ ->
        FIO.Unit()
        
    override _.effect =
        let chan1 = Channel<string>()
        let chan2 = Channel<string>()
        pinger chan1 chan2 <&&> ponger chan1 chan2

type PingPongCEApp() =
    inherit SimpleFIOApp()

    let pinger (chan1: string channel) (chan2: string channel) =
        fio {
            let! ping = chan1.Send "ping"
            do! Console.PrintLine $"pinger sent: %s{ping}"
            let! pong = chan2.Receive()
            do! Console.PrintLine $"pinger received: %s{pong}"
        }

    let ponger (chan1: string channel) (chan2: string channel) =
        fio {
            let! ping = chan1.Receive()
            do! Console.PrintLine $"ponger received: %s{ping}"
            let! pong = chan2.Send "pong"
            do! Console.PrintLine $"ponger sent: %s{pong}"
        }

    override _.effect =
        fio {
            let chan1 = Channel<string>()
            let chan2 = Channel<string>()
            return! pinger chan1 chan2 <&&> ponger chan1 chan2
        }

type Message =
    | PingMsg
    | PongMsg

type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    let pinger (chan1: Message channel) (chan2: Message channel) =
        fio {
            let! ping = chan1.Send PingMsg
            do! Console.PrintLineMapError($"pinger sent: %A{ping}", _.Message)
            
            match! chan2.Receive() with
            | PongMsg ->
                do! Console.PrintLineMapError($"pinger received: %A{PongMsg}", _.Message)
            | PingMsg ->
                return! FIO.Fail $"pinger received %A{PingMsg} when %A{PongMsg} was expected!"
        }

    let ponger (chan1: Message channel) (chan2: Message channel) =
        fio {
            match! chan1.Receive() with
            | PingMsg ->
                do! Console.PrintLineMapError($"ponger received: %A{PingMsg}", _.Message)
            | PongMsg ->
                return! FIO.Fail $"ponger received %A{PongMsg} when %A{PingMsg} was expected!"
            
            let! sentMsg =
                match Random().Next(0, 2) with
                | 0 -> chan2.Send PongMsg
                | _ -> chan2.Send PingMsg
            do! Console.PrintLineMapError($"ponger sent: %A{sentMsg}", _.Message)
        }

    override _.effect =
        fio {
            let chan1 = Channel<Message>()
            let chan2 = Channel<Message>()
            return! pinger chan1 chan2 <&&> ponger chan1 chan2
        }

type Error =
    | DbError of bool
    | WsError of int
    | GeneralError of string

type ErrorHandlingApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase : FIO<string, bool> =
        fio {
            let! rand = FIO.Attempt((fun () -> Random().Next(0, 2)), fun _ -> true)
            if rand = 0 then
                return "data"
            else
                return! FIO.Fail false
        }

    let awaitWebservice : FIO<char, int> =
        fio {
            let! rand = FIO.Attempt((fun () -> Random().Next(0, 2)), fun _ -> -1)
            if rand = 1 then
                return 'S'
            else
                return! FIO.Fail 404
        }

    let databaseResult : FIO<string, Error> =
        fio {
            return! readFromDatabase.CatchAll(fun error -> FIO.Fail(DbError error))
        }

    let webserviceResult : FIO<char, Error> =
        fio {
            return! awaitWebservice.CatchAll(fun error -> FIO.Fail(WsError error))
        }

    override _.effect =
        fio {
            return! (databaseResult <*> webserviceResult)
                    .CatchAll(fun _ -> FIO.Succeed("default", 'D'))
        }

type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase : FIO<string, bool> =
        fio {
            let! rand = FIO.Attempt((fun () -> Random().Next(0, 2)), fun _ -> true)
            if rand = 0 then
                return "data"
            else
                return! FIO.Fail false
        }

    let awaitWebservice : FIO<char, int> =
        fio {
            let! rand = FIO.Attempt((fun () -> Random().Next(0, 2)), fun _ -> -1)
            if rand = 1 then
                return 'S'
            else
                return! FIO.Fail 404
        }

    let databaseResult : FIO<string, Error> =
        fio {
            let onEachRetry (err, retry, maxRetries) =
                Console.PrintLine($"Database read failed with error: %A{err}. Retry attempt %d{retry} of %d{maxRetries}...")
                 .CatchAll(fun _ -> FIO.Fail false)
            return! readFromDatabase.Retry(4, onEachRetry)
                .CatchAll(fun error -> FIO.Fail (DbError error))
        }

    let webserviceResult : FIO<char, Error> =
        fio {
            let onEachRetry (err, retry, maxRetries) =
                Console.PrintLine($"Webservice read failed with error: %A{err}. Retry attempt %d{retry} of %d{maxRetries}...")
                    .CatchAll(fun _ -> FIO.Fail 400)
            return! awaitWebservice.Retry(4, onEachRetry)
                .CatchAll(fun error -> FIO.Fail (WsError error))
        }

    override _.effect =
        fio {
            return! (databaseResult <*> webserviceResult)
                    .CatchAll(fun _ -> FIO.Succeed ("default", 'D'))
        }

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
        (FIO<string, exn>.AwaitAsync databaseReadTask)
           .CatchAll(fun exn -> FIO.Fail(GeneralError exn.Message))

    let webserviceResult : FIO<int, Error> =
        (FIO<int, exn>.AwaitAsync webserviceAwaitTask)
           .CatchAll(fun exn -> FIO.Fail(GeneralError exn.Message))

    override _.effect =
        fio {
            return! databaseResult <&> webserviceResult
        }

type HighlyConcurrentApp() =
    inherit SimpleFIOApp()

    let sender (chan: int channel) id (rand: Random) =
        fio {
            let! msg = FIO.Succeed(rand.Next(100, 501))
            do! chan.Send(msg).Unit()
            do! Console.PrintLine $"Sender[%i{id}] sent: %i{msg}"
        }

    let rec receiver (chan: int channel) count (max: int) =
        fio {
            if count = 0 then
                let! maxFibers = FIO.Succeed(max.ToString("N0", CultureInfo "en-US"))
                do! Console.PrintLine $"Successfully received a message from all %s{maxFibers} fibers!"
            else
                let! msg = chan.Receive()
                do! Console.PrintLine $"Receiver received: %i{msg}"
                return! receiver chan (count - 1) max
        }

    let rec create chan count acc rand =
        fio {
            if count = 0 then
                return! acc
            else
                let newAcc = sender chan count rand <&&> acc
                return! create chan (count - 1) newAcc rand
        }

    override _.effect =
        fio {
            let fiberCount = 1000000
            let chan = Channel<int>()
            let rand = Random()
            let acc = sender chan fiberCount rand
                      <&&> receiver chan fiberCount fiberCount
            return! create chan (fiberCount - 1) acc rand
        }

type FiberFromTaskApp() =
    inherit SimpleFIOApp()

    let fibonacci n =
        FIO.FromTask<unit>(fun () ->
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

type FiberFromGenericTaskApp() =
    inherit SimpleFIOApp()

    let fibonacci n =
        FIO.FromTask<unit>(fun () ->
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
                 do! Console.PrintLine $"%s{res}"
            }
            
        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! awaitAndPrint fiber35 <&&>
                awaitAndPrint fiber40 <&&>
                awaitAndPrint fiber45
        }

type SocketApp(ip: string, port: int) =
    inherit SimpleFIOApp()

    let server (ip: string) (port: int) =
    
        let receiveAndSendASCII (socket: Socket<int, string, exn>) =
            fio {
                while true do
                    let! msg = socket.Receive()
                    do! Console.PrintLine $"Server received message: %s{msg}"
                    
                    let! ascii =
                        if msg.Length > 0 then
                            FIO.Attempt(fun () -> msg.Chars 0) >>= fun c ->
                            FIO.Succeed(int c)
                        else
                            FIO.Succeed -1
                    do! socket.Send ascii
                    do! Console.PrintLine $"Server sent ASCII: %i{ascii}"
            }

        let handleClient (clientSocket: Socket<int, string, exn>) =
            fio {
                let! remoteEndPoint = clientSocket.RemoteEndPoint()
                let! endPoint = FIO.Attempt(fun () -> remoteEndPoint.ToString())
                do! Console.PrintLine $"Client connected from %s{endPoint}"
                do! receiveAndSendASCII(clientSocket).Fork().Unit()
            }
        
        fio {
            let! listener = FIO.Attempt(fun () ->
                new TcpListener(IPAddress.Parse ip, port))
            do! FIO.Attempt(fun () -> listener.Start())
            do! Console.PrintLine $"Server listening on %s{ip}:%i{port}..."
            
            while true do
                let! internalSocket = FIO.Attempt(fun () -> listener.AcceptSocket())
                let! clientSocket = Socket.Create<int, string, exn> internalSocket
                do! handleClient clientSocket
        }

    let client (ip: string) (port: int) =

        let send (socket: Socket<string, int, exn>) =
            fio {
                while true do
                    do! Console.Print "Enter a message ('exit' to quit): "
                    let! msg = Console.ReadLine
                    if msg = "exit" then
                        do! Console.PrintLine "Client exiting..."
                        return! FIO.Fail (Exception "Client exited.")
                    do! socket.Send msg
                    do! Console.Print $"Client sent message: %s{msg}"
            }

        let receive (socket: Socket<string, int, exn>) =
            fio {
                while true do
                    let! ascii = socket.Receive()
                    do! Console.PrintLine $"Client received ASCII: %i{ascii}"
            }
    
        fio {
            do! Console.PrintLine $"Connecting to %s{ip}:%i{port}..."
            let! internalSocket = FIO.Attempt(fun () ->
                new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
            let! socket = Socket<string, int, exn>.Create(internalSocket, ip, port)
            do! Console.PrintLine $"Connected to %s{ip}:%i{port}"
            do! send socket <&&> receive socket
        }

    override _.effect =
        fio {
            do! client ip port <&&> server ip port
        }

type WebSocketApp(serverUrl, clientUrl) =
    inherit SimpleFIOApp()

    let server url =
        let receiveAndSendASCII (ws: WebSocket<exn>, server: WebSocketServer<exn>) =
            fio {
                let mutable loop = true
                while loop do
                    match! ws.ReceiveMessage() with
                    | Frame(Text text) ->
                        do! Console.PrintLine $"Server received message: %s{text}"
                        let! ascii =
                            if text.Length > 0 then
                                FIO.Attempt(fun () -> text.Chars 0) >>= fun c ->
                                FIO.Succeed(int c)
                            else
                                FIO.Succeed -1
                        do! ws.SendJson ascii
                        do! Console.PrintLine $"Server sent ASCII: %i{ascii}"
                    | ConnectionClosed(status, desc) ->
                        do! Console.PrintLine $"Server connection closed. Status: {status}, Description: {desc}"
                        do! ws.Close(WebSocketCloseStatus.NormalClosure, "Server closing connection")
                        do! server.Close()
                        loop <- false
                    | msg ->
                        do! Console.PrintLine $"Server received and ignored message: %A{msg}"
            }

        let handleClient (ws: WebSocket<exn>, server: WebSocketServer<exn>) =
            fio {
                do! Console.PrintLine "Client connected"
                do! receiveAndSendASCII(ws, server).Fork().Unit()
            }

        fio {
            let! server = WebSocketServer<exn>.Create()
            do! server.Start url
            do! Console.PrintLine $"Server listening on %s{url}..."
            
            while true do
                let! ws = server.Accept().CatchAll(fun _ ->
                    FIO.Fail (Exception "Failed to accept WebSocket connection"))
                do! handleClient(ws, server)
        }

    let client url =
        let send (ws: WebSocket<exn>) =
            fio {
                let mutable loop = true
                while loop do
                    do! Console.Print "Enter a message ('exit' to quit application): "
                    let! msg = Console.ReadLine
                    if msg = "exit" then
                        do! Console.PrintLine "Client exiting..."
                        do! ws.Close(WebSocketCloseStatus.NormalClosure, "Client exiting")
                        loop <- false
                    else
                        do! ws.SendText msg
                        do! Console.PrintLine $"Client sent message: %s{msg}"
            }

        let receive (ws: WebSocket<exn>) =
            fio {
                let mutable loop = true
                while loop do
                    match! ws.ReceiveMessage() with
                    | Frame(Text text) ->
                        let! ascii = FIO.Attempt (fun () -> JsonSerializer.Deserialize<int> text)
                        do! Console.PrintLine $"Client received ASCII: %i{ascii}"
                    | ConnectionClosed(status, desc) ->
                        do! Console.PrintLine $"Client connection closed. Status: {status}, Description: {desc}"
                        loop <- false
                    | msg ->
                        do! Console.PrintLine $"Client received and ignored message: %A{msg}"
            }

        fio {
            let! client = WebSocketClient<exn>.Create()
            let! ws = client.Connect(Uri url)
            do! send ws <&> receive ws
        }

    override _.effect =
        fio {
            do! client clientUrl <&&> server serverUrl
        }

type CommandLineArgsApp(args: string array) =
    inherit SimpleFIOApp()

    override this.effect =
        fio {
            if args.Length = 0 then
                do! Console.PrintLine "No command-line arguments provided"
                do! Console.PrintLine "Try running with: dotnet run -- arg1 arg2 arg3"
            else
                do! Console.PrintLine $"Received %d{args.Length} argument(s):"
                for i = 0 to args.Length - 1 do
                    do! Console.PrintLine $"  Arg[%d{i}]: %s{args[i]}"
        }

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
            do! Console.PrintLine "Running with custom ConcurrentRuntime configuration:"
            do! Console.PrintLine $"- Evaluation Workers: %d{Environment.ProcessorCount * 2}"
            do! Console.PrintLine "- Evaluation Steps: 500"
            do! Console.PrintLine "- Blocking Workers: 2"
        }

type ShutdownHookApp() =
    inherit SimpleFIOApp()

    let mutable resourceAcquired = false

    override _.effect =
        fio {
            do! Console.PrintLine "Acquiring resource..."
            resourceAcquired <- true
            do! Console.PrintLine "Resource acquired for 10 seconds! Press Ctrl+C to test shutdown hook."
            for i in 1..10 do
                do! Console.PrintLine $" - %d{i}..."
                do! FIO.Sleep(TimeSpan.FromSeconds 1.0)
            do! Console.PrintLine "Completed normally (no Ctrl+C)"
        }

    override _.shutdownHook () =
        fio {
            if resourceAcquired then
                do! Console.PrintLine "Shutdown hook: Releasing resource..."
                do! FIO.Sleep(TimeSpan.FromSeconds 1.0)
                do! Console.PrintLine "Shutdown hook: Resource released!"
        }

    override _.shutdownHookTimeout =
        TimeSpan.FromSeconds 5.0

type CustomExitCodeApp() =
    inherit FIOApp<int, int>()

    override _.effect =
        fio {
            do! Console.PrintLineMapError("Enter a number (1-5 for custom exit codes, 0 for success): ", fun _ -> 99)
            let! input = Console.ReadLineMapError(fun _ -> 99)
            match Int32.TryParse input with
            | true, 0 -> return 0
            | true, n when n > 0 && n <= 5 -> return! FIO.Fail n
            | _ -> return! FIO.Fail 99
        }

    override _.exitCodeSuccess res =
        printfn $"Success with value: {res}"
        0

    override _.exitCodeError err =
        printfn $"Failed with error code: {err}"
        err

type DisableThreadPoolConfigApp() =
    inherit SimpleFIOApp()

    override _.configureThreadPool() =
        printfn "ThreadPool configuration disabled for this app"
        ()

    override _.effect =
        fio {
            do! Console.PrintLine "Running without automatic ThreadPool configuration"
        }

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
    nameof SocketApp, fun () -> SocketApp("127.0.0.1", 5000).Run()
    nameof WebSocketApp, fun () -> WebSocketApp("http://localhost:8080/", "ws://localhost:8080/").Run()
    nameof CommandLineArgsApp, fun () -> CommandLineArgsApp([| "arg1"; "arg2"; "test" |]).Run()
    nameof CustomRuntimeApp, fun () -> CustomRuntimeApp().Run()
    nameof ShutdownHookApp, fun () -> ShutdownHookApp().Run()
    nameof CustomExitCodeApp, fun () -> CustomExitCodeApp().Run()
    nameof DisableThreadPoolConfigApp, fun () -> DisableThreadPoolConfigApp().Run()
]

examples |> List.iteri (fun i (name, example) ->
    printfn $"?? Running example: {name}\n"
    let exitCode = example()
    printfn $"\n?? Example '{name}' completed with exit code: %d{exitCode}"
    if i < examples.Length - 1 then
        System.Console.WriteLine "\n? Press Enter to run next example..."
        System.Console.ReadLine() |> ignore)

System.Console.WriteLine "\n? All examples completed. Press Enter to exit."
System.Console.ReadLine() |> ignore
