(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module private FSharp.FIO.Examples.App

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.Lib.IO
open FSharp.FIO.Lib.Net.Sockets
open FSharp.FIO.Lib.Net.WebSockets

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Globalization
open System.Net.WebSockets

type WelcomeApp() =
    inherit FIOApp<unit, exn>()

    override _.effect : FIO<unit, exn> =
        fio {
            do! FConsole.PrintLine "Hello! What is your name?"
            let! name = FConsole.ReadLine ()
            do! FConsole.PrintLine $"Hello, %s{name}! Welcome to FIO! 🪻💜"
        }

type EnterNumberApp() =
    inherit FIOApp<string, exn>()

    override _.effect =
        fio {
            do! FConsole.Print "Enter a number: "
            let! input = FConsole.ReadLine ()

            match! !<< (fun () -> Int32.TryParse input) with
            | true, number ->
                return $"You entered the number: %i{number}."
            | false, _ ->
                return! !- IOException("You entered an invalid number!")
        }

type TryCatchApp() =
    inherit FIOApp<string, int>()

    override _.effect =
        fio {
            try
                do! !- 1
                return "Successfully completed!"
            with errorCode ->
                return! !- errorCode
        }

type ForApp() =
    inherit FIOApp<unit, exn>()

    override _.effect =
        fio {
            for number in 1..10 do
                match! !<< (fun () -> number % 2 = 0) with
                | true -> do! FConsole.PrintLine $"%i{number} is even!"
                | false -> do! FConsole.PrintLine $"%i{number} is odd!"
        }

type GuessNumberApp() =
    inherit FIOApp<int, exn>()

    override _.effect =
        fio {
            let! numberToGuess = !<< (fun () -> Random().Next(1, 100))
            let mutable guess = -1

            while guess <> numberToGuess do
                do! FConsole.Print "Guess a number: "
                let! input = FConsole.ReadLine ()

                match! !<< (fun () -> Int32.TryParse input) with
                | true, parsedInput ->
                    guess <- parsedInput
                    if guess < numberToGuess then
                        do! FConsole.PrintLine "Too low! Try again."
                    elif guess > numberToGuess then
                        do! FConsole.PrintLine "Too high! Try again."
                    else
                        do! FConsole.PrintLine "Congratulations! You guessed the number!"
                | _ ->
                    do! FConsole.PrintLine "Invalid input. Please enter a number."

            return guess
        }

type PingPongApp() =
    inherit FIOApp<unit, exn>()

    let pinger chan1 chan2 =
        "ping" --> chan1 >>= fun ping ->
        FConsole.PrintLine $"pinger sent: %s{ping}" >>= fun _ ->
        !--> chan2 >>= fun pong ->
        FConsole.PrintLine $"pinger received: %s{pong}" >>= fun _ ->
        !+ ()

    let ponger chan1 chan2 =
        !--> chan1 >>= fun ping ->
        FConsole.PrintLine $"ponger received: %s{ping}" >>= fun _ ->
        "pong" --> chan2 >>= fun pong ->
        FConsole.PrintLine $"ponger sent: %s{pong}" >>= fun _ ->
        !+ ()

    override _.effect =
        let chan1 = Channel<string>()
        let chan2 = Channel<string>()
        pinger chan1 chan2 <~> ponger chan1 chan2

type PingPongCEApp() =
    inherit FIOApp<unit, exn>()

    let pinger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = chan1 <-- "ping"
            do! FConsole.PrintLine $"pinger sent: %s{ping}"
            let! pong = !<-- chan2
            do! FConsole.PrintLine $"pinger received: %s{pong}"
        }

    let ponger (chan1: Channel<string>) (chan2: Channel<string>) =
        fio {
            let! ping = !<-- chan1
            do! FConsole.PrintLine $"ponger received: %s{ping}"
            let! pong = chan2 <-- "pong"
            do! FConsole.PrintLine $"ponger sent: %s{pong}"
        }

    override _.effect =
        fio {
            let chan1 = Channel<string>()
            let chan2 = Channel<string>()
            return! pinger chan1 chan2 <~> ponger chan1 chan2
        }

type Message =
    | Ping
    | Pong

type PingPongMatchApp() =
    inherit FIOApp<unit, string>()

    let pinger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            let! ping = chan1 <-- Ping
            do! FConsole.PrintLineMapError ($"pinger sent: %A{ping}", _.Message)
            
            match! !<-- chan2 with
            | Pong -> do! FConsole.PrintLineMapError ($"pinger received: %A{Pong}", _.Message)
            | Ping -> return! !- $"pinger received %A{Ping} when %A{Pong} was expected!"
        }

    let ponger (chan1: Channel<Message>) (chan2: Channel<Message>) =
        fio {
            match! !<-- chan1 with
            | Ping -> do! FConsole.PrintLineMapError ($"ponger received: %A{Ping}", _.Message)
            | Pong -> return! !- $"ponger received %A{Pong} when %A{Ping} was expected!"
            
            let! sentMsg =
                match Random().Next(0, 2) with
                | 0 -> chan2 <-- Pong
                | _ -> chan2 <-- Ping
            do! FConsole.PrintLineMapError ($"ponger sent: %A{sentMsg}", _.Message)
        }

    override _.effect =
        fio {
            let chan1 = Channel<Message>()
            let chan2 = Channel<Message>()
            return! pinger chan1 chan2 <~> ponger chan1 chan2
        }

type Error =
    | DbError of bool
    | WsError of int
    | GeneralError of string

type ErrorHandlingApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase : FIO<string, bool> =
        fio {
            let! rand = !<<< (fun () -> Random().Next(0, 2)) (fun _ -> true)
            if rand = 0 then
                return "data"
            else
                return! !- false
        }

    let awaitWebservice : FIO<char, int> =
        fio {
            let! rand = !<<< (fun () -> Random().Next(0, 2)) (fun _ -> -1)
            if rand = 1 then
                return 'S'
            else
                return! !- 404
        }

    let databaseResult : FIO<string, Error> =
        fio {
            return! readFromDatabase >>=? fun error -> !- (DbError error)
        }

    let webserviceResult : FIO<char, Error> =
        fio {
            return! awaitWebservice >>=? fun error -> !- (WsError error)
        }

    override _.effect =
        fio {
            return! databaseResult <^> webserviceResult
                    >>=? fun _ -> !+ ("default", 'D')
        }

type ErrorHandlingWithRetryApp() =
    inherit FIOApp<string * char, Error>()

    let readFromDatabase : FIO<string, bool> =
        fio {
            let! rand = !<<< (fun () -> Random().Next(0, 2)) (fun _ -> true)
            if rand = 0 then
                return "data"
            else
                return! !- false
        }

    let awaitWebservice : FIO<char, int> =
        fio {
            let! rand = !<<< (fun () -> Random().Next(0, 2)) (fun _ -> -1)
            if rand = 1 then
                return 'S'
            else
                return! !- 404
        }

    let databaseResult : FIO<string, Error> =
        fio {
            let onRetry retry maxRetries =
                FConsole.PrintLine $"Retrying database read (attempt %d{retry + 1} of %d{maxRetries})..."
                >>=? fun _ -> !- false
            return! readFromDatabase.Retry 100.0 3 onRetry
                    >>=? fun error -> !- (DbError error)
        }

    let webserviceResult : FIO<char, Error> =
        fio {
            let onRetry retry maxRetries =
                FConsole.PrintLine $"Retrying webservice read (attempt %d{retry + 1} of %d{maxRetries})..."
                >>=? fun _ -> !- 400
            return! awaitWebservice.Retry 100.0 3 onRetry
                >>=? fun error -> !- (WsError error)
        }

    override _.effect =
        fio {
            return! databaseResult <^> webserviceResult
                    >>=? fun _ -> !+ ("default", 'D')
        }

type AsyncErrorHandlingApp() =
    inherit FIOApp<string * int, Error>()

    let databaseReadTask : Async<string> =
        async {
            do printfn $"Reading from database..."
            if Random().Next(0, 2) = 0 then
                return "data"
            else 
                raise <| Exception "Database error!"
                return "error data"
        }

    let webserviceAwaitTask : Async<int> =
        async {
            do printfn $"Awaiting webservice..."
            if Random().Next(0, 2) = 0 then
                return 200
            else 
                raise <| Exception "Webservice error!"
                return 400
        }

    let databaseResult : FIO<string, Error> =
        FIO<string, exn>.AwaitAsync databaseReadTask
        >>=? fun exn -> !- (GeneralError exn.Message)

    let webserviceResult : FIO<int, Error> =
        FIO<int, exn>.AwaitAsync webserviceAwaitTask
        >>=? fun exn -> !- (GeneralError exn.Message)

    override _.effect =
        fio {
            return! databaseResult <!> webserviceResult
        }

type HighlyConcurrentApp() =
    inherit FIOApp<unit, exn>()

    let sender (chan: Channel<int>) id (rand: Random) =
        fio {
            let! msg = !+ rand.Next(100, 501)
            do! msg --!> chan
            do! FConsole.PrintLine $"Sender[%i{id}] sent: %i{msg}"
        }

    let rec receiver (chan: Channel<int>) count (max: int) =
        fio {
            if count = 0 then
                let! maxFibers = !+ max.ToString("N0", CultureInfo "en-US")
                do! FConsole.PrintLine $"Successfully received a message from all %s{maxFibers} fibers!"
            else
                let! msg = !<-- chan
                do! FConsole.PrintLine $"Receiver received: %i{msg}"
                return! receiver chan (count - 1) max
        }

    let rec create chan count acc rand =
        fio {
            if count = 0 then
                return! acc
            else
                let newAcc = sender chan count rand <~> acc
                return! create chan (count - 1) newAcc rand
        }

    override _.effect =
        fio {
            let fiberCount = 1000000
            let chan = Channel<int>()
            let rand = Random()
            let acc = sender chan fiberCount rand
                      <~> receiver chan fiberCount fiberCount
            return! create chan (fiberCount - 1) acc rand
        }

type FiberFromTaskApp() =
    inherit FIOApp<unit, exn>()

    let fibonacci n =
        FIO.FromTask<Fiber<string, exn>, exn> <| fun () ->
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
            }

    override _.effect : FIO<unit, exn> =
        let await (fiber: Fiber<unit, exn>) =
            fio {
                do! !!<~~ fiber
                return ()
            }
            
        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! await fiber35 <~>
                await fiber40 <~>
                await fiber45
        }

type FiberFromGenericTaskApp() =
    inherit FIOApp<unit, exn>()

    let fibonacci n =
        FIO.FromGenericTask<Fiber<string, exn>> <| fun () ->
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
            }

    override _.effect : FIO<unit, exn> =
        let awaitAndPrint (fiber: Fiber<string, exn>) =
            fio {
                 let! res = !<~~ fiber
                 do! FConsole.PrintLine $"%s{res}"
            }
            
        fio {
            let! fiber35 = fibonacci 35L
            and! fiber40 = fibonacci 40L
            and! fiber45 = fibonacci 45L

            do! awaitAndPrint fiber35 <~>
                awaitAndPrint fiber40 <~>
                awaitAndPrint fiber45
        }

type SocketApp(ip: string, port: int) =
    inherit FIOApp<unit, exn>()

    let server (ip: string) (port: int) =
    
        let receiveAndSendASCII (socket: FSocket<int, string, exn>) =
            fio {
                while true do
                    let! msg = socket.Receive()
                    do! FConsole.PrintLine $"Server received message: %s{msg}"
                    
                    let! ascii =
                        if msg.Length > 0 then
                            !<< (fun () -> msg.Chars 0) >>= fun c ->
                            !+ (int c)
                        else
                            !+ -1
                    do! socket.Send ascii
                    do! FConsole.PrintLine $"Server sent ASCII: %i{ascii}"
            }

        let handleClient (clientSocket: FSocket<int, string, exn>) =
            fio {
                let! remoteEndPoint = clientSocket.RemoteEndPoint()
                let! endPoint = !<< (fun () -> remoteEndPoint.ToString())
                do! FConsole.PrintLine $"Client connected from %s{endPoint}"
                do! !!~> receiveAndSendASCII(clientSocket)
            }
        
        fio {
            let! listener = !<< (fun () ->
                new TcpListener(IPAddress.Parse ip, port))
            do! !<< (fun () -> listener.Start())
            do! FConsole.PrintLine $"Server listening on %s{ip}:%i{port}..."
            
            while true do
                let! internalSocket = !<< (fun () -> listener.AcceptSocket())
                let! clientSocket = FSocket.Create<int, string, exn> internalSocket
                do! handleClient clientSocket
        }

    let client (ip: string) (port: int) =

        let send (socket: FSocket<string, int, exn>) =
            fio {
                while true do
                    do! FConsole.Print "Enter a message: "
                    let! msg = FConsole.ReadLine ()
                    do! socket.Send msg
                    do! FConsole.Print $"Client sent message: %s{msg}"
            }

        let receive (socket: FSocket<string, int, exn>) =
            fio {
                while true do
                    let! ascii = socket.Receive()
                    do! FConsole.PrintLine $"Client received ASCII: %i{ascii}"
            }
    
        fio {
            do! FConsole.PrintLine $"Connecting to %s{ip}:%i{port}..."
            let! internalSocket = !<< (fun () ->
                new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp))
            let! socket = FSocket<string, int, exn>.Create(internalSocket, ip, port)
            do! FConsole.PrintLine $"Connected to %s{ip}:%i{port}"
            do! send socket <~> receive socket
        }

    override _.effect =
        fio {
            do! client ip port <~> server ip port
        }

type WebSocketApp(serverUrl, clientUrl) =
    inherit FIOApp<unit, exn>()

    let server url =

        let receiveAndSendASCII (clientSocket: FWebSocket<int, string, exn>) =
            fio {
                let! state = clientSocket.State()
                while state = WebSocketState.Open do
                    let! msg = clientSocket.Receive()
                    do! FConsole.PrintLine $"Server received message: %s{msg}"
                    
                    let! ascii =
                        if msg.Length > 0 then
                            !<< (fun () -> msg.Chars 0) >>= fun c ->
                            !+ (int c)
                        else
                            !+ -1
                    do! clientSocket.Send ascii
                    do! FConsole.PrintLine $"Server sent ASCII: %i{ascii}"
            }

        let handleClient (clientSocket: FWebSocket<int, string, exn>) =
            fio {
                let! remoteEndPoint = clientSocket.RemoteEndPoint()
                let! endPoint = !<< (fun () -> remoteEndPoint.ToString())
                do! FConsole.PrintLine $"Client connected from %s{endPoint}"
                do! !!~> receiveAndSendASCII(clientSocket)
            }

        fio {
            let! serverSocket = FServerWebSocket.Create<int, string, exn>()
            do! serverSocket.Start url
            do! FConsole.PrintLine $"Server listening on %s{url}..."
            
            while true do
                let! clientSocket = serverSocket.Accept()
                do! handleClient clientSocket
        }

    let client url =

        let send (clientSocket: FClientWebSocket<string, int, exn>) =
            fio {
                while true do
                    do! FConsole.Print "Enter a message: "
                    let! msg = FConsole.ReadLine ()
                    do! clientSocket.Send msg
                    do! FConsole.Print $"Client sent message: %s{msg}"
            }

        let receive (clientSocket: FClientWebSocket<string, int, exn>) =
            fio {
                while true do
                    let! ascii = clientSocket.Receive()
                    do! FConsole.PrintLine $"Client received ASCII: %i{ascii}"
            }

        fio {
            let! clientSocket = FClientWebSocket.Create<string, int, exn>()
            do! clientSocket.Connect url
            do! send clientSocket <~> receive clientSocket
        }

    override _.effect =
        fio {
            do! client clientUrl <~> server serverUrl
        }
        
WelcomeApp().Run ()
Console.ReadLine () |> ignore

EnterNumberApp().Run ()
Console.ReadLine () |> ignore

TryCatchApp().Run ()
Console.ReadLine () |> ignore

ForApp().Run ()
Console.ReadLine () |> ignore

GuessNumberApp().Run ()
Console.ReadLine () |> ignore

PingPongApp().Run ()
Console.ReadLine () |> ignore

PingPongCEApp().Run ()
Console.ReadLine () |> ignore

PingPongMatchApp().Run ()
Console.ReadLine () |> ignore

ErrorHandlingApp().Run ()
Console.ReadLine () |> ignore

ErrorHandlingWithRetryApp().Run ()
Console.ReadLine () |> ignore

AsyncErrorHandlingApp().Run ()
Console.ReadLine () |> ignore

HighlyConcurrentApp().Run ()
Console.ReadLine () |> ignore

FiberFromTaskApp().Run ()
Console.ReadLine () |> ignore

FiberFromGenericTaskApp().Run ()
Console.ReadLine () |> ignore

SocketApp("127.0.0.1", 5000).Run ()
Console.ReadLine () |> ignore

WebSocketApp("http://localhost:8080/", "ws://localhost:8080/").Run ()
Console.ReadLine () |> ignore
