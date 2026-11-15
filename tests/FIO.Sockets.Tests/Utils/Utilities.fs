module FIO.Sockets.Tests.Utilities

open FIO.Tests.Utilities

open FIO.DSL
open FIO.Runtime
open FIO.Sockets

open System
open System.Net
open System.Diagnostics

let noopHandler (_socket: Socket) = FIO.unit ()

let echoHandler (socket: Socket) =
    fio {
        let! data, _ = socket.ReceiveBytes 8192
        do! socket.SendBytes data
    }

let runWithTimeout (runtime: FIORuntime) (effect: FIO<'A, SocketError>) = runWithin 10.0 runtime effect

// The port freePort returns can be taken before the server binds it, so a server whose bind failed is started again on a fresh port.
let serveAndConnect (runtime: FIORuntime) (serveOn: int -> FIO<unit, SocketError>) =
    let rec start attemptsLeft =
        let port = freePort ()
        let server = runtime.Run(serveOn port)
        let deadline = Stopwatch.StartNew()
        let mutable client = new Sockets.TcpClient()

        while not client.Connected
              && not (server.IsTerminal())
              && deadline.ElapsedMilliseconds < 5_000L do
            try
                client.ConnectAsync(IPAddress.Loopback, port).Wait(500) |> ignore
            with _ ->
                ()

            if not client.Connected then
                client.Dispose()
                client <- new Sockets.TcpClient()
                Threading.Thread.Sleep 20

        let lostPort =
            not client.Connected
            && server.IsTerminal()
            && (match server.Task().Result with
                | Failed(BindFailed _) -> true
                | _ -> false)

        if lostPort && attemptsLeft > 1 then
            client.Dispose()
            start (attemptsLeft - 1)
        else
            server, port, client

    start 5

let connectWhenListening (host: string) (port: int) =
    (SocketClient.connectWith host port).Retry 60 (fun (_, _, _) -> FIO.sleep (TimeSpan.FromMilliseconds 50.0))

let withTestServer
    (handler: Socket -> FIO<unit, SocketError>)
    (action: int -> FIO<'A, SocketError>)
    (runtime: FIORuntime)
    =
    let effect =
        fio {
            let! config = ServerSocketConfig.create "127.0.0.1" 0
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port

            let! serverFiber =
                (fio {
                    let! socket = ServerSocket.accept server
                    do! handler socket
                    do! socket.Close()
                })
                    .Fork()

            let! result = action port
            do! serverFiber.InterruptNow()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect

let withTestEchoServer (action: int -> FIO<'A, SocketError>) (runtime: FIORuntime) =
    let effect =
        fio {
            let! config = ServerSocketConfig.create "127.0.0.1" 0
            let! server = ServerSocket.bind config
            let! ep = ServerSocket.getLocalEndPoint server
            let port = (ep :?> IPEndPoint).Port
            let! serverFiber = (ServerSocket.acceptLoop echoHandler server).Fork()
            do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
            let! result = action port
            do! serverFiber.InterruptNow()
            do! ServerSocket.close server
            return result
        }

    runWithTimeout runtime effect
