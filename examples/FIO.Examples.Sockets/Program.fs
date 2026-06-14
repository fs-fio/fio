module private FIO.Examples.Sockets

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.Sockets

type SocketApp(host, port) =
    inherit FIOApp<unit, SocketError>()

    let handleClient (socket: Socket) =
        fio {
            let! endpoint = socket.GetRemoteEndPoint()
            do! Console.printLine $"Client connected: {endpoint}" SocketError.fromException

            let rec loop () =
                fio {
                    let! message = socket.ReceiveString 1024

                    if message = "quit" then
                        do! Console.printLine "Client disconnected" SocketError.fromException
                        do! socket.Close()
                    else
                        do! Console.printLine $"Received: {message}" SocketError.fromException
                        let response = $"Echo: {message}"
                        do! socket.SendString response
                        do! Console.printLine $"Sent: {response}" SocketError.fromException
                        return! loop ()
                }

            do! loop ()
        }

    let server (readyChannel: Channel<unit>) =
        fio {
            let! config = ServerSocketConfig.create host port
            let! serverSocket = ServerSocket.bind config
            do! Console.printLine $"Server listening on {host}:{port}" SocketError.fromException
            do! readyChannel.Write(()).Unit()
            let! clientSocket = ServerSocket.accept serverSocket
            do! handleClient clientSocket
            do! ServerSocket.close serverSocket
        }

    let client (readyChannel: Channel<unit>) =
        fio {
            do! readyChannel.Read().Unit()
            let! config = SocketConfig.create host port

            do! SocketClient.withConnection config <| fun socket ->
                fio {
                    do! Console.printLine $"Connected to {host}:{port}" SocketError.fromException

                    let rec loop () =
                        fio {
                            do! Console.print $"Enter message (or 'quit'): " SocketError.fromException
                            let! input = Console.readLine SocketError.fromException
                            do! socket.SendString input

                            if input = "quit" then
                                do! Console.printLine "Disconnecting..." SocketError.fromException
                                do! socket.Close()
                            else
                                let! response = socket.ReceiveString 1024
                                do! Console.printLine $"Server: {response}" SocketError.fromException
                                return! loop ()
                        }

                    do! loop ()
                }
        }

    override _.effect =
        fio {
            let readyChannel = Channel<unit>()
            do! server readyChannel <&&> client readyChannel
        }

[<EntryPoint>]
let main _ =
    let host = "127.0.0.1"
    let port = 5000
    SocketApp(host, port).Run()
