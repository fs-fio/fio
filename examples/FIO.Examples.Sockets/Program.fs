/// <summary>
/// FIO.Sockets example demonstrating TCP socket client/server communication.
/// </summary>
module private FIO.Examples.Sockets

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.Sockets

/// <summary>
/// TCP socket demo app with echo server and interactive client.
/// </summary>
/// <param name="host">Server hostname or IP address.</param>
/// <param name="port">Server port number.</param>
type private SocketApp(host, port) =
    inherit FIOApp<unit, SocketError>()

    let server () =
    
        let handleClient (socket: Socket) = fio {
            let! endpoint = socket.GetRemoteEndPoint()
            do! Console.printLine($"Client connected: {endpoint}", SocketError.fromException)

            let rec loop () = fio {
                let! msg = socket.ReceiveString 1024
                if msg = "quit" then
                    do! Console.printLine("Client disconnected", SocketError.fromException)
                    do! socket.Close()
                else
                    do! Console.printLine($"Received: {msg}", SocketError.fromException)
                    let response = $"Echo: {msg}"
                    do! socket.SendString response
                    do! Console.printLine($"Sent: {response}", SocketError.fromException)
                    return! loop ()
            }
            do! loop ()
        }

        fio {
            let! config = ServerSocketConfig.create(host, port)
            do! Console.printLine($"Server listening on {host}:{port}", SocketError.fromException)
            do! ServerSocket.serve(config, handleClient)
        }

    let client () = fio {
        let! config = SocketConfig.create(host, port)
        do! SocketClient.withConnection config (fun socket -> fio {
            do! Console.printLine($"Connected to {host}:{port}", SocketError.fromException)

            let rec loop () = fio {
                do! Console.print("Enter message (or 'quit'): ", SocketError.fromException)
                let! input = Console.readLine SocketError.fromException
                do! socket.SendString input
                if input = "quit" then
                    do! Console.printLine("Disconnecting...", SocketError.fromException)
                    do! socket.Close()
                else
                    let! response = socket.ReceiveString 1024
                    do! Console.printLine($"Server: {response}", SocketError.fromException)
                    return! loop ()
            }
            do! loop ()
        })
    }

    override _.effect =
        server () <&&> client ()

[<EntryPoint>]
let main _ =
    let host = "127.0.0.1"
    let port = 5000
    SocketApp(host, port).Run()
