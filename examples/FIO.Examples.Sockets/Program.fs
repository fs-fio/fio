/// <summary>Provides a TCP socket example demonstrating an echo server and interactive client using FIO.Sockets for bidirectional communication.</summary>
module private FIO.Examples.Sockets

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.Sockets

/// <summary>Represents a TCP socket application that runs an echo server and an interactive client concurrently using the <c>&lt;&amp;&amp;&gt;</c> parallel operator.</summary>
/// <remarks>The server accepts connections and echoes received messages; the client reads user input from the console and sends it to the server. Typing "quit" gracefully disconnects both sides.</remarks>
type private SocketApp(host, port) =
    inherit FIOApp<unit, SocketError>()

    /// <summary>Builds an echo server effect that listens for TCP connections, reads string messages from each client, and sends them back prefixed with "Echo: ".</summary>
    /// <returns>An effect that binds to the configured host and port and loops over incoming messages until the client sends "quit".</returns>
    /// <remarks>Uses <c>ServerSocket.serve</c> with a <c>ServerSocketConfig</c> to accept connections, demonstrating FIO.Sockets connection handling and recursive message loops.</remarks>
    let server () =

        let handleClient (socket: Socket) =
            fio {
                let! endpoint = socket.GetRemoteEndPoint()
                do! Console.printLine $"Client connected: {endpoint}" SocketError.fromException

                let rec loop () =
                    fio {
                        let! msg = socket.ReceiveString 1024

                        if msg = "quit" then
                            do! Console.printLine "Client disconnected" SocketError.fromException
                            do! socket.Close()
                        else
                            do! Console.printLine $"Received: {msg}" SocketError.fromException
                            let response = $"Echo: {msg}"
                            do! socket.SendString response
                            do! Console.printLine $"Sent: {response}" SocketError.fromException
                            return! loop ()
                    }

                do! loop ()
            }

        fio {
            let! config = ServerSocketConfig.create (host, port)
            do! Console.printLine $"Server listening on {host}:{port}" SocketError.fromException
            do! ServerSocket.serve (config, handleClient)
        }

    /// <summary>Builds an interactive client effect that connects to the server, reads user input from the console, and displays the server's echoed responses.</summary>
    /// <returns>An effect that establishes a TCP connection and enters a send/receive loop until the user types "quit".</returns>
    /// <remarks>Uses <c>SocketClient.withConnection</c> to manage the connection lifecycle, ensuring the socket is properly closed when the loop terminates.</remarks>
    let client () =
        fio {
            let! config = SocketConfig.create (host, port)

            do!
                SocketClient.withConnection config (fun socket ->
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
                    })
        }

    override _.effect = server () <&&> client ()

[<EntryPoint>]
let main _ =
    let host = "127.0.0.1"
    let port = 5000
    SocketApp(host, port).Run()
