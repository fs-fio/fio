/// <summary>Provides a WebSocket example demonstrating an echo server and interactive client using FIO.WebSockets for frame-based bidirectional communication.</summary>
module private FIO.Examples.WebSockets

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.WebSockets

open System.Net

/// <summary>Represents a WebSocket application that runs an echo server and an interactive client concurrently using the <c>&lt;&amp;&amp;&gt;</c> parallel operator.</summary>
/// <remarks>The server pattern-matches on <c>Frame(Text _)</c> and <c>ConnectionClosed</c> to handle messages and graceful shutdown. The client reads user input and sends text frames, demonstrating the FIO.WebSockets send/receive cycle.</remarks>
type private WebSocketApp(host, port) =
    inherit FIOApp<unit, WsError>()

    /// <summary>Builds an echo server effect that listens for WebSocket connections, receives text frames, and sends them back prefixed with "Echo: ".</summary>
    /// <returns>An effect that starts an HTTP listener, accepts a WebSocket upgrade, and loops over incoming frames until the client sends "quit" or the connection closes.</returns>
    /// <remarks>Demonstrates pattern matching on <c>Frame(Text _)</c> for message handling and <c>ConnectionClosed</c> for graceful shutdown, including closing both the WebSocket and the underlying HTTP listener.</remarks>
    let server () =

        let handleClient (ws: WebSocket) (listener: HttpListener) =
            fio {
                do! Console.printLine "Client connected" WsError.fromException

                let rec loop () =
                    fio {
                        match! ws.ReceiveMessage() with
                        | Frame(Text text) ->
                            if text = "quit" then
                                do! Console.printLine "Client disconnected" WsError.fromException
                                do! ws.Close()
                                do! WebSocketServer.close listener
                            else
                                do! Console.printLine ($"Received: {text}") WsError.fromException
                                let response = $"Echo: {text}"
                                do! ws.SendText response
                                do! Console.printLine ($"Sent: {response}") WsError.fromException
                                return! loop ()
                        | ConnectionClosed(status, desc) ->
                            do! Console.printLine ($"Connection closed: {status} - {desc}") WsError.fromException
                            do! ws.Close()
                            do! WebSocketServer.close listener
                        | _ -> return! loop ()
                    }

                do! loop ()
            }

        fio {
            let url = $"http://{host}:{port}/"
            let! listener = WebSocketServer.start url
            do! Console.printLine ($"Server listening on ws://{host}:{port}/") WsError.fromException
            let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
            do! handleClient ws listener
        }

    /// <summary>Builds an interactive client effect that connects to the WebSocket server, sends user input as text frames, and displays the server's echoed responses.</summary>
    /// <returns>An effect that opens a WebSocket connection and enters a send/receive loop until the user types "quit" or the server closes the connection.</returns>
    /// <remarks>Uses <c>WebSocketClient.connectDefault</c> for the initial handshake and pattern-matches received frames to distinguish text replies from connection-closed notifications.</remarks>
    let client () =
        fio {
            let url = $"ws://{host}:{port}/"
            let! ws = WebSocketClient.connectDefault url
            do! Console.printLine ($"Connected to {url}") WsError.fromException

            let rec loop () =
                fio {
                    do! Console.print "Enter message (or 'quit'): " WsError.fromException
                    let! input = Console.readLine WsError.fromException
                    do! ws.SendText input

                    if input = "quit" then
                        do! Console.printLine "Disconnecting..." WsError.fromException
                        do! ws.Close()
                    else
                        match! ws.ReceiveMessage() with
                        | Frame(Text text) ->
                            do! Console.printLine ($"Server: {text}") WsError.fromException
                            return! loop ()
                        | ConnectionClosed(status, desc) ->
                            do! Console.printLine ($"Connection closed: {status} - {desc}") WsError.fromException
                        | _ -> return! loop ()
                }

            do! loop ()
        }

    override _.effect = server () <&&> client ()

[<EntryPoint>]
let main _ =
    let host = "localhost"
    let port = 8080
    WebSocketApp(host, port).Run()
