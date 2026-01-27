/// <summary>
/// FSharp.FIO.WebSockets example demonstrating WebSocket client/server communication.
/// </summary>
module private FSharp.FIO.Examples.WebSockets

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.Console
open FSharp.FIO.WebSockets

open System.Net

/// <summary>
/// WebSocket demo app with echo server and interactive client.
/// </summary>
/// <param name="host">Server hostname or IP address.</param>
/// <param name="port">Server port number.</param>
type private WebSocketApp(host, port) =
    inherit FIOApp<unit, WsError>()

    let server () =
    
        let handleClient (ws: WebSocket) (listener: HttpListener) = fio {
            do! Console.printLine("Client connected", WsError.fromException)

            let rec loop () = fio {
                match! ws.ReceiveMessage() with
                | Frame(Text text) ->
                    if text = "quit" then
                        do! Console.printLine("Client disconnected", WsError.fromException)
                        do! ws.Close()
                        do! WebSocketServer.close listener
                    else
                        do! Console.printLine($"Received: {text}", WsError.fromException)
                        let response = $"Echo: {text}"
                        do! ws.SendText response
                        do! Console.printLine($"Sent: {response}", WsError.fromException)
                        return! loop ()
                | ConnectionClosed(status, desc) ->
                    do! Console.printLine($"Connection closed: {status} - {desc}", WsError.fromException)
                    do! ws.Close()
                    do! WebSocketServer.close listener
                | _ -> return! loop ()
            }
            do! loop ()
        }

        fio {
            let url = $"http://{host}:{port}/"
            let! listener = WebSocketServer.start url
            do! Console.printLine($"Server listening on ws://{host}:{port}/", WsError.fromException)
            let! ws = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
            do! handleClient ws listener
        }

    let client () = fio {
        let url = $"ws://{host}:{port}/"
        let! ws = WebSocketClient.connectDefault url
        do! Console.printLine($"Connected to {url}", WsError.fromException)

        let rec loop () = fio {
            do! Console.print("Enter message (or 'quit'): ", WsError.fromException)
            let! input = Console.readLine WsError.fromException
            do! ws.SendText input
            if input = "quit" then
                do! Console.printLine("Disconnecting...", WsError.fromException)
                do! ws.Close()
            else
                match! ws.ReceiveMessage() with
                | Frame(Text text) ->
                    do! Console.printLine($"Server: {text}", WsError.fromException)
                    return! loop ()
                | ConnectionClosed(status, desc) ->
                    do! Console.printLine($"Connection closed: {status} - {desc}", WsError.fromException)
                | _ -> return! loop ()
        }
        do! loop ()
    }


    override _.effect =
        server () <&&> client ()

[<EntryPoint>]
let main _ =
    let host = "localhost"
    let port = 8080
    WebSocketApp(host, port).Run()
