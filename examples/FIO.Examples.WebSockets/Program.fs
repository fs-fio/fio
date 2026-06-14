module private FIO.Examples.WebSockets

open FIO.DSL
open FIO.App
open FIO.Console
open FIO.WebSockets

open System.Net

type WebSocketApp(host, port) =
    inherit FIOApp<unit, WsError>()

    let server (readyChannel: Channel<unit>) =

        let handleClient (webSocket: WebSocket) (listener: HttpListener) =
            fio {
                do! Console.printLine "Client connected" WsError.fromException

                let rec loop () =
                    fio {
                        match! webSocket.ReceiveMessage() with
                        | Frame(Text text) ->
                            if text = "quit" then
                                do! Console.printLine "Client disconnected" WsError.fromException
                                do! webSocket.Close()
                                do! WebSocketServer.close listener
                            else
                                do! Console.printLine ($"Received: {text}") WsError.fromException
                                let response = $"Echo: {text}"
                                do! webSocket.SendText response
                                do! Console.printLine ($"Sent: {response}") WsError.fromException
                                return! loop ()
                        | ConnectionClosed(status, desc) ->
                            do! Console.printLine ($"Connection closed: {status} - {desc}") WsError.fromException
                            do! webSocket.Close()
                            do! WebSocketServer.close listener
                        | _ -> return! loop ()
                    }

                do! loop ()
            }

        fio {
            let url = $"http://{host}:{port}/"
            let! listener = WebSocketServer.start url
            do! Console.printLine ($"Server listening on ws://{host}:{port}/") WsError.fromException
            do! readyChannel.Write(()).Unit()
            let! webSocket = WebSocketServer.acceptDefault listener WebSocketConfig.defaultConfig
            do! handleClient webSocket listener
        }

    let client (readyChannel: Channel<unit>) =
        fio {
            do! readyChannel.Read().Unit()
            let url = $"ws://{host}:{port}/"
            let! webSocket = WebSocketClient.connectDefault url
            do! Console.printLine ($"Connected to {url}") WsError.fromException

            let rec loop () =
                fio {
                    do! Console.print "Enter message (or 'quit'): " WsError.fromException
                    let! input = Console.readLine WsError.fromException
                    do! webSocket.SendText input

                    if input = "quit" then
                        do! Console.printLine "Disconnecting..." WsError.fromException
                        do! webSocket.Close()
                    else
                        match! webSocket.ReceiveMessage() with
                        | Frame(Text text) ->
                            do! Console.printLine ($"Server: {text}") WsError.fromException
                            return! loop ()
                        | ConnectionClosed(status, desc) ->
                            do! Console.printLine ($"Connection closed: {status} - {desc}") WsError.fromException
                        | _ -> return! loop ()
                }

            do! loop ()
        }

    override _.effect =
        fio {
            let readyChannel = Channel<unit>()
            do! server readyChannel <&&> client readyChannel
        }

[<EntryPoint>]
let main _ =
    let host = "localhost"
    let port = 8080
    WebSocketApp(host, port).Run()
