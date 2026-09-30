namespace FIO.WebSockets

open FIO.DSL

open System
open System.Threading

[<RequireQualifiedAccess>]
module WebSocketClient =

    let private logAndSuppress (context: string) (error: WsError) =
        fio {
            let str = error.ToString()

            do! FIO.attempt
                    (fun () -> eprintfn $"WebSocketClient encountered error during {context}: {str}")
                    WsError.fromException

            return ()
        }

    /// Connects to a WebSocket server at the given URI using the given configuration and cancellation token.
    let connect (uri: Uri) (config: WebSocketConfig) (cancellationToken: CancellationToken) =
        fio {
            let! clientSocket =
                FIO.attempt
                    (fun () -> new Net.WebSockets.ClientWebSocket())
                    WsError.connectionFailed

            let establish =
                fio {
                    let! connectTask =
                        FIO.attempt
                            (fun () -> clientSocket.ConnectAsync(uri, cancellationToken))
                            WsError.connectionFailed
                    do! FIO.awaitUnitTask connectTask WsError.connectionFailed
                    return new WebSocket(clientSocket, config, None, None)
                }

            return! establish.CatchAll(fun error ->
                fio {
                    do! (FIO.attempt (fun () -> clientSocket.Dispose()) WsError.fromException)
                            .CatchAll(logAndSuppress "client socket disposal")
                    return! FIO.fail error
                })
        }

    /// Connects to the given URI using default configuration and the fiber's cancellation token.
    let connectWith (uri: Uri) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! connect uri WebSocketConfig.defaultConfig cancellationToken
        }

    /// Connects to the given URL string using the given configuration and cancellation token.
    let connectString (url: string) (config: WebSocketConfig) (cancellationToken: CancellationToken) =
        fio {
            let! uri = FIO.attempt (fun () -> Uri url) WsError.connectionFailed
            return! connect uri config cancellationToken
        }

    /// Connects to the given URL string using default configuration and the fiber's cancellation token.
    let connectStringWith (url: string) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! connectString url WebSocketConfig.defaultConfig cancellationToken
        }

    /// Connects to the given URL string using default configuration. Alias for connectStringWith.
    let connectDefault (url: string) =
        connectStringWith url

    /// Connects, runs an action with the open connection, then closes it.
    let withConnection<'A> (uri: Uri) (config: WebSocketConfig) (action: WebSocket -> FIO<'A, WsError>) =
        // Acquire runs uninterruptibly, so it only creates the socket; the connect happens in use,
        // where an interruption still cancels it, and release also covers a socket that never connected.
        let acquire =
            FIO.attempt
                (fun () ->
                    let clientSocket = new Net.WebSockets.ClientWebSocket()
                    clientSocket, new WebSocket(clientSocket, config, None, None))
                WsError.connectionFailed

        let release (_: Net.WebSockets.ClientWebSocket, ws: WebSocket) =
            (ws.CloseIfOpen())
                .Ensuring(
                    (FIO.attempt (fun () -> (ws :> IDisposable).Dispose()) WsError.fromException)
                        .CatchAll(logAndSuppress "websocket disposal"))

        let connectThenAct (clientSocket: Net.WebSockets.ClientWebSocket, ws: WebSocket) =
            fio {
                let! cancellationToken = FIO.cancellationToken ()
                let! connectTask =
                    FIO.attempt
                        (fun () -> clientSocket.ConnectAsync(uri, cancellationToken))
                        WsError.connectionFailed
                do! FIO.awaitUnitTask connectTask WsError.connectionFailed
                return! action ws
            }

        FIO.acquireReleaseWith acquire release connectThenAct

    /// Connects to the given URL string, runs an action with the open connection, then closes it.
    let withConnectionString<'A> (url: string) (action: WebSocket -> FIO<'A, WsError>) =
        fio {
            let! uri = FIO.attempt (fun () -> Uri url) WsError.connectionFailed
            return! withConnection uri WebSocketConfig.defaultConfig action
        }
