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
    let connect (uri: Uri) (config: WebSocketConfig) (cancelToken: CancellationToken) =
        fio {
            let! clientSocket =
                FIO.attempt
                    (fun () -> new Net.WebSockets.ClientWebSocket())
                    WsError.fromException

            let establish =
                fio {
                    let! connectTask =
                        FIO.attempt
                            (fun () -> clientSocket.ConnectAsync(uri, cancelToken))
                            WsError.fromException
                    do! FIO.awaitUnitTask connectTask WsError.fromException
                    return new WebSocket(clientSocket, config)
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
            let! cancelToken = FIO.cancellationToken ()
            return! connect uri WebSocketConfig.defaultConfig cancelToken
        }

    /// Connects to the given URL string using the given configuration and cancellation token.
    let connectString (url: string) (config: WebSocketConfig) (cancelToken: CancellationToken) =
        fio {
            let! uri = FIO.attempt (fun () -> Uri url) WsError.fromException
            return! connect uri config cancelToken
        }

    /// Connects to the given URL string using default configuration and the fiber's cancellation token.
    let connectStringWith (url: string) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! connectString url WebSocketConfig.defaultConfig cancelToken
        }

    /// Connects to the given URL string using default configuration. Alias for connectStringWith.
    let connectDefault (url: string) =
        connectStringWith url

    /// Connects, runs an action with the open connection, then closes it.
    let withConnection<'A> (uri: Uri) (config: WebSocketConfig) (action: WebSocket -> FIO<'A, WsError>) =
        let acquire =
            fio {
                let! cancelToken = FIO.cancellationToken ()
                return! connect uri config cancelToken
            }

        FIO.acquireReleaseWith acquire
            (fun ws -> ws.Close(Net.WebSockets.WebSocketCloseStatus.NormalClosure, "Closing connection")
                        .CatchAll(logAndSuppress "websocket close"))
            action

    /// Connects to the given URL string, runs an action with the open connection, then closes it.
    let withConnectionString<'A> (url: string) (action: WebSocket -> FIO<'A, WsError>) =
        fio {
            let! uri = FIO.attempt (fun () -> Uri url) WsError.fromException
            return! withConnection uri WebSocketConfig.defaultConfig action
        }
