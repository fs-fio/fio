namespace FIO.WebSockets

open FIO.DSL

open System
open System.Threading

/// <summary>Builds effects for establishing outgoing WebSocket connections.</summary>
[<RequireQualifiedAccess>]
module WebSocketClient =

    /// <summary>Transforms a WebSocket error into a logged-and-suppressed unit effect for use in client cleanup paths.</summary>
    /// <param name="context">A description of the operation being cleaned up.</param>
    /// <param name="error">The WebSocket error to log.</param>
    /// <returns>An effect that logs the error to standard error and succeeds with unit.</returns>
    let private logAndSuppress (context: string) (error: WsError) =
        fio {
            let str = error.ToString()

            do!
                FIO.attempt
                    (fun () -> eprintfn $"[WebSocketClient] Error during {context}: {str}")
                    WsError.fromException

            return ()
        }

    /// <summary>Creates an effect that connects to a WebSocket server at the specified URI.</summary>
    /// <param name="uri">The URI of the WebSocket server to connect to.</param>
    /// <param name="config">The configuration options for the connection.</param>
    /// <param name="ct">The cancellation token to observe during the connection attempt.</param>
    /// <returns>An effect that produces a connected <c>WebSocket</c>.</returns>
    let connect (uri: Uri) (config: WebSocketConfig) (ct: CancellationToken) =
        fio {
            let! clientSocket =
                FIO.attempt
                    (fun () -> new Net.WebSockets.ClientWebSocket())
                    WsError.fromException

            let establish =
                fio {
                    let! connectTask =
                        FIO.attempt
                            (fun () -> clientSocket.ConnectAsync(uri, ct))
                            WsError.fromException

                    do! FIO.awaitUnitTask connectTask WsError.fromException
                    return new WebSocket(clientSocket, config)
                }

            return!
                establish.CatchAll(fun error ->
                    fio {
                        do!
                            (FIO.attempt (fun () -> clientSocket.Dispose()) WsError.fromException)
                                .CatchAll(logAndSuppress "client socket disposal")

                        return! FIO.fail error
                    })
        }

    /// <summary>Creates an effect that connects to a WebSocket server at the specified URI with default configuration, observing the running fiber's cancellation token.</summary>
    /// <param name="uri">The URI of the WebSocket server to connect to.</param>
    /// <returns>An effect that produces a connected <c>WebSocket</c>.</returns>
    let connectWith (uri: Uri) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! connect uri WebSocketConfig.defaultConfig ct
        }

    /// <summary>Creates an effect that connects to a WebSocket server from a URL string.</summary>
    /// <param name="url">The URL string of the WebSocket server to connect to.</param>
    /// <param name="config">The configuration options for the connection.</param>
    /// <param name="ct">The cancellation token to observe during the connection attempt.</param>
    /// <returns>An effect that produces a connected <c>WebSocket</c>.</returns>
    let connectString (url: string) (config: WebSocketConfig) (ct: CancellationToken) =
        fio {
            let! uri = FIO.attempt (fun () -> Uri url) WsError.fromException
            return! connect uri config ct
        }

    /// <summary>Creates an effect that connects to a WebSocket server from a URL string with default configuration, observing the running fiber's cancellation token.</summary>
    /// <param name="url">The URL string of the WebSocket server to connect to.</param>
    /// <returns>An effect that produces a connected <c>WebSocket</c>.</returns>
    let connectStringWith (url: string) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! connectString url WebSocketConfig.defaultConfig ct
        }

    /// <summary>Creates an effect that connects to a WebSocket server from a URL string with default configuration, observing the running fiber's cancellation token.</summary>
    /// <param name="url">The URL string of the WebSocket server to connect to.</param>
    /// <returns>An effect that produces a connected <c>WebSocket</c>.</returns>
    let connectDefault (url: string) = connectStringWith url

    /// <summary>Builds a resource-managed effect that connects, runs an action, and closes the connection on every outcome.</summary>
    /// <typeparam name="R">The result type produced by the action.</typeparam>
    /// <param name="uri">The URI of the WebSocket server to connect to.</param>
    /// <param name="config">The configuration options for the connection.</param>
    /// <param name="action">A function from the connected WebSocket to the effect to run.</param>
    /// <returns>An effect that produces the action's result and always closes the connection afterwards.</returns>
    /// <remarks>The connection step observes the running fiber's cancellation token, so interrupting the fiber aborts the in-flight handshake.</remarks>
    let withConnection<'A> (uri: Uri) (config: WebSocketConfig) (action: WebSocket -> FIO<'A, WsError>) =
        let acquire =
            fio {
                let! ct = FIO.cancellationToken ()
                return! connect uri config ct
            }

        FIO.acquireReleaseWith
            acquire
            (fun ws ->
                ws.Close(Net.WebSockets.WebSocketCloseStatus.NormalClosure, "Closing connection")
                    .CatchAll(logAndSuppress "websocket close"))
            action

    /// <summary>Builds a resource-managed effect that connects to a URL string, runs an action, and closes the connection on every outcome.</summary>
    /// <typeparam name="R">The result type produced by the action.</typeparam>
    /// <param name="url">The URL string of the WebSocket server to connect to.</param>
    /// <param name="action">A function from the connected WebSocket to the effect to run.</param>
    /// <returns>An effect that produces the action's result and always closes the connection afterwards.</returns>
    let withConnectionString<'A> (url: string) (action: WebSocket -> FIO<'A, WsError>) =
        fio {
            let! uri = FIO.attempt (fun () -> Uri url) WsError.fromException
            return! withConnection uri WebSocketConfig.defaultConfig action
        }
