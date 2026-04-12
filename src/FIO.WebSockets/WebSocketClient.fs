namespace FIO.WebSockets

open FIO.DSL

open System
open System.Threading

/// <summary>
/// Client operations for establishing WebSocket connections.
/// </summary>
[<RequireQualifiedAccess>]
module WebSocketClient =

    /// <summary>
    /// Internal: Logs an error and suppresses it.
    /// Used for cleanup operations where errors should not propagate.
    /// </summary>
    let private logAndSuppress (context: string) (err: WsError) =
        fio {
            let str = err.ToString()
            do! FIO.attempt((fun () ->
                eprintfn $"[WebSocketClient] Error during {context}: {str}"), WsError.fromException)
            return ()
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URI.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    /// <returns>The connected WebSocket.</returns>
    let connect (uri: Uri) (config: WebSocketConfig) (ct: CancellationToken) =
        fio {
            let! clientSocket = FIO.attempt((fun () -> new Net.WebSockets.ClientWebSocket()), WsError.fromException)
            let! connectTask = FIO.attempt((fun () -> clientSocket.ConnectAsync(uri, ct)), WsError.fromException)
            do! FIO.awaitTask(connectTask, WsError.fromException)
            return new WebSocket(clientSocket, config)
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URI with default config.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <returns>The connected WebSocket.</returns>
    let connectWith (uri: Uri) =
        connect uri WebSocketConfig.defaultConfig CancellationToken.None

    /// <summary>
    /// Connects to a WebSocket server from a URL string.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    /// <returns>The connected WebSocket.</returns>
    let connectString (url: string) (config: WebSocketConfig) (ct: CancellationToken) =
        fio {
            let! uri = FIO.attempt((fun () -> Uri url), WsError.fromException)
            return! connect uri config ct
        }

    /// <summary>
    /// Connects to a WebSocket server from a URL string with default config.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <returns>The connected WebSocket.</returns>
    let connectStringWith (url: string) =
        connectString url WebSocketConfig.defaultConfig CancellationToken.None

    /// <summary>
    /// Connects with default error handling.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <returns>The connected WebSocket.</returns>
    let connectDefault (url: string) =
        connectStringWith url

    /// <summary>
    /// Executes an action with a WebSocket connection using AcquireRelease pattern.
    /// </summary>
    /// <typeparam name="R">The result type of the action.</typeparam>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="action">The action to execute with the connection.</param>
    /// <returns>The result of the action.</returns>
    let withConnection<'R> (uri: Uri) (config: WebSocketConfig) (action: WebSocket -> FIO<'R, WsError>) =
        FIO.acquireRelease(
            connect uri config CancellationToken.None,
            (fun ws -> ws.Close(Net.WebSockets.WebSocketCloseStatus.NormalClosure, "Closing connection").CatchAll(logAndSuppress "websocket close")),
            action
        )

    /// <summary>
    /// Executes an action with a WebSocket connection (default config).
    /// </summary>
    /// <typeparam name="R">The result type of the action.</typeparam>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="action">The action to execute with the connection.</param>
    /// <returns>The result of the action.</returns>
    let withConnectionString<'R> (url: string) (action: WebSocket -> FIO<'R, WsError>) =
        fio {
            let! uri = FIO.attempt((fun () -> Uri url), WsError.fromException)
            return! withConnection uri WebSocketConfig.defaultConfig action
        }
