namespace FSharp.FIO.WebSockets

open FSharp.FIO.DSL

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
    let private logAndSuppress (context: string) (err: WsError) : FIO<unit, WsError> =
        fio {
            let str = WsError.ToString err
            do! FIO.attempt((fun () ->
                eprintfn $"[WebSocketClient] Error during {context}: {str}"), WsError.FromException)
            return ()
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URI.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    let connect
        (uri: Uri)
        (config: WebSocketConfig)
        (cancellationToken: CancellationToken)
        : FIO<WebSocket, WsError> =
        fio {
            let! clientSocket = FIO.attempt((fun () -> new Net.WebSockets.ClientWebSocket()), WsError.FromException)
            let! connectTask = FIO.attempt((fun () -> clientSocket.ConnectAsync(uri, cancellationToken)), WsError.FromException)
            do! FIO.awaitTask(connectTask, WsError.FromException)
            return new WebSocket(clientSocket, config)
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URI with default config.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    let connectWith (uri: Uri) : FIO<WebSocket, WsError> =
        connect uri WebSocketConfig.Default CancellationToken.None

    /// <summary>
    /// Connects to a WebSocket server from a URL string.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    let connectString
        (url: string)
        (config: WebSocketConfig)
        (cancellationToken: CancellationToken)
        : FIO<WebSocket, WsError> =
        fio {
            let! uri = FIO.attempt((fun () -> Uri url), WsError.FromException)
            return! connect uri config cancellationToken
        }

    /// <summary>
    /// Connects to a WebSocket server from a URL string with default config.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    let connectStringWith (url: string) : FIO<WebSocket, WsError> =
        connectString url WebSocketConfig.Default CancellationToken.None

    /// <summary>
    /// Connects with default error handling.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    let connectDefault (url: string) : FIO<WebSocket, WsError> =
        connectStringWith url

    /// <summary>
    /// Executes an action with a WebSocket connection using AcquireRelease pattern.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="config">WebSocket configuration options.</param>
    /// <param name="action">The action to execute with the connection.</param>
    let withConnection<'R>
        (uri: Uri)
        (config: WebSocketConfig)
        (action: WebSocket -> FIO<'R, WsError>)
        : FIO<'R, WsError> =
        FIO.acquireRelease(
            connect uri config CancellationToken.None,
            (fun ws -> ws.Close(Net.WebSockets.WebSocketCloseStatus.NormalClosure, "Closing connection").CatchAll(logAndSuppress "websocket close")),
            action
        )

    /// <summary>
    /// Executes an action with a WebSocket connection (default config).
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="action">The action to execute with the connection.</param>
    let withConnectionString<'R>
        (url: string)
        (action: WebSocket -> FIO<'R, WsError>)
        : FIO<'R, WsError> =
        fio {
            let! uri = FIO.attempt((fun () -> Uri url), WsError.FromException)
            return! withConnection uri WebSocketConfig.Default action
        }

/// <summary>
/// Backward compatibility: WebSocketClient type matching original API.
/// </summary>
type WebSocketClient private (clientSocket: Net.WebSockets.ClientWebSocket, config: WebSocketConfig) =

    /// <summary>
    /// Creates a new WebSocket client with custom configuration.
    /// </summary>
    /// <param name="config">WebSocket configuration options.</param>
    static member Create(config: WebSocketConfig) : FIO<WebSocketClient, WsError> =
        fio {
            let! clientSocket = FIO.attempt((fun () -> new Net.WebSockets.ClientWebSocket()), WsError.FromException)
            return WebSocketClient(clientSocket, config)
        }

    /// <summary>
    /// Creates a new WebSocket client with default configuration.
    /// </summary>
    static member Create() : FIO<WebSocketClient, WsError> =
        WebSocketClient.Create WebSocketConfig.Default

    /// <summary>
    /// Connects to a WebSocket server at the specified URI.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.Connect(uri: Uri, cancellationToken: CancellationToken) : FIO<WebSocket, WsError> =
        fio {
            let! connectTask = FIO.attempt((fun () -> clientSocket.ConnectAsync(uri, cancellationToken)), WsError.FromException)
            do! FIO.awaitTask(connectTask, WsError.FromException)
            return new WebSocket(clientSocket, config)
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URI.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    member this.Connect(uri: Uri) : FIO<WebSocket, WsError> =
        this.Connect(uri, CancellationToken.None)

    /// <summary>
    /// Connects to a WebSocket server at the specified URL.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.Connect(url: string, cancellationToken: CancellationToken) : FIO<WebSocket, WsError> =
        fio {
            let! uri = FIO.attempt((fun () -> Uri url), WsError.FromException)
            return! this.Connect(uri, cancellationToken)
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URL.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    member this.Connect(url: string) : FIO<WebSocket, WsError> =
        this.Connect(url, CancellationToken.None)
