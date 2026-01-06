/// <summary>
/// Provides functional, effectful, and type-safe abstractions over .NET WebSockets for FIO.
/// </summary>
module FSharp.FIO.WebSockets

open FSharp.FIO.DSL

open System
open System.Net
open System.Text
open System.Buffers
open System.Text.Json
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// <summary>
/// Represents all WebSocket frame types.
/// </summary>
type WebSocketFrame =
    /// Text frame containing UTF-8 encoded string data
    | Text of string
    /// Binary frame containing raw byte data
    | Binary of byte[]
    /// Close frame with status code and reason
    | Close of WebSocketCloseStatus * string
    /// Ping frame for keep-alive (contains payload data)
    | Ping of byte[]
    /// Pong frame responding to ping (contains payload data)
    | Pong of byte[]

/// <summary>
/// Messages received from a WebSocket connection.
/// Distinguishes between frames and connection closure events.
/// </summary>
type WebSocketMessage =
    /// A WebSocket frame was received
    | Frame of WebSocketFrame
    /// The connection was closed (with optional status and description)
    | ConnectionClosed of WebSocketCloseStatus option * string

/// <summary>
/// A functional, effectful, and type-safe abstraction over a WebSocket connection.
/// Works for both client and server connections after establishment.
/// </summary>
type WebSocket<'E> internal (socket: WebSockets.WebSocket, onError: exn -> 'E) =

    // Partially applied functions for consistent error handling
    let fromFunc (func: unit -> 'T) : FIO<'T, 'E> =
        FIO.Attempt (func, onError)

    let awaitTask (task: Task) : FIO<unit, 'E> =
        FIO.AwaitTask (task, onError)

    let awaitTaskT (task: Task<'T>) : FIO<'T, 'E> =
        FIO.AwaitTask (task, onError)

    /// <summary>
    /// Receives a complete message from the WebSocket, handling fragmentation automatically.
    /// Uses buffer pooling to minimize GC pressure.
    /// </summary>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    /// <returns>A WebSocketMessage containing either a frame or connection closed event.</returns>
    member _.ReceiveMessage (cancellationToken: CancellationToken) : FIO<WebSocketMessage, 'E> =
        fio {
            let bufferSize = 4096
            let buffer = ArrayPool<byte>.Shared.Rent bufferSize

            let! result = fio {
                let fragments = ResizeArray<byte>()
                let mutable endOfMessage = false
                let mutable messageType = WebSocketMessageType.Text
                let mutable isCloseFrame = false

                while not endOfMessage do
                    let! receiveTask = fromFunc <| fun () ->
                        socket.ReceiveAsync(ArraySegment(buffer, 0, bufferSize), cancellationToken)
                    let! receiveResult = awaitTaskT receiveTask

                    messageType <- receiveResult.MessageType
                    endOfMessage <- receiveResult.EndOfMessage

                    if messageType = WebSocketMessageType.Close then
                        isCloseFrame <- true
                        endOfMessage <- true
                    else
                        fragments.AddRange(buffer.[0..receiveResult.Count-1])

                if isCloseFrame then
                    let status = Option.ofNullable socket.CloseStatus
                    let desc = socket.CloseStatusDescription
                    return ConnectionClosed (status, desc)
                else
                    let data = fragments.ToArray()
                    let frame =
                        match messageType with
                        | WebSocketMessageType.Text ->
                            let text = Encoding.UTF8.GetString data
                            Text text
                        | WebSocketMessageType.Binary ->
                            Binary data
                        | _ ->
                            failwith "Unexpected message type"

                    return Frame frame
            }

            do! fromFunc <| fun () -> ArrayPool<byte>.Shared.Return buffer
            return result
        }

    /// <summary>
    /// Receives a complete message from the WebSocket with no cancellation.
    /// </summary>
    member this.ReceiveMessage () : FIO<WebSocketMessage, 'E> =
        this.ReceiveMessage CancellationToken.None

    // ------------------------------------------------------------------------
    // Send Operations
    // ------------------------------------------------------------------------

    /// <summary>
    /// Sends a WebSocket frame.
    /// </summary>
    /// <param name="frame">The frame to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.SendFrame (frame: WebSocketFrame, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            match frame with
            | Text text ->
                let! buffer = fromFunc <| fun () -> Encoding.UTF8.GetBytes text
                let! sendTask = fromFunc <| fun () ->
                    socket.SendAsync(
                        ArraySegment buffer,
                        WebSocketMessageType.Text,
                        true,
                        cancellationToken)
                do! awaitTask sendTask

            | Binary data ->
                let! sendTask = fromFunc <| fun () ->
                    socket.SendAsync(
                        ArraySegment data,
                        WebSocketMessageType.Binary,
                        true,
                        cancellationToken)
                do! awaitTask sendTask

            | Close (status, description) ->
                let! closeTask = fromFunc <| fun () ->
                    socket.CloseAsync(status, description, cancellationToken)
                do! awaitTask closeTask

            | Ping data ->
                // Note: .NET WebSockets don't have explicit ping/pong at the API level
                // They're handled automatically by the implementation
                // We send as binary for now - users can implement custom ping/pong if needed
                let! sendTask = fromFunc <| fun () ->
                    socket.SendAsync(
                        ArraySegment data,
                        WebSocketMessageType.Binary,
                        true,
                        cancellationToken)
                do! awaitTask sendTask

            | Pong data ->
                // Same as Ping - .NET handles this automatically
                let! sendTask = fromFunc <| fun () ->
                    socket.SendAsync(
                        ArraySegment data,
                        WebSocketMessageType.Binary,
                        true,
                        cancellationToken)
                do! awaitTask sendTask
        }

    /// <summary>
    /// Sends a WebSocket frame with no cancellation.
    /// </summary>
    member this.SendFrame (frame: WebSocketFrame) : FIO<unit, 'E> =
        this.SendFrame(frame, CancellationToken.None)

    /// <summary>
    /// Sends a text frame.
    /// </summary>
    /// <param name="text">The text to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.SendText (text: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.SendFrame(Text text, cancellationToken)

    /// <summary>
    /// Sends a text frame with no cancellation.
    /// </summary>
    member this.SendText (text: string) : FIO<unit, 'E> =
        this.SendText(text, CancellationToken.None)

    /// <summary>
    /// Sends a binary frame.
    /// </summary>
    /// <param name="data">The binary data to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.SendBinary (data: byte[], cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.SendFrame(Binary data, cancellationToken)

    /// <summary>
    /// Sends a binary frame with no cancellation.
    /// </summary>
    member this.SendBinary (data: byte[]) : FIO<unit, 'E> =
        this.SendBinary(data, CancellationToken.None)

    /// <summary>
    /// Sends a ping frame.
    /// Note: .NET WebSockets handle ping/pong automatically in most cases.
    /// </summary>
    /// <param name="data">Optional payload data.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.SendPing (data: byte[], cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.SendFrame(Ping data, cancellationToken)

    /// <summary>
    /// Sends a ping frame with no cancellation.
    /// </summary>
    member this.SendPing (data: byte[]) : FIO<unit, 'E> =
        this.SendPing(data, CancellationToken.None)

    /// <summary>
    /// Sends a ping frame with empty payload.
    /// </summary>
    member this.SendPing () : FIO<unit, 'E> =
        this.SendPing(Array.empty, CancellationToken.None)

    /// <summary>
    /// Sends a pong frame in response to a ping.
    /// Note: .NET WebSockets handle ping/pong automatically in most cases.
    /// </summary>
    /// <param name="data">The payload data (typically echoing the ping payload).</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.SendPong (data: byte[], cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.SendFrame(Pong data, cancellationToken)

    /// <summary>
    /// Sends a pong frame with no cancellation.
    /// </summary>
    member this.SendPong (data: byte[]) : FIO<unit, 'E> =
        this.SendPong(data, CancellationToken.None)

    /// <summary>
    /// Closes the WebSocket connection with the specified status and description.
    /// </summary>
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.Close (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = fromFunc <| fun () ->
                socket.CloseAsync(closeStatus, statusDescription, cancellationToken)
            do! awaitTask closeTask
        }

    /// <summary>
    /// Closes the WebSocket connection with the specified status and description.
    /// </summary>
    member this.Close (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.Close(closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the WebSocket connection with normal closure.
    /// </summary>
    member this.Close (cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Close(WebSocketCloseStatus.NormalClosure, "Normal closure", cancellationToken)

    /// <summary>
    /// Closes the WebSocket connection with normal closure.
    /// </summary>
    member this.Close () : FIO<unit, 'E> =
        this.Close CancellationToken.None

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection (half-close).
    /// </summary>
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.CloseOutput (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = fromFunc <| fun () ->
                socket.CloseOutputAsync(closeStatus, statusDescription, cancellationToken)
            do! awaitTask closeTask
        }

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection.
    /// </summary>
    member this.CloseOutput (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.CloseOutput(closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the outgoing side with normal closure.
    /// </summary>
    member this.CloseOutput () : FIO<unit, 'E> =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal closure", CancellationToken.None)

    /// <summary>
    /// Aborts the WebSocket connection immediately.
    /// </summary>
    member _.Abort () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> socket.Abort()
        }
    
    /// <summary>
    /// Gets the current state of the WebSocket connection.
    /// </summary>
    member _.State () : FIO<WebSocketState, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.State
        }

    /// <summary>
    /// Gets the close status if the connection is closed.
    /// </summary>
    member _.CloseStatus () : FIO<WebSocketCloseStatus option, 'E> =
        fio {
            return! fromFunc <| fun () -> Option.ofNullable socket.CloseStatus
        }

    /// <summary>
    /// Gets the close status description if the connection is closed.
    /// </summary>
    member _.CloseStatusDescription () : FIO<string, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.CloseStatusDescription
        }

    /// <summary>
    /// Gets the negotiated subprotocol, if any.
    /// </summary>
    member _.Subprotocol () : FIO<string, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.SubProtocol
        }

    /// <summary>
    /// Disposes the underlying WebSocket.
    /// </summary>
    member _.Dispose () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> socket.Dispose()
        }

    /// Internal accessor for JSON extensions
    member internal _.OnError = onError

/// <summary>
/// A WebSocket client for establishing outgoing connections.
/// </summary>
type WebSocketClient<'E> private (clientSocket: ClientWebSocket, onError: exn -> 'E) =

    /// <summary>
    /// Creates a new WebSocket client.
    /// </summary>
    /// <param name="onError">Function to map exceptions to the error type.</param>
    static member Create (onError: exn -> 'E) : FIO<WebSocketClient<'E>, 'E> =
        fio {
            let! clientSocket = FIO.Attempt((fun () -> new ClientWebSocket()), onError)
            return WebSocketClient(clientSocket, onError)
        }

    /// <summary>
    /// Creates a new WebSocket client with default error handling (exn).
    /// </summary>
    static member Create () : FIO<WebSocketClient<exn>, exn> =
        WebSocketClient<exn>.Create id

    /// <summary>
    /// Connects to a WebSocket server at the specified URI.
    /// </summary>
    /// <param name="uri">The WebSocket server URI.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    /// <returns>A connected WebSocket.</returns>
    member _.Connect (uri: Uri, cancellationToken: CancellationToken) : FIO<WebSocket<'E>, 'E> =
        fio {
            let! connectTask = FIO.Attempt((fun () -> clientSocket.ConnectAsync(uri, cancellationToken)), onError)
            do! FIO.AwaitTask(connectTask, onError)
            return WebSocket<'E>(clientSocket :> WebSockets.WebSocket, onError)
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URI.
    /// </summary>
    member this.Connect (uri: Uri) : FIO<WebSocket<'E>, 'E> =
        this.Connect(uri, CancellationToken.None)

    /// <summary>
    /// Connects to a WebSocket server at the specified URL.
    /// </summary>
    /// <param name="url">The WebSocket server URL.</param>
    /// <parameter name="cancellationToken">Optional cancellation token.</param>
    member this.Connect (url: string, cancellationToken: CancellationToken) : FIO<WebSocket<'E>, 'E> =
        fio {
            let! uri = FIO.Attempt((fun () -> Uri url), onError)
            return! this.Connect(uri, cancellationToken)
        }

    /// <summary>
    /// Connects to a WebSocket server at the specified URL.
    /// </summary>
    member this.Connect (url: string) : FIO<WebSocket<'E>, 'E> =
        this.Connect(url, CancellationToken.None)
/// <summary>
/// A WebSocket server for accepting incoming connections.
/// </summary>
type WebSocketServer<'E> private (listener: HttpListener, onError: exn -> 'E) =

    /// <summary>
    /// Creates a new WebSocket server.
    /// </summary>
    /// <param name="onError">Function to map exceptions to the error type.</param>
    static member Create (onError: exn -> 'E) : FIO<WebSocketServer<'E>, 'E> =
        fio {
            let! listener = FIO.Attempt((fun () -> new HttpListener()), onError)
            return WebSocketServer(listener, onError)
        }

    /// <summary>
    /// Creates a new WebSocket server with default error handling (exn).
    /// </summary>
    static member Create () : FIO<WebSocketServer<exn>, exn> =
        WebSocketServer<exn>.Create id

    /// <summary>
    /// Starts the server listening on the specified URL prefix.
    /// </summary>
    /// <param name="url">The URL prefix to listen on (e.g., "http://localhost:8080/").</param>
    member _.Start (url: string) : FIO<unit, 'E> =
        fio {
            do! FIO.Attempt((fun () -> listener.Prefixes.Add url), onError)
            do! FIO.Attempt((fun () -> listener.Start()), onError)
        }

    /// <summary>
    /// Accepts an incoming WebSocket connection.
    /// </summary>
    /// <param name="subProtocol">Optional subprotocol to negotiate.</param>
    /// <returns>A connected WebSocket.</returns>
    member _.Accept (subProtocol: string option) : FIO<WebSocket<'E>, 'E> =
        fio {
            let! listenerCtxTask = FIO.Attempt((fun () -> listener.GetContextAsync()), onError)
            let! listenerCtx = FIO.AwaitTask(listenerCtxTask, onError)

            if listenerCtx.Request.IsWebSocketRequest then
                let subProto = match subProtocol with | Some s -> s | None -> null
                let! ctxTask = FIO.Attempt((fun () -> listenerCtx.AcceptWebSocketAsync subProto), onError)
                let! ctx = FIO.AwaitTask(ctxTask, onError)
                return WebSocket<'E>(ctx.WebSocket, onError)
            else
                do! FIO.Attempt((fun () -> listenerCtx.Response.StatusCode <- 400), onError)
                do! FIO.Attempt((fun () -> listenerCtx.Response.Close()), onError)
                return! FIO.Fail (onError <| Exception "Not a WebSocket request")
        }

    /// <summary>
    /// Accepts an incoming WebSocket connection with no subprotocol.
    /// </summary>
    member this.Accept () : FIO<WebSocket<'E>, 'E> =
        this.Accept None

    /// <summary>
    /// Stops the server.
    /// </summary>
    member _.Close () : FIO<unit, 'E> =
        fio {
            do! FIO.Attempt((fun () -> listener.Stop()), onError)
        }

    /// <summary>
    /// Aborts the server immediately.
    /// </summary>
    member _.Abort () : FIO<unit, 'E> =
        fio {
            do! FIO.Attempt((fun () -> listener.Abort()), onError)
        }

/// <summary>
/// Optional extension methods for JSON serialization over WebSockets.
/// Provides convenient SendJson/ReceiveJson methods on top of the frame-based API.
/// </summary>
[<AutoOpen>]
module WebSocketJsonExtensions =

    type WebSocket<'E> with

        /// <summary>
        /// Sends a value as JSON over the WebSocket.
        /// Serializes the value to JSON and sends as a Text frame.
        /// </summary>
        /// <param name="value">The value to send.</param>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <param name="cancellationToken">Optional cancellation token.</param>
        member this.SendJson<'T> (value: 'T, ?options: JsonSerializerOptions, ?cancellationToken: CancellationToken) : FIO<unit, 'E> =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let ct = defaultArg cancellationToken CancellationToken.None
                let! json = FIO.Attempt((fun () -> JsonSerializer.Serialize(value, opts)), this.OnError)
                do! this.SendText(json, ct)
            }

        /// <summary>
        /// Receives and deserializes a JSON value from the WebSocket.
        /// Waits for a Text frame and deserializes it as JSON.
        /// </summary>
        /// <param name="options">Optional JSON serializer options.</param>
        /// <param name="cancellationToken">Optional cancellation token.</param>
        /// <returns>The deserialized value.</returns>
        member this.ReceiveJson<'T> (?options: JsonSerializerOptions, ?cancellationToken: CancellationToken) : FIO<'T, 'E> =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let ct = defaultArg cancellationToken CancellationToken.None
                let! msg = this.ReceiveMessage ct

                match msg with
                | Frame (Text json) ->
                    return! FIO.Attempt((fun () -> JsonSerializer.Deserialize<'T>(json, opts)), this.OnError)
                | Frame (Binary _) ->
                    return! FIO.Fail (this.OnError (Exception "Expected text frame for JSON, got binary"))
                | Frame (Close _) ->
                    return! FIO.Fail (this.OnError (Exception "Connection closed while waiting for JSON"))
                | Frame (Ping _) | Frame (Pong _) ->
                    return! FIO.Fail (this.OnError (Exception "Expected text frame for JSON, got control frame"))
                | ConnectionClosed (status, desc) ->
                    return! FIO.Fail (this.OnError (Exception $"Connection closed. Status: {status}, Description: {desc}"))
            }
