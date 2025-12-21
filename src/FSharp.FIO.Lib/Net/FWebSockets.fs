(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional, effectful, and type-safe abstractions over .NET WebSockets for FIO.
/// </summary>
module FSharp.FIO.Lib.Net.FWebSockets

open FSharp.FIO.DSL

open System
open System.Net
open System.Text
open System.Text.Json
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// <summary>
/// A functional, effectful, and type-safe abstraction over a .NET WebSocket connection (server-side).
/// </summary>
type FWebSocket<'S, 'R, 'E> private (ctx: HttpListenerWebSocketContext, listenerCtx: HttpListenerContext, onError: exn -> 'E, options: JsonSerializerOptions) =

    // Partially applied functions as it is the same
    // onError function used everywhere in the type
    let fromFunc (func: unit -> 'T) : FIO<'T, 'E> =
        FIO.FromFunc (func, onError)

    let awaitTask (task: Task) : FIO<unit, 'E> =
        FIO.AwaitTask (task, onError)

    let awaitTaskT (task: Task<'T>) : FIO<'T, 'E> =
        FIO.AwaitTask (task, onError)

    /// <summary>
    /// Creates a new functional WebSocket from an existing context with custom error handling and JSON options.
    /// </summary>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx, onError: exn -> 'E, options) : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            return FWebSocket (ctx, listenerCtx, onError, options)
        }
    
    /// <summary>
    /// Creates a new functional WebSocket from an existing context with default error handling.
    /// </summary>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx, options) : FIO<FWebSocket<'S, 'R, exn>, exn> =
        FWebSocket.Create<'S, 'R, exn> (ctx, listenerCtx, id, options)
    
    /// <summary>
    /// Creates a new functional WebSocket from an existing context with custom error handling.
    /// </summary>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx, onError: exn -> 'E) : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        FWebSocket.Create<'S, 'R, 'E> (ctx, listenerCtx, onError, JsonSerializerOptions())
    
    /// <summary>
    /// Creates a new functional WebSocket from an existing context with default settings.
    /// </summary>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx) : FIO<FWebSocket<'S, 'R, exn>, exn> =
        FWebSocket.Create<'S, 'R, exn> (ctx, listenerCtx, id, JsonSerializerOptions())

    /// <summary>
    /// Sends a value over the WebSocket as a JSON-serialized string.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    /// <param name="messageType">The type of message to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.Send<'S, 'E> (msg: 'S, messageType: WebSocketMessageType, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! json = fromFunc <| fun () -> JsonSerializer.Serialize(msg, options)
            let! buffer = fromFunc <| fun () -> Encoding.UTF8.GetBytes json
            let! sendTask = fromFunc <| fun () -> ctx.WebSocket.SendAsync(ArraySegment<byte> buffer, messageType, endOfMessage, cancellationToken)
            do! awaitTask sendTask
        }

    /// <summary>
    /// Sends a value over the WebSocket with default message type.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Send<'S, 'E> (msg: 'S, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send (msg, WebSocketMessageType.Text, endOfMessage, cancellationToken)

    /// <summary>
    /// Sends a value over the WebSocket with default endOfMessage.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Send<'S, 'E> (msg: 'S, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send (msg, true, cancellationToken)

    /// <summary>
    /// Sends a value over the WebSocket with default settings.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    member this.Send<'S, 'E> (msg: 'S) : FIO<unit, 'E> =
        this.Send (msg, true, CancellationToken.None)

    /// <summary>
    /// Receives a value from the WebSocket, deserializing from JSON.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <param name="bufferSize">The size of the buffer to use for receiving.</param>
    member _.Receive<'R, 'E> (cancellationToken: CancellationToken, bufferSize: int) : FIO<'R, 'E> =
        fio {
            let! buffer = fromFunc <| fun () -> Array.zeroCreate bufferSize
            let! receiveTask = fromFunc <| fun () -> ctx.WebSocket.ReceiveAsync(ArraySegment<byte> buffer, cancellationToken)
            let! receiveResult = awaitTaskT receiveTask
            
            if receiveResult.MessageType = WebSocketMessageType.Close then
                let! closeTask = fromFunc <| fun () -> ctx.WebSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Received Close message", CancellationToken.None)
                do! awaitTask closeTask
                return! FIO.Fail (onError <| Exception "Received Close message")
            else
                let! json = fromFunc <| fun () -> Encoding.UTF8.GetString(buffer, 0, receiveResult.Count)
                let! msg = fromFunc <| fun () -> JsonSerializer.Deserialize<'R>(json, options)
                return msg
        }

    /// <summary>
    /// Receives a value from the WebSocket with default buffer size.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Receive<'R, 'E> (cancellationToken: CancellationToken) : FIO<'R, 'E> =
        this.Receive (cancellationToken, 1024)
        
    /// <summary>
    /// Receives a value from the WebSocket with default settings.
    /// </summary>
    member this.Receive<'R, 'E> () : FIO<'R, 'E> =
        this.Receive (CancellationToken.None, 1024)

    /// <summary>
    /// Closes the WebSocket with the specified status, description and cancellation.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = fromFunc <| fun () -> ctx.WebSocket.CloseAsync(closeStatus, statusDescription, cancellationToken)
            do! awaitTask closeTask
        }

    /// <summary>
    /// Closes the WebSocket with the specified status and description.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    member this.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.Close (closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the WebSocket with normal closure.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Close<'E> (cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Close (WebSocketCloseStatus.NormalClosure, "Normal Closure", cancellationToken)

    /// <summary>
    /// Closes the WebSocket with normal closure.
    /// </summary>
    member this.Close<'E> () : FIO<unit, 'E> =
        this.Close CancellationToken.None

    /// <summary>
    /// Aborts the WebSocket connection.
    /// </summary>
    member _.Abort<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> ctx.WebSocket.Abort ()
        }

    /// <summary>
    /// Gets the remote endpoint of the WebSocket connection.
    /// </summary>
    member _.RemoteEndPoint<'E> () : FIO<IPEndPoint, 'E> =
        fio {
            return! fromFunc <| fun () -> listenerCtx.Request.RemoteEndPoint
        }

    /// <summary>
    /// Gets the local endpoint of the WebSocket connection.
    /// </summary>
    member _.LocalEndPoint<'E> () : FIO<IPEndPoint, 'E> =
        fio {
            return! fromFunc <| fun () -> listenerCtx.Request.LocalEndPoint
        }

    /// <summary>
    /// Gets the state of the WebSocket.
    /// </summary>
    member _.State<'E> () : FIO<WebSocketState, 'E> =
        fio {
            return! fromFunc <| fun () -> ctx.WebSocket.State
        }

    /// <summary>
    /// Gets the negotiated subprotocol, if any.
    /// </summary>
    member _.Subprotocol<'E> () : FIO<string, 'E> =
        fio {
            return! fromFunc <| fun () -> ctx.WebSocket.SubProtocol
        }

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = fromFunc <| fun () -> ctx.WebSocket.CloseOutputAsync(closeStatus, statusDescription, cancellationToken)
            do! awaitTask closeTask
        }

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    member this.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.CloseOutput (closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection with normal closure.
    /// </summary>
    member this.CloseOutput<'E> () : FIO<unit, 'E> =
        this.CloseOutput (WebSocketCloseStatus.NormalClosure, "Normal Closure", CancellationToken.None)

    /// <summary>
    /// Gets the close status of the WebSocket connection, if closed.
    /// </summary>
    member _.CloseStatus<'E> () : FIO<WebSocketCloseStatus option, 'E> =
        fio {
            return! fromFunc <| fun () -> Option.ofNullable ctx.WebSocket.CloseStatus
        }

    /// <summary>
    /// Gets the close status description of the WebSocket connection.
    /// </summary>
    member _.CloseStatusDescription<'E> () : FIO<string, 'E> =
        fio {
            return! fromFunc <| fun () -> ctx.WebSocket.CloseStatusDescription
        }

    /// <summary>
    /// Disposes the underlying WebSocket.
    /// </summary>
    member _.Dispose<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> (ctx.WebSocket :> IDisposable).Dispose ()
        }

/// <summary>
/// A functional, effectful, and type-safe abstraction over a .NET server-side WebSocket listener.
/// </summary>
type FServerWebSocket<'S, 'R, 'E> private (listener: HttpListener, onError: exn -> 'E, options: JsonSerializerOptions) =

    // Partially applied functions as it is the same
    // onError function used everywhere in the type
    let fromFunc (func: unit -> 'T) : FIO<'T, 'E> =
        FIO.FromFunc (func, onError)

    let awaitTask (task: Task) : FIO<unit, 'E> =
        FIO.AwaitTask (task, onError)

    let awaitTaskT (task: Task<'T>) : FIO<'T, 'E> =
        FIO.AwaitTask (task, onError)

    /// <summary>
    /// Creates a new functional server WebSocket listener.
    /// </summary>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FServerWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! listener = FIO.FromFunc ((fun () -> new HttpListener()), onError)
            return FServerWebSocket (listener, onError, options)
        }
    
    /// <summary>
    /// Creates a new functional server WebSocket listener with default error handling.
    /// </summary>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (options: JsonSerializerOptions) : FIO<FServerWebSocket<'S, 'R, exn>, exn> =
        FServerWebSocket.Create<'S, 'R, exn> (id, options)
    
    /// <summary>
    /// Creates a new functional server WebSocket listener with custom error handling.
    /// </summary>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E) : FIO<FServerWebSocket<'S, 'R, 'E>, 'E> =
        FServerWebSocket.Create<'S, 'R, 'E> (onError, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional server WebSocket listener with default settings.
    /// </summary>
    static member Create<'S, 'R, 'E> () : FIO<FServerWebSocket<'S, 'R, exn>, exn> =
        FServerWebSocket.Create<'S, 'R, exn> (id, JsonSerializerOptions())

    /// <summary>
    /// Starts the HTTP listener for accepting WebSocket connections.
    /// </summary>
    /// <param name="url">The URL to listen on.</param>
    member _.Start<'E> url : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> listener.Prefixes.Add url
            do! fromFunc <| fun () -> listener.Start()
        }

    /// <summary>
    /// Accepts an incoming WebSocket connection.
    /// </summary>
    /// <param name="subProtocol">The subprotocol to negotiate, or null.</param>
    member _.Accept<'S, 'R, 'E> (subProtocol: string | null) : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! listenerCtxTask = fromFunc <| fun () -> listener.GetContextAsync()
            let! listenerCtx = awaitTaskT listenerCtxTask
            
            if listenerCtx.Request.IsWebSocketRequest then
                let! ctxTask = fromFunc <| fun () -> listenerCtx.AcceptWebSocketAsync subProtocol
                let! ctx = awaitTaskT ctxTask
                return! FWebSocket.Create<'S, 'R, 'E> (ctx, listenerCtx, onError, options)
            else
                do! fromFunc <| fun () -> listenerCtx.Response.StatusCode <- 400
                do! fromFunc <| fun () -> listenerCtx.Response.Close()
                let! error = FIO.Fail (onError <| Exception "Not a WebSocket request")
                return error
        }
    
    /// <summary>
    /// Accepts an incoming WebSocket connection with no subprotocol.
    /// </summary>
    member this.Accept<'S, 'R, 'E> () : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        this.Accept<'S, 'R, 'E> null

    /// <summary>
    /// Stops the HTTP listener.
    /// </summary>
    member _.Close<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> listener.Stop ()
        }

    /// <summary>
    /// Aborts the HTTP listener.
    /// </summary>
    member _.Abort<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> listener.Abort ()
        }

/// <summary>
/// A functional, effectful, and type-safe abstraction over a .NET client WebSocket.
/// </summary>
type FClientWebSocket<'S, 'R, 'E> private (clientWebSocket: ClientWebSocket, onError: exn -> 'E, options: JsonSerializerOptions) =

    // Partially applied functions as it is the same
    // onError function used everywhere in the type
    let fromFunc (func: unit -> 'T) : FIO<'T, 'E> =
        FIO.FromFunc (func, onError)

    let awaitTask (task: Task) : FIO<unit, 'E> =
        FIO.AwaitTask (task, onError)

    let awaitTaskT (task: Task<'T>) : FIO<'T, 'E> =
        FIO.AwaitTask (task, onError)

    /// <summary>
    /// Creates a new functional client WebSocket.
    /// </summary>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FClientWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! clientWebSocket = FIO.FromFunc ((fun () -> new ClientWebSocket()), onError)
            return FClientWebSocket (clientWebSocket, onError, options)
        }
    
    /// <summary>
    /// Creates a new functional client WebSocket with default error handling.
    /// </summary>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (options: JsonSerializerOptions) : FIO<FClientWebSocket<'S, 'R, exn>, exn> =
        FClientWebSocket.Create<'S, 'R, exn> (id, options)
    
    /// <summary>
    /// Creates a new functional client WebSocket with custom error handling.
    /// </summary>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E) : FIO<FClientWebSocket<'S, 'R, 'E>, 'E> =
        FClientWebSocket.Create<'S, 'R, 'E> (onError, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional client WebSocket with default settings.
    /// </summary>
    static member Create<'S, 'R, 'E> () : FIO<FClientWebSocket<'S, 'R, exn>, exn> =
        FClientWebSocket.Create<'S, 'R, exn> (id, JsonSerializerOptions())
    
    /// <summary>
    /// Connects the client WebSocket to the specified URL.
    /// </summary>
    /// <param name="url">The URL to connect to.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.Connect<'E> (url, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! uri = fromFunc <| fun () -> Uri url
            let! connectTask = fromFunc <| fun () -> clientWebSocket.ConnectAsync(uri, cancellationToken)
            do! awaitTask connectTask
        }

    /// <summary>
    /// Connects the client WebSocket to the specified URL.
    /// </summary>
    /// <param name="url">The URL to connect to.</param>
    member this.Connect<'E> url : FIO<unit, 'E> =
        this.Connect (url, CancellationToken.None)
        
    /// <summary>
    /// Sends a value over the client WebSocket as a JSON-serialized string.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    /// <param name="messageType">The type of message to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.Send<'S, 'E> (msg: 'S, messageType: WebSocketMessageType, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! json = fromFunc <| fun () -> JsonSerializer.Serialize(msg, options)
            let! buffer = fromFunc <| fun () -> Encoding.UTF8.GetBytes json
            let! sendTask = fromFunc <| fun () -> clientWebSocket.SendAsync(ArraySegment buffer, messageType, endOfMessage, cancellationToken)
            do! awaitTask sendTask
        }

    /// <summary>
    /// Sends a value over the client WebSocket with default message type.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Send<'S, 'E> (msg: 'S, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send (msg, WebSocketMessageType.Text, endOfMessage, cancellationToken)

    /// <summary>
    /// Sends a value over the client WebSocket with default endOfMessage.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Send<'S, 'E> (msg: 'S, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send (msg, true, cancellationToken)

    /// <summary>
    /// Sends a value over the client WebSocket with default settings.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    member this.Send<'S, 'E> (msg: 'S) : FIO<unit, 'E> =
        this.Send (msg, true, CancellationToken.None)

    /// <summary>
    /// Receives a value from the client WebSocket, deserializing from JSON.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <param name="bufferSize">The size of the buffer to use for receiving.</param>
    member _.Receive<'R, 'E> (cancellationToken: CancellationToken, bufferSize: int) : FIO<'R, 'E> =
        fio {
            let! buffer = fromFunc <| fun () -> Array.zeroCreate bufferSize
            let! receiveTask = fromFunc <| fun () -> clientWebSocket.ReceiveAsync(ArraySegment buffer, cancellationToken)
            let! receiveResult = awaitTaskT receiveTask
            let! json = fromFunc <| fun () -> Encoding.UTF8.GetString(buffer, 0, receiveResult.Count)
            let! msg = fromFunc <| fun () -> JsonSerializer.Deserialize<'R>(json, options)
            return msg
        }

    /// <summary>
    /// Receives a value from the client WebSocket with default buffer size.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Receive<'R, 'E> (cancellationToken: CancellationToken) : FIO<'R, 'E> =
        this.Receive (cancellationToken, 1024)
    
    /// <summary>
    /// Receives a value from the client WebSocket with default settings.
    /// </summary>
    member this.Receive<'R, 'E> () : FIO<'R, 'E> =
        this.Receive (CancellationToken.None, 1024)

    /// <summary>
    /// Closes the client WebSocket with the specified status, description and cancellation.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = fromFunc <| fun () -> clientWebSocket.CloseAsync(closeStatus, statusDescription, cancellationToken)
            do! awaitTask closeTask
        }

    /// <summary>
    /// Closes the client WebSocket with the specified status and description.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    member this.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.Close (closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the client WebSocket with normal closure.
    /// </summary>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member this.Close<'E> (cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Close (WebSocketCloseStatus.NormalClosure, "Normal Closure", cancellationToken)

    /// <summary>
    /// Closes the client WebSocket with normal closure.
    /// </summary>
    member this.Close<'E> () : FIO<unit, 'E> =
        this.Close CancellationToken.None

    /// <summary>
    /// Aborts the client WebSocket connection.
    /// </summary>
    member _.Abort<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> clientWebSocket.Abort()
        }

    /// <summary>
    /// Gets the state of the WebSocket.
    /// </summary>
    member _.State<'E> () : FIO<WebSocketState, 'E> =
        fio {
            return! fromFunc <| fun () -> clientWebSocket.State
        }

    /// <summary>
    /// Gets the negotiated subprotocol, if any.
    /// </summary>
    member _.Subprotocol<'E> () : FIO<string, 'E> =
        fio {
            return! fromFunc <| fun () -> clientWebSocket.SubProtocol
        }

    /// <summary>
    /// Closes the outgoing side of the client WebSocket connection.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    member _.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = fromFunc <| fun () -> clientWebSocket.CloseOutputAsync(closeStatus, statusDescription, cancellationToken)
            do! awaitTask closeTask
        }

    /// <summary>
    /// Closes the outgoing side of the client WebSocket connection.
    /// </summary>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    member this.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.CloseOutput (closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the outgoing side of the client WebSocket connection with normal closure.
    /// </summary>
    member this.CloseOutput<'E> () : FIO<unit, 'E> =
        this.CloseOutput (WebSocketCloseStatus.NormalClosure, "Normal Closure", CancellationToken.None)

    /// <summary>
    /// Gets the close status of the client WebSocket connection, if closed.
    /// </summary>
    member _.CloseStatus<'E> () : FIO<WebSocketCloseStatus option, 'E> =
        fio {
            return! fromFunc <| fun () -> Option.ofNullable clientWebSocket.CloseStatus
        }

    /// <summary>
    /// Gets the close status description of the client WebSocket connection.
    /// </summary>
    member _.CloseStatusDescription<'E> () : FIO<string, 'E> =
        fio {
            return! fromFunc <| fun () -> clientWebSocket.CloseStatusDescription
        }

    /// <summary>
    /// Disposes the underlying client WebSocket.
    /// </summary>
    member _.Dispose<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> (clientWebSocket :> IDisposable).Dispose ()
        }
