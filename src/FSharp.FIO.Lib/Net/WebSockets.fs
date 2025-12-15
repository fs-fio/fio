(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional, effectful, and type-safe abstractions over .NET WebSockets for FIO.
/// Includes FWebSocket, FServerWebSocket, and FClientWebSocket types for server and client-side WebSocket communication, with JSON serialization and composable effectful APIs.
/// </summary>
module FSharp.FIO.Lib.Net.WebSockets

open FSharp.FIO.DSL

open System
open System.Net
open System.Text
open System.Text.Json
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// <summary>
/// Represents a functional, effectful, and type-safe abstraction over a .NET WebSocket connection (server-side).
/// <para>
/// Provides FIO-based methods for sending and receiving typed messages (with JSON serialization), closing or aborting the connection,
/// querying connection state, and accessing connection metadata such as endpoints and subprotocol.
/// </para>
/// <para>
/// All operations are asynchronous, non-blocking, and return FIO effects, enabling composable and safe WebSocket communication in a purely functional style.
/// </para>
/// </summary>
type FWebSocket<'S, 'R, 'E> private (ctx: HttpListenerWebSocketContext, listenerCtx: HttpListenerContext, onError: exn -> 'E, options: JsonSerializerOptions) =

    // Partially applied functions as it is the same
    // onError function used everywhere in the type
    let ( !<<< ) (func: unit -> 'T) : FIO<'T, 'E> =
        !<<< func onError

    let ( !<<~ ) (task: Task) : FIO<unit, 'E> =
        !<<~ task onError

    let ( !<<~~ ) (task: Task<'T>) : FIO<'T, 'E> =
        !<<~~ task onError

    /// <summary>
    /// Creates a new functional WebSocket abstraction from an existing .NET WebSocket context, with custom error handling and JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx, onError: exn -> 'E, options) : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            return FWebSocket (ctx, listenerCtx, onError, options)
        }
    
    /// <summary>
    /// Creates a new functional WebSocket abstraction from an existing .NET WebSocket context, using default error handling and JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx, options) : FIO<FWebSocket<'S, 'R, exn>, exn> =
        FWebSocket.Create<'S, 'R, exn> (ctx, listenerCtx, id, options)
    
    /// <summary>
    /// Creates a new functional WebSocket abstraction from an existing .NET WebSocket context, with custom error handling and default JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>A new FWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx, onError: exn -> 'E) : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        FWebSocket.Create<'S, 'R, 'E> (ctx, listenerCtx, onError, JsonSerializerOptions())
    
    /// <summary>
    /// Creates a new functional WebSocket abstraction from an existing .NET WebSocket context, using default error handling and default JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="ctx">The underlying .NET WebSocket context.</param>
    /// <param name="listenerCtx">The underlying .NET HttpListener context.</param>
    /// <returns>A new FWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (ctx, listenerCtx) : FIO<FWebSocket<'S, 'R, exn>, exn> =
        FWebSocket.Create<'S, 'R, exn> (ctx, listenerCtx, id, JsonSerializerOptions())

    /// <summary>
    /// Sends a value of type 'S over the WebSocket as a JSON-serialized string.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <param name="messageType">The type of message to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that sends the value over the WebSocket or returns an error of type 'E.</returns>
    member _.Send<'S, 'E> (msg: 'S, messageType: WebSocketMessageType, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! json = !<<< (fun () -> JsonSerializer.Serialize(msg, options))
            let! buffer = !<<< (fun () -> Encoding.UTF8.GetBytes json)
            let! sendTask = !<<< (fun () -> ctx.WebSocket.SendAsync(ArraySegment<byte> buffer, messageType, endOfMessage, cancellationToken))
            do! !<<~ sendTask
        }

    /// <summary>
    /// Sends a value of type 'S over the WebSocket as a JSON-serialized string with default message type and cancellation.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that sends the value over the WebSocket or returns an error of type 'E.</returns>
    member this.Send<'S, 'E> (msg: 'S, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send<'S, 'E> (msg, WebSocketMessageType.Text, endOfMessage, cancellationToken)

    /// <summary>
    /// Sends a value of type 'S over the WebSocket as a JSON-serialized string with default endOfMessage and cancellation.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that sends the value over the WebSocket or returns an error of type 'E.</returns>
    member this.Send<'S, 'E> (msg: 'S, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send<'S, 'E> (msg, true, cancellationToken)

    /// <summary>
    /// Sends a value of type 'S over the WebSocket as a JSON-serialized string with default endOfMessage and no cancellation.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <returns>An FIO effect that sends the value over the WebSocket or returns an error of type 'E.</returns>
    member this.Send<'S, 'E> (msg: 'S) : FIO<unit, 'E> =
        this.Send<'S, 'E> (msg, true, CancellationToken.None)

    /// <summary>
    /// Receives a value of type 'R from the WebSocket, deserializing from JSON.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <param name="bufferSize">The size of the buffer to use for receiving.</param>
    /// <returns>An FIO effect that receives a value from the WebSocket or returns an error of type 'E.</returns>
    member _.Receive<'R, 'E> (cancellationToken: CancellationToken, bufferSize: int) : FIO<'R, 'E> =
        fio {
            let! buffer = !<<< (fun () -> Array.zeroCreate bufferSize)
            let! receiveTask = !<<< (fun () -> ctx.WebSocket.ReceiveAsync(ArraySegment<byte> buffer, cancellationToken))
            let! receiveResult = !<<~~ receiveTask
            
            if receiveResult.MessageType = WebSocketMessageType.Close then
                let! closeTask = !<<< (fun () -> ctx.WebSocket.CloseAsync(WebSocketCloseStatus.NormalClosure, "Received Close message", CancellationToken.None))
                do! !<<~ closeTask
                return! !- (onError <| Exception "Received Close message")
            else
                let! json = !<<< (fun () -> Encoding.UTF8.GetString(buffer, 0, receiveResult.Count))
                let! msg = !<<< (fun () -> JsonSerializer.Deserialize<'R>(json, options))
                return msg
        }

    /// <summary>
    /// Receives a value of type 'R from the WebSocket with default buffer size.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that receives a value from the WebSocket or returns an error of type 'E.</returns>
    member this.Receive<'R, 'E> (cancellationToken: CancellationToken) : FIO<'R, 'E> =
        this.Receive<'R, 'E> (cancellationToken, 1024)
        
    /// <summary>
    /// Receives a value of type 'R from the WebSocket with default buffer size and no cancellation.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that receives a value from the WebSocket or returns an error of type 'E.</returns>
    member this.Receive<'R, 'E> () : FIO<'R, 'E> =
        this.Receive<'R, 'E> (CancellationToken.None, 1024)

    /// <summary>
    /// Closes the WebSocket with the specified status, description and cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that closes the WebSocket or returns an error of type 'E.</returns>
    member _.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = !<<< (fun () -> ctx.WebSocket.CloseAsync(closeStatus, statusDescription, cancellationToken))
            do! !<<~ closeTask
        }

    /// <summary>
    /// Closes the WebSocket with the specified status and description, no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <returns>An FIO effect that closes the WebSocket or returns an error of type 'E.</returns>
    member this.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.Close<'E> (closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the WebSocket with normal closure and optional cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that closes the WebSocket or returns an error of type 'E.</returns>
    member this.Close<'E> (cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Close<'E> (WebSocketCloseStatus.NormalClosure, "Normal Closure", cancellationToken)

    /// <summary>
    /// Closes the WebSocket with normal closure and no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that closes the WebSocket or returns an error of type 'E.</returns>
    member this.Close<'E> () : FIO<unit, 'E> =
        this.Close<'E> CancellationToken.None

    /// <summary>
    /// Aborts the WebSocket connection.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that aborts the WebSocket or returns an error of type 'E.</returns>
    member _.Abort<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> ctx.WebSocket.Abort())
        }

    /// <summary>
    /// Gets the remote endpoint of the WebSocket connection.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the remote endpoint or returns an error of type 'E.</returns>
    member _.RemoteEndPoint<'E> () : FIO<IPEndPoint, 'E> =
        fio {
            return! !<<< (fun () -> listenerCtx.Request.RemoteEndPoint)
        }

    /// <summary>
    /// Gets the local endpoint of the WebSocket connection.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the local endpoint or returns an error of type 'E.</returns>
    member _.LocalEndPoint<'E> () : FIO<IPEndPoint, 'E> =
        fio {
            return! !<<< (fun () -> listenerCtx.Request.LocalEndPoint)
        }

    /// <summary>
    /// Returns the state of the WebSocket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the WebSocket state or returns an error of type 'E.</returns>
    member _.State<'E> () : FIO<WebSocketState, 'E> =
        fio {
            return! !<<< (fun () -> ctx.WebSocket.State)
        }

    /// <summary>
    /// Returns the negotiated subprotocol, if any.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the subprotocol or returns an error of type 'E.</returns>
    member _.Subprotocol<'E> () : FIO<string, 'E> =
        fio {
            return! !<<< (fun () -> ctx.WebSocket.SubProtocol)
        }

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection with the specified status, description, and cancellation token.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that closes the outgoing side of the WebSocket or returns an error of type 'E.</returns>
    member _.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = !<<< (fun () -> ctx.WebSocket.CloseOutputAsync(closeStatus, statusDescription, cancellationToken))
            do! !<<~ closeTask
        }

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection with the specified status and description, no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <returns>An FIO effect that closes the outgoing side of the WebSocket or returns an error of type 'E.</returns>
    member this.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.CloseOutput(closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection with normal closure, "Normal Closure", and no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that closes the outgoing side of the WebSocket or returns an error of type 'E.</returns>
    member this.CloseOutput<'E> () : FIO<unit, 'E> =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal Closure", CancellationToken.None)

    /// <summary>
    /// Gets the close status of the WebSocket connection, if closed.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the close status or returns an error of type 'E.</returns>
    member _.CloseStatus<'E> () : FIO<WebSocketCloseStatus option, 'E> =
        fio {
            return! !<<< (fun () -> Option.ofNullable ctx.WebSocket.CloseStatus)
        }

    /// <summary>
    /// Gets the close status description of the WebSocket connection.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the close status description or returns an error of type 'E.</returns>
    member _.CloseStatusDescription<'E> () : FIO<string, 'E> =
        fio {
            return! !<<< (fun () -> ctx.WebSocket.CloseStatusDescription)
        }

    /// <summary>
    /// Disposes the underlying WebSocket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that disposes the underlying WebSocket or returns an error of type 'E.</returns>
    member _.Dispose<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> (ctx.WebSocket :> IDisposable).Dispose())
        }

/// <summary>
/// Represents a functional, effectful, and type-safe abstraction over a .NET server-side WebSocket listener.
/// <para>
/// Provides FIO-based methods for starting and stopping an HTTP listener, accepting incoming WebSocket connections (with optional subprotocol negotiation),
/// and managing the server's lifecycle in a purely functional style.
/// </para>
/// <para>
/// All operations are asynchronous, non-blocking, and return FIO effects, enabling composable and safe server-side WebSocket handling.
/// </para>
/// </summary>
type FServerWebSocket<'S, 'R, 'E> private (listener: HttpListener, onError: exn -> 'E, options: JsonSerializerOptions) =

    // Partially applied functions as it is the same
    // onError function used everywhere in the type
    let ( !<<< ) (func: unit -> 'T) : FIO<'T, 'E> =
        !<<< func onError

    let ( !<<~ ) (task: Task) : FIO<unit, 'E> =
        !<<~ task onError

    let ( !<<~~ ) (task: Task<'T>) : FIO<'T, 'E> =
        !<<~~ task onError

    /// <summary>
    /// Creates a new functional server WebSocket listener abstraction.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FServerWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FServerWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! listener = !<<< (fun () -> new HttpListener()) onError
            return FServerWebSocket (listener, onError, options)
        }
    
    /// <summary>
    /// Creates a new functional server WebSocket listener abstraction with default options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FServerWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (options: JsonSerializerOptions) : FIO<FServerWebSocket<'S, 'R, exn>, exn> =
        FServerWebSocket.Create<'S, 'R, exn> (id, options)
    
    /// <summary>
    /// Creates a new functional server WebSocket listener abstraction with a custom onError function.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>A new FServerWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E) : FIO<FServerWebSocket<'S, 'R, 'E>, 'E> =
        FServerWebSocket.Create<'S, 'R, 'E> (onError, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional server WebSocket listener abstraction with default options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>A new FServerWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> () : FIO<FServerWebSocket<'S, 'R, exn>, exn> =
        FServerWebSocket.Create<'S, 'R, exn> (id, JsonSerializerOptions())

    /// <summary>
    /// Starts the HTTP listener for accepting WebSocket connections on the specified URL.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="url">The URL to listen on.</param>
    /// <returns>An FIO effect that starts the listener or returns an error of type 'E.</returns>
    member _.Start<'E> url : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> listener.Prefixes.Add url)
            do! !<<< (fun () -> listener.Start())
        }

    /// <summary>
    /// Accepts an incoming WebSocket connection, optionally specifying a subprotocol.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="subProtocol">The subprotocol to negotiate, or null.</param>
    /// <returns>An FIO effect that accepts an incoming WebSocket connection or returns an error of type 'E.</returns>
    member _.Accept<'S, 'R, 'E> (subProtocol: string | null) : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! listenerCtxTask = !<<< (fun () -> listener.GetContextAsync())
            let! listenerCtx = !<<~~ listenerCtxTask
            
            if listenerCtx.Request.IsWebSocketRequest then
                let! ctxTask = !<<< (fun () -> listenerCtx.AcceptWebSocketAsync subProtocol)
                let! ctx = !<<~~ ctxTask
                return! FWebSocket.Create<'S, 'R, 'E> (ctx, listenerCtx, onError, options)
            else
                do! !<<< (fun () -> listenerCtx.Response.StatusCode <- 400)
                do! !<<< (fun () -> listenerCtx.Response.Close())
                let! error = !- (onError <| Exception "Not a WebSocket request")
                return error
        }
    
    /// <summary>
    /// Accepts an incoming WebSocket connection with no subprotocol.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that accepts an incoming WebSocket connection or returns an error of type 'E.</returns>
    member this.Accept<'S, 'R, 'E> () : FIO<FWebSocket<'S, 'R, 'E>, 'E> =
        this.Accept<'S, 'R, 'E> null

    /// <summary>
    /// Stops the HTTP listener.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that stops the listener or returns an error of type 'E.</returns>
    member _.Close<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> listener.Stop())
        }

    /// <summary>
    /// Aborts the HTTP listener.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that aborts the listener or returns an error of type 'E.</returns>
    member _.Abort<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> listener.Abort())
        }

/// <summary>
/// Represents a functional, effectful, and type-safe abstraction over a .NET client WebSocket.
/// <para>
/// Provides FIO-based methods for connecting to a WebSocket server, sending and receiving typed messages (with JSON serialization),
/// closing or aborting the connection, and querying connection state and subprotocol.
/// </para>
/// <para>
/// All operations are asynchronous, non-blocking, and return FIO effects, allowing for composable and safe networking code in a purely functional style.
/// </para>
/// </summary>
type FClientWebSocket<'S, 'R, 'E> private (clientWebSocket: ClientWebSocket, onError: exn -> 'E, options: JsonSerializerOptions) =

    // Partially applied functions as it is the same
    // onError function used everywhere in the type
    let ( !<<< ) (func: unit -> 'T) : FIO<'T, 'E> =
        !<<< func onError

    let ( !<<~ ) (task: Task) : FIO<unit, 'E>=
        !<<~ task onError

    let ( !<<~~ ) (task: Task<'T>) : FIO<'T, 'E> =
        !<<~~ task onError

    /// <summary>
    /// Creates a new functional client WebSocket abstraction.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FClientWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FClientWebSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! clientWebSocket = !<<< (fun () -> new ClientWebSocket()) onError
            return FClientWebSocket (clientWebSocket, onError, options)
        }
    
    /// <summary>
    /// Creates a new functional client WebSocket abstraction with default options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FClientWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (options: JsonSerializerOptions) : FIO<FClientWebSocket<'S, 'R, exn>, exn> =
        FClientWebSocket.Create<'S, 'R, exn> (id, options)
    
    /// <summary>
    /// Creates a new functional client WebSocket abstraction with a custom onError function.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>A new FClientWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (onError: exn -> 'E) : FIO<FClientWebSocket<'S, 'R, 'E>, 'E> =
        FClientWebSocket.Create<'S, 'R, 'E> (onError, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional client WebSocket abstraction with default options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>A new FClientWebSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> () : FIO<FClientWebSocket<'S, 'R, exn>, exn> =
        FClientWebSocket.Create<'S, 'R, exn> (id, JsonSerializerOptions())
    
    /// <summary>
    /// Connects the client WebSocket to the specified URL with an optional cancellation token.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="url">The URL to connect to.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that connects the client WebSocket or returns an error of type 'E.</returns>
    member _.Connect<'E> (url, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! uri = !<<< (fun () -> Uri url)
            let! connectTask = !<<< (fun () -> clientWebSocket.ConnectAsync(uri, cancellationToken))
            do! !<<~ connectTask
        }

    /// <summary>
    /// Connects the client WebSocket to the specified URL with no cancellation token.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="url">The URL to connect to.</param>
    /// <returns>An FIO effect that connects the client WebSocket or returns an error of type 'E.</returns>
    member this.Connect<'E> url : FIO<unit, 'E> =
        this.Connect<'E> (url, CancellationToken.None)
        
    /// <summary>
    /// Sends a value of type 'S over the client WebSocket as a JSON-serialized string.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <param name="messageType">The type of message to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that sends the value over the client WebSocket or returns an error of type 'E.</returns>
    member _.Send<'S, 'E> (msg: 'S, messageType: WebSocketMessageType, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! json = !<<< (fun () -> JsonSerializer.Serialize(msg, options))
            let! buffer = !<<< (fun () -> Encoding.UTF8.GetBytes json)
            let! sendTask = !<<< (fun () -> clientWebSocket.SendAsync(ArraySegment buffer, messageType, endOfMessage, cancellationToken))
            do! !<<~ sendTask
        }

    /// <summary>
    /// Sends a value of type 'S over the client WebSocket as a JSON-serialized string with default message type and cancellation.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <param name="endOfMessage">Indicates if this is the last message to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that sends the value over the client WebSocket or returns an error of type 'E.</returns>
    member this.Send<'S, 'E> (msg: 'S, endOfMessage: bool, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send<'S, 'E> (msg, WebSocketMessageType.Text, endOfMessage, cancellationToken)

    /// <summary>
    /// Sends a value of type 'S over the client WebSocket as a JSON-serialized string with default endOfMessage and cancellation.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that sends the value over the client WebSocket or returns an error of type 'E.</returns>
    member this.Send<'S, 'E> (msg: 'S, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Send<'S, 'E> (msg, true, cancellationToken)

    /// <summary>
    /// Sends a value of type 'S over the client WebSocket as a JSON-serialized string with default endOfMessage and no cancellation.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <returns>An FIO effect that sends the value over the client WebSocket or returns an error of type 'E.</returns>
    member this.Send<'S, 'E> (msg: 'S) : FIO<unit, 'E> =
        this.Send<'S, 'E> (msg, true, CancellationToken.None)

    /// <summary>
    /// Receives a value of type 'R from the client WebSocket, deserializing from JSON.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <param name="bufferSize">The size of the buffer to use for receiving.</param>
    /// <returns>An FIO effect that receives a value from the client WebSocket or returns an error of type 'E.</returns>
    member _.Receive<'R, 'E> (cancellationToken: CancellationToken, bufferSize: int) : FIO<'R, 'E> =
        fio {
            let! buffer = !<<< (fun () -> Array.zeroCreate bufferSize)
            let! receiveTask = !<<< (fun () -> clientWebSocket.ReceiveAsync(ArraySegment buffer, cancellationToken))
            let! receiveResult = !<<~~ receiveTask
            let! json = !<<< (fun () -> Encoding.UTF8.GetString(buffer, 0, receiveResult.Count))
            let! msg = !<<< (fun () -> JsonSerializer.Deserialize<'R>(json, options))
            return msg
        }

    /// <summary>
    /// Receives a value of type 'R from the client WebSocket with default buffer size.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that receives a value from the client WebSocket or returns an error of type 'E.</returns>
    member this.Receive<'R, 'E> (cancellationToken: CancellationToken) : FIO<'R, 'E> =
        this.Receive<'R, 'E> (cancellationToken, 1024)
    
    /// <summary>
    /// Receives a value of type 'R from the client WebSocket with default buffer size and no cancellation.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that receives a value from the client WebSocket or returns an error of type 'E.</returns>
    member this.Receive<'R, 'E> () : FIO<'R, 'E> =
        this.Receive<'R, 'E> (CancellationToken.None, 1024)

    /// <summary>
    /// Closes the client WebSocket with the specified status, description and cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that closes the client WebSocket or returns an error of type 'E.</returns>
    member _.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = !<<< (fun () -> clientWebSocket.CloseAsync(closeStatus, statusDescription, cancellationToken))
            do! !<<~ closeTask
        }

    /// <summary>
    /// Closes the client WebSocket with the specified status and description, no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <returns>An FIO effect that closes the client WebSocket or returns an error of type 'E.</returns>
    member this.Close<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.Close<'E> (closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the client WebSocket with normal closure and optional cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that closes the client WebSocket or returns an error of type 'E.</returns>
    member this.Close<'E> (cancellationToken: CancellationToken) : FIO<unit, 'E> =
        this.Close<'E> (WebSocketCloseStatus.NormalClosure, "Normal Closure", cancellationToken)

    /// <summary>
    /// Closes the client WebSocket with normal closure and no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that closes the client WebSocket or returns an error of type 'E.</returns>
    member this.Close<'E> () : FIO<unit, 'E> =
        this.Close<'E> CancellationToken.None

    /// <summary>
    /// Aborts the client WebSocket connection.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that aborts the client WebSocket or returns an error of type 'E.</returns>
    member _.Abort<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> clientWebSocket.Abort())
        }

    /// <summary>
    /// Returns the state of the WebSocket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the WebSocket state or returns an error of type 'E.</returns>
    member _.State<'E> () : FIO<WebSocketState, 'E> =
        fio {
            return! !<<< (fun () -> clientWebSocket.State)
        }

    /// <summary>
    /// Returns the negotiated subprotocol, if any.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the subprotocol or returns an error of type 'E.</returns>
    member _.Subprotocol<'E> () : FIO<string, 'E> =
        fio {
            return! !<<< (fun () -> clientWebSocket.SubProtocol)
        }

    /// <summary>
    /// Closes the outgoing side of the client WebSocket connection with the specified status, description, and cancellation token.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">A cancellation token for the operation.</param>
    /// <returns>An FIO effect that closes the outgoing side of the client WebSocket or returns an error of type 'E.</returns>
    member _.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, 'E> =
        fio {
            let! closeTask = !<<< (fun () -> clientWebSocket.CloseOutputAsync(closeStatus, statusDescription, cancellationToken))
            do! !<<~ closeTask
        }

    /// <summary>
    /// Closes the outgoing side of the client WebSocket connection with the specified status and description, no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="closeStatus">The status to close with.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <returns>An FIO effect that closes the outgoing side of the client WebSocket or returns an error of type 'E.</returns>
    member this.CloseOutput<'E> (closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, 'E> =
        this.CloseOutput(closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the outgoing side of the client WebSocket connection with normal closure, "Normal Closure", and no cancellation.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that closes the outgoing side of the client WebSocket or returns an error of type 'E.</returns>
    member this.CloseOutput<'E> () : FIO<unit, 'E> =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal Closure", CancellationToken.None)

    /// <summary>
    /// Gets the close status of the client WebSocket connection, if closed.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the close status or returns an error of type 'E.</returns>
    member _.CloseStatus<'E> () : FIO<WebSocketCloseStatus option, 'E> =
        fio {
            return! !<<< (fun () -> Option.ofNullable clientWebSocket.CloseStatus)
        }

    /// <summary>
    /// Gets the close status description of the client WebSocket connection.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the close status description or returns an error of type 'E.</returns>
    member _.CloseStatusDescription<'E> () : FIO<string, 'E> =
        fio {
            return! !<<< (fun () -> clientWebSocket.CloseStatusDescription)
        }

    /// <summary>
    /// Disposes the underlying client WebSocket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that disposes the underlying client WebSocket or returns an error of type 'E.</returns>
    member _.Dispose<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> (clientWebSocket :> IDisposable).Dispose())
        }
