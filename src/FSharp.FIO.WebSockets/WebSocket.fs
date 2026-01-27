namespace FSharp.FIO.WebSockets

open FSharp.FIO.DSL

open System
open System.Text
open System.Buffers
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// <summary>
/// A functional, effectful, and type-safe abstraction over a WebSocket connection.
/// </summary>
type WebSocket internal (socket: Net.WebSockets.WebSocket, config: WebSocketConfig) =

    // Semaphores for thread-safe concurrent operations
    let sendLock = new SemaphoreSlim(1, 1)
    let receiveLock = new SemaphoreSlim(1, 1)

    // Internal: Logs an error and suppresses it (used for cleanup)
    let logAndSuppress (context: string) (err: WsError) : FIO<unit, WsError> =
        fio {
            let str = err.ToString()
            do! FIO.attempt((fun () ->
                eprintfn $"[WebSocket] Error during {context}: {str}"), WsError.fromException)
            return ()
        }

    // Partially applied functions for consistent error handling
    let fromFunc (func: unit -> 'T) : FIO<'T, WsError> =
        FIO.attempt(func, WsError.fromException)

    let awaitTask (task: Task) : FIO<unit, WsError> =
        FIO.awaitTask(task, WsError.fromException)

    let awaitTaskT (task: Task<'T>) : FIO<'T, WsError> =
        FIO.awaitGenericTask(task, WsError.fromException)

    /// <summary>
    /// Receives a complete message from the WebSocket.
    /// </summary>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.ReceiveMessage(cancellationToken: CancellationToken) : FIO<WebSocketMessage, WsError> =
        fio {
            let! state = fromFunc <| fun () -> socket.State

            if state <> WebSocketState.Open && state <> WebSocketState.CloseSent then
                return! FIO.fail(WsError.fromException (Exception $"Cannot receive message - WebSocket state is {state}"))

            let! timeoutCts =
                fromFunc
                <| fun () ->
                    if config.ReceiveTimeout > 0 then
                        new CancellationTokenSource(config.ReceiveTimeout)
                    else
                        new CancellationTokenSource()

            let! linkedCts =
                fromFunc
                <| fun () -> CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeoutCts.Token)

            let effectiveToken = linkedCts.Token

            let! lockTask = fromFunc <| fun () -> receiveLock.WaitAsync effectiveToken
            do! awaitTask lockTask

            let bufferSize = config.ReceiveBufferSize
            let buffer = ArrayPool<byte>.Shared.Rent bufferSize

            let computation =
                fio {
                    let fragments = ResizeArray<byte>()
                    let mutable endOfMessage = false
                    let mutable messageType = WebSocketMessageType.Text
                    let mutable isCloseFrame = false
                    let mutable totalSize = 0L

                    while not endOfMessage do
                        do! fromFunc <| fun () -> effectiveToken.ThrowIfCancellationRequested()

                        let! receiveTask =
                            fromFunc
                            <| fun () -> socket.ReceiveAsync(ArraySegment(buffer, 0, bufferSize), effectiveToken)

                        let! receiveResult = awaitTaskT receiveTask

                        messageType <- receiveResult.MessageType
                        endOfMessage <- receiveResult.EndOfMessage

                        if messageType = WebSocketMessageType.Close then
                            isCloseFrame <- true
                            endOfMessage <- true
                        else
                            let count = receiveResult.Count
                            totalSize <- totalSize + int64 count

                            if totalSize > config.MaxMessageSize then
                                return!
                                    FIO.fail(WsError.fromException (
                                        Exception $"Message size {totalSize} exceeds maximum allowed size {config.MaxMessageSize}"))

                            fragments.AddRange(ArraySegment(buffer, 0, count))

                    if isCloseFrame then
                        let status = Option.ofNullable socket.CloseStatus
                        let desc = socket.CloseStatusDescription
                        return ConnectionClosed(status, desc)
                    else
                        let data = fragments.ToArray()

                        match messageType with
                        | WebSocketMessageType.Text ->
                            let text = Encoding.UTF8.GetString data
                            return Frame(Text text)
                        | WebSocketMessageType.Binary -> return Frame(Binary data)
                        | _ -> return! FIO.fail(WsError.fromException (Exception "Unexpected message type"))
                }

            let finalizer =
                fio {
                    do! fromFunc <| fun () -> ArrayPool<byte>.Shared.Return buffer
                    do! fromFunc(fun () -> receiveLock.Release() |> ignore).CatchAll(logAndSuppress "receiveLock release")
                    do! fromFunc(fun () -> linkedCts.Dispose()).CatchAll(logAndSuppress "linkedCts disposal")
                    do! fromFunc(fun () -> timeoutCts.Dispose()).CatchAll(logAndSuppress "timeoutCts disposal")
                }

            return! computation.Ensuring finalizer
        }

    /// <summary>
    /// Receives a complete message from the WebSocket.
    /// </summary>
    member this.ReceiveMessage() : FIO<WebSocketMessage, WsError> =
        this.ReceiveMessage CancellationToken.None

    /// <summary>
    /// Sends a WebSocket frame.
    /// </summary>
    /// <param name="frame">The frame to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.SendFrame(frame: WebSocketFrame, cancellationToken: CancellationToken) : FIO<unit, WsError> =
        fio {
            let! state = fromFunc <| fun () -> socket.State

            if state <> WebSocketState.Open then
                return! FIO.fail(WsError.fromException (Exception $"Cannot send frame - WebSocket state is {state}"))

            let! timeoutCts = fromFunc <| fun () ->
                if config.SendTimeout > 0 then
                    new CancellationTokenSource(config.SendTimeout)
                else
                    new CancellationTokenSource()

            let! linkedCts = fromFunc <| fun () ->
                CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeoutCts.Token)

            let effectiveToken = linkedCts.Token

            let! lockTask = fromFunc <| fun () -> sendLock.WaitAsync effectiveToken
            do! awaitTask lockTask

            let computation =
                fio {
                    match frame with
                    | Text text ->
                        let maxByteCount = Encoding.UTF8.GetMaxByteCount text.Length
                        let buffer = ArrayPool<byte>.Shared.Rent maxByteCount
                        let sendOp =
                            fio {
                                let! actualByteCount = fromFunc <| fun () ->
                                    Encoding.UTF8.GetBytes(text, 0, text.Length, buffer, 0)

                                let! sendTask = fromFunc <| fun () ->
                                    socket.SendAsync(
                                        ArraySegment(buffer, 0, actualByteCount),
                                        WebSocketMessageType.Text,
                                        true,
                                        effectiveToken)

                                do! awaitTask sendTask
                            }
                        let returnBuffer = fromFunc <| fun () -> ArrayPool<byte>.Shared.Return buffer
                        do! sendOp.Ensuring returnBuffer

                    | Binary data ->
                        let! sendTask =
                            fromFunc
                            <| fun () ->
                                socket.SendAsync(ArraySegment data, WebSocketMessageType.Binary, true, effectiveToken)
                        do! awaitTask sendTask

                    | Close(status, description) ->
                        let! closeTask = fromFunc <| fun () -> socket.CloseAsync(status, description, effectiveToken)
                        do! awaitTask closeTask
                }

            let finalizer =
                fio {
                    do! fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")
                    do! fromFunc(fun () -> linkedCts.Dispose()).CatchAll(logAndSuppress "linkedCts disposal")
                    do! fromFunc(fun () -> timeoutCts.Dispose()).CatchAll(logAndSuppress "timeoutCts disposal")
                }

            return! computation.Ensuring finalizer
        }

    /// <summary>
    /// Sends a WebSocket frame.
    /// </summary>
    /// <param name="frame">The frame to send.</param>
    member this.SendFrame(frame: WebSocketFrame) : FIO<unit, WsError> =
        this.SendFrame(frame, CancellationToken.None)

    /// <summary>
    /// Sends a text frame.
    /// </summary>
    /// <param name="text">The text to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.SendText(text: string, cancellationToken: CancellationToken) : FIO<unit, WsError> =
        this.SendFrame(Text text, cancellationToken)

    /// <summary>
    /// Sends a text frame.
    /// </summary>
    /// <param name="text">The text to send.</param>
    member this.SendText(text: string) : FIO<unit, WsError> =
        this.SendText(text, CancellationToken.None)

    /// <summary>
    /// Sends a binary frame.
    /// </summary>
    /// <param name="data">The binary data to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.SendBinary(data: byte[], cancellationToken: CancellationToken) : FIO<unit, WsError> =
        this.SendFrame(Binary data, cancellationToken)

    /// <summary>
    /// Sends a binary frame.
    /// </summary>
    /// <param name="data">The binary data to send.</param>
    member this.SendBinary(data: byte[]) : FIO<unit, WsError> =
        this.SendBinary(data, CancellationToken.None)

    /// <summary>
    /// Sends a value using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.Send<'T>(codec: WebSocketCodec<'T>, value: 'T, cancellationToken: CancellationToken) : FIO<unit, WsError> =
        fio {
            let! frameResult =
                (codec.Encode value)
                    .CatchAll(fun err -> FIO.fail(err))
            do! this.SendFrame(frameResult, cancellationToken)
        }

    /// <summary>
    /// Sends a value using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    member this.Send<'T>(codec: WebSocketCodec<'T>, value: 'T) : FIO<unit, WsError> =
        this.Send(codec, value, CancellationToken.None)

    /// <summary>
    /// Receives and decodes a value using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.Receive<'T>(codec: WebSocketCodec<'T>, cancellationToken: CancellationToken) : FIO<'T, WsError> =
        fio {
            match! this.ReceiveMessage cancellationToken with
            | Frame frame ->
                return! codec.Decode(frame)
                    .CatchAll(fun err -> FIO.fail err)
            | ConnectionClosed(status, desc) ->
                return! FIO.fail(WsError.fromException (Exception $"Connection closed. Status: {status}, Description: {desc}"))
        }

    /// <summary>
    /// Receives and decodes a value using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    member this.Receive<'T>(codec: WebSocketCodec<'T>) : FIO<'T, WsError> =
        this.Receive(codec, CancellationToken.None)

    /// <summary>
    /// Closes the WebSocket connection with the specified status and description.
    /// </summary>
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.Close(closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, WsError> =
        fio {
            let! sendLockTask = fromFunc <| fun () -> sendLock.WaitAsync cancellationToken
            do! awaitTask sendLockTask
            let! receiveLockTask = fromFunc <| fun () -> receiveLock.WaitAsync cancellationToken
            do! awaitTask receiveLockTask

            let closeOp =
                fio {
                    let! closeTask =
                        fromFunc
                        <| fun () -> socket.CloseAsync(closeStatus, statusDescription, cancellationToken)

                    do! awaitTask closeTask
                }

            let finalizer =
                fio {
                    do! fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")
                    do! fromFunc(fun () -> receiveLock.Release() |> ignore).CatchAll(logAndSuppress "receiveLock release")
                }

            return! closeOp.Ensuring finalizer
        }

    /// <summary>
    /// Closes the WebSocket connection with the specified status and description.
    /// </summary>
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    member this.Close(closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, WsError> =
        this.Close(closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the WebSocket connection with normal closure.
    /// </summary>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member this.Close(cancellationToken: CancellationToken) : FIO<unit, WsError> =
        this.Close(WebSocketCloseStatus.NormalClosure, "Normal closure", cancellationToken)

    /// <summary>
    /// Closes the WebSocket connection with normal closure.
    /// </summary>
    member this.Close() : FIO<unit, WsError> =
        this.Close CancellationToken.None

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection.
    /// </summary>
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="cancellationToken">Optional cancellation token.</param>
    member _.CloseOutput(closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) : FIO<unit, WsError> =
        fio {
            let! sendLockTask = fromFunc <| fun () -> sendLock.WaitAsync cancellationToken
            do! awaitTask sendLockTask

            let closeOp =
                fio {
                    let! closeTask =
                        fromFunc
                        <| fun () -> socket.CloseOutputAsync(closeStatus, statusDescription, cancellationToken)

                    do! awaitTask closeTask
                }

            let finalizer = fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")
            return! closeOp.Ensuring finalizer
        }

    /// <summary>
    /// Closes the outgoing side of the WebSocket connection.
    /// </summary>
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    member this.CloseOutput(closeStatus: WebSocketCloseStatus, statusDescription: string) : FIO<unit, WsError> =
        this.CloseOutput(closeStatus, statusDescription, CancellationToken.None)

    /// <summary>
    /// Closes the outgoing side with normal closure.
    /// </summary>
    member this.CloseOutput() : FIO<unit, WsError> =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal closure", CancellationToken.None)

    /// <summary>
    /// Aborts the WebSocket connection immediately.
    /// </summary>
    member _.Abort() : FIO<unit, WsError> =
        fio {
            do! fromFunc <| fun () -> socket.Abort()
        }

    /// <summary>
    /// Gets the current state of the WebSocket connection.
    /// </summary>
    member _.State() : FIO<WebSocketState, WsError> =
        fio {
            return! fromFunc <| fun () -> socket.State
        }

    /// <summary>
    /// Gets the close status if the connection is closed.
    /// </summary>
    member _.CloseStatus() : FIO<WebSocketCloseStatus option, WsError> =
        fio {
            return! fromFunc <| fun () -> Option.ofNullable socket.CloseStatus
        }

    /// <summary>
    /// Gets the close status description if the connection is closed.
    /// </summary>
    member _.CloseStatusDescription() : FIO<string, WsError> =
        fio {
            return! fromFunc <| fun () -> socket.CloseStatusDescription
        }

    /// <summary>
    /// Gets the negotiated subprotocol, if any.
    /// </summary>
    member _.Subprotocol() : FIO<string, WsError> =
        fio {
            return! fromFunc <| fun () -> socket.SubProtocol
        }

    /// <summary>
    /// Disposes the underlying WebSocket and releases all resources.
    /// This is a synchronous FIO effect wrapper for cleanup.
    /// Note: Prefer using Close() in FIO code for proper async cleanup.
    /// </summary>
    member _.Dispose() : FIO<unit, WsError> =
        fio {
            do! fromFunc(fun () -> socket.Dispose()).CatchAll(logAndSuppress "socket disposal")
            do! fromFunc(fun () -> sendLock.Dispose()).CatchAll(logAndSuppress "sendLock disposal")
            do! fromFunc(fun () -> receiveLock.Dispose()).CatchAll(logAndSuppress "receiveLock disposal")
        }

    interface IDisposable with
        member _.Dispose() =
            try
                socket.Dispose()
                sendLock.Dispose()
                receiveLock.Dispose()
            with exn ->
                eprintfn $"[WebSocket] Error during IDisposable.Dispose: {exn.Message}"
