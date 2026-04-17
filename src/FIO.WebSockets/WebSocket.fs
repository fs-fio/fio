namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text
open System.Buffers
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// A functional, effectful, and type-safe abstraction over a WebSocket connection.
type WebSocket internal (socket: Net.WebSockets.WebSocket, config: WebSocketConfig) =

    /// Semaphore ensuring thread-safe send operations.
    let sendLock = new SemaphoreSlim(1, 1)
    /// Semaphore ensuring thread-safe receive operations.
    let receiveLock = new SemaphoreSlim(1, 1)

    /// Logs an error and suppresses it for cleanup operations.
    /// <param name="context">The context description for the error.</param>
    /// <param name="err">The error to log.</param>
    /// <returns>Effect that logs the error and succeeds with unit.</returns>
    let logAndSuppress (context: string) (err: WsError) =
        fio {
            let str = err.ToString()
            do! FIO.attempt ((fun () -> eprintfn $"[WebSocket] Error during {context}: {str}"), WsError.fromException)
            return ()
        }

    /// Wraps a function as an FIO effect with WebSocket error handling.
    /// <param name="func">The function to wrap.</param>
    /// <returns>Effect returning the function's result.</returns>
    let fromFunc (func: unit -> 'T) =
        FIO.attempt (func, WsError.fromException)

    /// Wraps a Task as an FIO effect with WebSocket error handling.
    /// <param name="task">The Task to await.</param>
    /// <returns>Effect that completes when the Task finishes.</returns>
    let awaitTask (task: Task) =
        FIO.awaitTask (task, WsError.fromException)

    /// Wraps a generic Task as an FIO effect with WebSocket error handling.
    /// <param name="task">The Task to await.</param>
    /// <returns>Effect returning the Task's result.</returns>
    let awaitTaskT (task: Task<'T>) =
        FIO.awaitGenericTask (task, WsError.fromException)

    /// Receives a complete message from the WebSocket.
    /// <param name="ct">The cancellation token.</param>
    /// <returns>Effect returning the received message.</returns>
    member _.ReceiveMessage(ct: CancellationToken) =
        fio {
            let! state = fromFunc <| fun () -> socket.State

            if state <> WebSocketState.Open && state <> WebSocketState.CloseSent then
                return!
                    FIO.fail (WsError.fromException (Exception $"Cannot receive message - WebSocket state is {state}"))

            let! timeoutCts =
                fromFunc
                <| fun () ->
                    if config.ReceiveTimeout > 0 then
                        new CancellationTokenSource(config.ReceiveTimeout)
                    else
                        new CancellationTokenSource()

            let! linkedCts =
                fromFunc
                <| fun () -> CancellationTokenSource.CreateLinkedTokenSource(ct, timeoutCts.Token)

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
                                    FIO.fail (
                                        WsError.fromException (
                                            Exception
                                                $"Message size {totalSize} exceeds maximum allowed size {config.MaxMessageSize}"
                                        )
                                    )

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
                        | _ -> return! FIO.fail (WsError.fromException (Exception "Unexpected message type"))
                }

            let finalizer =
                fio {
                    do! fromFunc <| fun () -> ArrayPool<byte>.Shared.Return buffer

                    do!
                        fromFunc(fun () -> receiveLock.Release() |> ignore)
                            .CatchAll(logAndSuppress "receiveLock release")

                    do! fromFunc(fun () -> linkedCts.Dispose()).CatchAll(logAndSuppress "linkedCts disposal")
                    do! fromFunc(fun () -> timeoutCts.Dispose()).CatchAll(logAndSuppress "timeoutCts disposal")
                }

            return! computation.Ensuring finalizer
        }

    /// Receives a complete message from the WebSocket.
    /// <returns>Effect returning the received message.</returns>
    member this.ReceiveMessage() =
        this.ReceiveMessage CancellationToken.None

    /// Sends a WebSocket frame.
    /// <param name="frame">The frame to send.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that sends the frame.</returns>
    member _.SendFrame(frame: WebSocketFrame, ct: CancellationToken) =
        fio {
            let! state = fromFunc <| fun () -> socket.State

            if state <> WebSocketState.Open then
                return! FIO.fail (WsError.fromException (Exception $"Cannot send frame - WebSocket state is {state}"))

            let! timeoutCts =
                fromFunc
                <| fun () ->
                    if config.SendTimeout > 0 then
                        new CancellationTokenSource(config.SendTimeout)
                    else
                        new CancellationTokenSource()

            let! linkedCts =
                fromFunc
                <| fun () -> CancellationTokenSource.CreateLinkedTokenSource(ct, timeoutCts.Token)

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
                                let! actualByteCount =
                                    fromFunc <| fun () -> Encoding.UTF8.GetBytes(text, 0, text.Length, buffer, 0)

                                let! sendTask =
                                    fromFunc
                                    <| fun () ->
                                        socket.SendAsync(
                                            ArraySegment(buffer, 0, actualByteCount),
                                            WebSocketMessageType.Text,
                                            true,
                                            effectiveToken
                                        )

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
                        let! closeTask =
                            fromFunc <| fun () -> socket.CloseAsync(status, description, effectiveToken)

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

    /// Sends a WebSocket frame.
    /// <param name="frame">The frame to send.</param>
    /// <returns>Effect that sends the frame.</returns>
    member this.SendFrame(frame: WebSocketFrame) =
        this.SendFrame(frame, CancellationToken.None)

    /// Sends a text frame.
    /// <param name="text">The text to send.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that sends the text frame.</returns>
    member this.SendText(text: string, ct: CancellationToken) = this.SendFrame(Text text, ct)

    /// Sends a text frame.
    /// <param name="text">The text to send.</param>
    /// <returns>Effect that sends the text frame.</returns>
    member this.SendText(text: string) =
        this.SendText(text, CancellationToken.None)

    /// Sends a binary frame.
    /// <param name="data">The binary data to send.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that sends the binary frame.</returns>
    member this.SendBinary(data: byte[], ct: CancellationToken) = this.SendFrame(Binary data, ct)

    /// Sends a binary frame.
    /// <param name="data">The binary data to send.</param>
    /// <returns>Effect that sends the binary frame.</returns>
    member this.SendBinary(data: byte[]) =
        this.SendBinary(data, CancellationToken.None)

    /// Sends a value using a codec.
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that sends the encoded value.</returns>
    member this.Send<'T>(codec: WebSocketCodec<'T>, value: 'T, ct: CancellationToken) =
        fio {
            let! frameResult = codec.Encode(value).CatchAll(fun err -> FIO.fail err)
            do! this.SendFrame(frameResult, ct)
        }

    /// Sends a value using a codec.
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <returns>Effect that sends the encoded value.</returns>
    member this.Send<'T>(codec: WebSocketCodec<'T>, value: 'T) =
        this.Send(codec, value, CancellationToken.None)

    /// Receives and decodes a value using a codec.
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect returning the decoded value.</returns>
    member this.Receive<'T>(codec: WebSocketCodec<'T>, ct: CancellationToken) =
        fio {
            match! this.ReceiveMessage ct with
            | Frame frame -> return! codec.Decode(frame).CatchAll(fun err -> FIO.fail err)
            | ConnectionClosed(status, desc) ->
                return!
                    FIO.fail (
                        WsError.fromException (Exception $"Connection closed. Status: {status}, Description: {desc}")
                    )
        }

    /// Receives and decodes a value using a codec.
    /// <param name="codec">The codec to use for decoding.</param>
    /// <returns>Effect returning the decoded value.</returns>
    member this.Receive<'T>(codec: WebSocketCodec<'T>) =
        this.Receive(codec, CancellationToken.None)

    /// Closes the WebSocket connection with the specified status and description.
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that closes the connection.</returns>
    member _.Close(closeStatus: WebSocketCloseStatus, statusDescription: string, ct: CancellationToken) =
        fio {
            let! sendLockTask = fromFunc <| fun () -> sendLock.WaitAsync ct
            do! awaitTask sendLockTask
            let! receiveLockTask = fromFunc <| fun () -> receiveLock.WaitAsync ct
            do! awaitTask receiveLockTask

            let closeOp =
                fio {
                    let! closeTask =
                        fromFunc <| fun () -> socket.CloseAsync(closeStatus, statusDescription, ct)

                    do! awaitTask closeTask
                }

            let finalizer =
                fio {
                    do! fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")

                    do!
                        fromFunc(fun () -> receiveLock.Release() |> ignore)
                            .CatchAll(logAndSuppress "receiveLock release")
                }

            return! closeOp.Ensuring finalizer
        }

    /// Closes the WebSocket connection with the specified status and description.
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <returns>Effect that closes the connection.</returns>
    member this.Close(closeStatus: WebSocketCloseStatus, statusDescription: string) =
        this.Close(closeStatus, statusDescription, CancellationToken.None)

    /// Closes the WebSocket connection with normal closure.
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that closes the connection.</returns>
    member this.Close(ct: CancellationToken) : FIO<unit, WsError> =
        this.Close(WebSocketCloseStatus.NormalClosure, "Normal closure", ct)

    /// Closes the WebSocket connection with normal closure.
    /// <returns>Effect that closes the connection.</returns>
    member this.Close() = this.Close CancellationToken.None

    /// Closes the outgoing side of the WebSocket connection.
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <param name="ct">Optional cancellation token.</param>
    /// <returns>Effect that closes the outgoing side.</returns>
    member _.CloseOutput(closeStatus: WebSocketCloseStatus, statusDescription: string, ct: CancellationToken) =
        fio {
            let! sendLockTask = fromFunc <| fun () -> sendLock.WaitAsync ct
            do! awaitTask sendLockTask

            let closeOp =
                fio {
                    let! closeTask =
                        fromFunc
                        <| fun () -> socket.CloseOutputAsync(closeStatus, statusDescription, ct)

                    do! awaitTask closeTask
                }

            let finalizer =
                fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")

            return! closeOp.Ensuring finalizer
        }

    /// Closes the outgoing side of the WebSocket connection.
    /// <param name="closeStatus">The WebSocket close status.</param>
    /// <param name="statusDescription">A description for the close status.</param>
    /// <returns>Effect that closes the outgoing side.</returns>
    member this.CloseOutput(closeStatus: WebSocketCloseStatus, statusDescription: string) =
        this.CloseOutput(closeStatus, statusDescription, CancellationToken.None)

    /// Closes the outgoing side with normal closure.
    /// <returns>Effect that closes the outgoing side.</returns>
    member this.CloseOutput() =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal closure", CancellationToken.None)

    /// Aborts the WebSocket connection immediately.
    /// <returns>Effect that aborts the connection.</returns>
    member _.Abort() =
        fio { do! fromFunc <| fun () -> socket.Abort() }

    /// Gets the current state of the WebSocket connection.
    /// <returns>Effect returning the WebSocket state.</returns>
    member _.State() =
        fio { return! fromFunc <| fun () -> socket.State }

    /// Gets the close status if the connection is closed.
    /// <returns>Effect returning the close status, or None.</returns>
    member _.CloseStatus() =
        fio { return! fromFunc <| fun () -> Option.ofNullable socket.CloseStatus }

    /// Gets the close status description if the connection is closed.
    /// <returns>Effect returning the close status description.</returns>
    member _.CloseStatusDescription() =
        fio { return! fromFunc <| fun () -> socket.CloseStatusDescription }

    /// Gets the negotiated subprotocol, if any.
    /// <returns>Effect returning the negotiated subprotocol.</returns>
    member _.Subprotocol() =
        fio { return! fromFunc <| fun () -> socket.SubProtocol }

    /// Disposes the underlying WebSocket and releases all resources.
    /// This is a synchronous FIO effect wrapper for cleanup.
    /// Note: Prefer using Close() in FIO code for proper async cleanup.
    /// <returns>Effect that disposes all resources.</returns>
    member _.Dispose() =
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
