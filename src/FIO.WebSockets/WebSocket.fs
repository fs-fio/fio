namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text
open System.Buffers
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// <summary>Represents a type-safe, effectful abstraction over a WebSocket connection.</summary>
type WebSocket internal (socket: Net.WebSockets.WebSocket, config: WebSocketConfig) =

    /// <summary>Represents the semaphore serializing concurrent send operations.</summary>
    let sendLock = new SemaphoreSlim(1, 1)
    /// <summary>Represents the semaphore serializing concurrent receive operations.</summary>
    let receiveLock = new SemaphoreSlim(1, 1)

    /// <summary>Transforms a WebSocket error into a logged-and-suppressed unit effect for use in cleanup paths.</summary>
    /// <param name="context">A description of the operation being cleaned up.</param>
    /// <param name="error">The WebSocket error to log.</param>
    /// <returns>An effect that logs the error to standard error and succeeds with unit.</returns>
    let logAndSuppress (context: string) (error: WsError) =
        fio {
            let str = error.ToString()
            do! FIO.attempt (fun () -> eprintfn $"[WebSocket] Error during {context}: {str}") WsError.fromException
            return ()
        }

    /// <summary>Lifts a synchronous function into a WebSocket effect, mapping exceptions to <c>WsError</c>.</summary>
    /// <param name="func">The synchronous function to execute.</param>
    /// <returns>An effect that produces the function's result or fails with a <c>WsError</c>.</returns>
    let fromFunc (func: unit -> 'T) =
        FIO.attempt func WsError.fromException

    /// <summary>Lifts a unit-returning .NET Task into a WebSocket effect, mapping exceptions to <c>WsError</c>.</summary>
    /// <param name="task">The task to await.</param>
    /// <returns>An effect that completes when the task finishes or fails with a <c>WsError</c>.</returns>
    let awaitUnitTask (task: Task) =
        FIO.awaitUnitTask task WsError.fromException

    /// <summary>Lifts a <c>Task&lt;'T&gt;</c> into a WebSocket effect, mapping exceptions to <c>WsError</c>.</summary>
    /// <param name="task">The task to await.</param>
    /// <returns>An effect that produces the task's result or fails with a <c>WsError</c>.</returns>
    let awaitTask (task: Task<'T>) =
        FIO.awaitTask task WsError.fromException

    /// <summary>Creates an effect that receives a complete message from the connection.</summary>
    /// <param name="ct">The cancellation token to observe during the receive operation.</param>
    /// <returns>An effect that produces the received <c>WebSocketMessage</c>.</returns>
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
            do! awaitUnitTask lockTask

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

                        let! receiveResult = awaitTask receiveTask

                        messageType <- receiveResult.MessageType
                        endOfMessage <- receiveResult.EndOfMessage

                        if messageType = WebSocketMessageType.Close then
                            isCloseFrame <- true
                            endOfMessage <- true
                        else
                            let count = receiveResult.Count
                            totalSize <- totalSize + int64 count

                            if totalSize > config.MaxMessageSize then
                                return! FIO.fail (MessageTooLarge(totalSize, config.MaxMessageSize))

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

            let remapTimeout (error: WsError) =
                if timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"Receive operation timed out after {config.ReceiveTimeout}ms")
                else
                    FIO.fail error

            return! (computation.CatchAll remapTimeout).Ensuring finalizer
        }

    /// <summary>Creates an effect that receives a complete message from the connection, observing the running fiber's cancellation token.</summary>
    /// <returns>An effect that produces the received <c>WebSocketMessage</c>.</returns>
    /// <remarks>The fiber's cancellation token is obtained automatically and linked with the configured receive timeout. To pass a custom token instead, use the overload that takes a <c>CancellationToken</c>.</remarks>
    member this.ReceiveMessage() =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.ReceiveMessage ct
        }

    /// <summary>Creates an effect that sends a frame over the connection.</summary>
    /// <param name="frame">The WebSocket frame to send.</param>
    /// <param name="ct">The cancellation token to observe during the send operation.</param>
    /// <returns>An effect that completes when the frame has been sent.</returns>
    member _.SendFrame(frame: WebSocketFrame, ct: CancellationToken) =
        fio {
            let! state = fromFunc <| fun () -> socket.State

            let canSend =
                match frame with
                | Close _ -> state = WebSocketState.Open || state = WebSocketState.CloseReceived
                | _ -> state = WebSocketState.Open

            if not canSend then
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
            do! awaitUnitTask lockTask

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

                                do! awaitUnitTask sendTask
                            }

                        let returnBuffer = fromFunc <| fun () -> ArrayPool<byte>.Shared.Return buffer
                        do! sendOp.Ensuring returnBuffer

                    | Binary data ->
                        let! sendTask =
                            fromFunc
                            <| fun () ->
                                socket.SendAsync(ArraySegment data, WebSocketMessageType.Binary, true, effectiveToken)

                        do! awaitUnitTask sendTask

                    | Close(status, description) ->
                        let! closeTask =
                            fromFunc <| fun () -> socket.CloseAsync(status, description, effectiveToken)

                        do! awaitUnitTask closeTask
                }

            let finalizer =
                fio {
                    do! fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")
                    do! fromFunc(fun () -> linkedCts.Dispose()).CatchAll(logAndSuppress "linkedCts disposal")
                    do! fromFunc(fun () -> timeoutCts.Dispose()).CatchAll(logAndSuppress "timeoutCts disposal")
                }

            let remapTimeout (error: WsError) =
                if timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"Send operation timed out after {config.SendTimeout}ms")
                else
                    FIO.fail error

            return! (computation.CatchAll remapTimeout).Ensuring finalizer
        }

    /// <summary>Creates an effect that sends a frame over the connection, observing the running fiber's cancellation token.</summary>
    /// <param name="frame">The WebSocket frame to send.</param>
    /// <returns>An effect that completes when the frame has been sent.</returns>
    /// <remarks>The fiber's cancellation token is obtained automatically and linked with the configured send timeout.</remarks>
    member this.SendFrame(frame: WebSocketFrame) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.SendFrame(frame, ct)
        }

    /// <summary>Creates an effect that sends a text frame over the connection.</summary>
    /// <param name="text">The UTF-8 string to send.</param>
    /// <param name="ct">The cancellation token to observe during the send operation.</param>
    /// <returns>An effect that completes when the text frame has been sent.</returns>
    member this.SendText(text: string, ct: CancellationToken) = this.SendFrame(Text text, ct)

    /// <summary>Creates an effect that sends a text frame over the connection, observing the running fiber's cancellation token.</summary>
    /// <param name="text">The UTF-8 string to send.</param>
    /// <returns>An effect that completes when the text frame has been sent.</returns>
    member this.SendText(text: string) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.SendText(text, ct)
        }

    /// <summary>Creates an effect that sends a binary frame over the connection.</summary>
    /// <param name="data">The byte array to send.</param>
    /// <param name="ct">The cancellation token to observe during the send operation.</param>
    /// <returns>An effect that completes when the binary frame has been sent.</returns>
    member this.SendBinary(data: byte[], ct: CancellationToken) = this.SendFrame(Binary data, ct)

    /// <summary>Creates an effect that sends a binary frame over the connection, observing the running fiber's cancellation token.</summary>
    /// <param name="data">The byte array to send.</param>
    /// <returns>An effect that completes when the binary frame has been sent.</returns>
    member this.SendBinary(data: byte[]) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.SendBinary(data, ct)
        }

    /// <summary>Creates an effect that encodes a value with the specified codec and sends it over the connection.</summary>
    /// <param name="codec">The codec to use for encoding the value into a frame.</param>
    /// <param name="value">The value to encode and send.</param>
    /// <param name="ct">The cancellation token to observe during the send operation.</param>
    /// <returns>An effect that completes when the encoded value has been sent.</returns>
    member this.Send<'T>(codec: WebSocketCodec<'T>, value: 'T, ct: CancellationToken) =
        fio {
            let! frameResult = codec.Encode value
            do! this.SendFrame(frameResult, ct)
        }

    /// <summary>Creates an effect that encodes a value with the specified codec and sends it over the connection, observing the running fiber's cancellation token.</summary>
    /// <param name="codec">The codec to use for encoding the value into a frame.</param>
    /// <param name="value">The value to encode and send.</param>
    /// <returns>An effect that completes when the encoded value has been sent.</returns>
    member this.Send<'T>(codec: WebSocketCodec<'T>, value: 'T) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.Send(codec, value, ct)
        }

    /// <summary>Creates an effect that receives a frame and decodes it using the specified codec.</summary>
    /// <param name="codec">The codec to use for decoding the received frame.</param>
    /// <param name="ct">The cancellation token to observe during the receive operation.</param>
    /// <returns>An effect that produces the decoded value, or fails if the connection is closed.</returns>
    member this.Receive<'T>(codec: WebSocketCodec<'T>, ct: CancellationToken) =
        fio {
            match! this.ReceiveMessage ct with
            | Frame frame -> return! codec.Decode frame
            | ConnectionClosed(status, desc) ->
                return!
                    FIO.fail (
                        WsError.fromException (Exception $"Connection closed. Status: {status}, Description: {desc}")
                    )
        }

    /// <summary>Creates an effect that receives a frame and decodes it using the specified codec, observing the running fiber's cancellation token.</summary>
    /// <param name="codec">The codec to use for decoding the received frame.</param>
    /// <returns>An effect that produces the decoded value, or fails if the connection is closed.</returns>
    member this.Receive<'T>(codec: WebSocketCodec<'T>) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.Receive(codec, ct)
        }

    /// <summary>Creates an effect that gracefully closes the connection with the specified status and description.</summary>
    /// <param name="closeStatus">The status code for the close handshake.</param>
    /// <param name="statusDescription">A human-readable description of the close reason.</param>
    /// <param name="ct">The cancellation token to observe during the close operation.</param>
    /// <returns>An effect that completes when the close handshake finishes.</returns>
    member _.Close(closeStatus: WebSocketCloseStatus, statusDescription: string, ct: CancellationToken) =
        fio {
            let! sendLockTask = fromFunc <| fun () -> sendLock.WaitAsync ct
            do! awaitUnitTask sendLockTask

            let! hasReceiveLock = fromFunc <| fun () -> receiveLock.Wait 0

            let closeOp =
                fio {
                    let! closeTask =
                        fromFunc
                        <| fun () ->
                            if hasReceiveLock then
                                socket.CloseAsync(closeStatus, statusDescription, ct)
                            else
                                socket.CloseOutputAsync(closeStatus, statusDescription, ct)

                    do! awaitUnitTask closeTask
                }

            let finalizer =
                fio {
                    do! fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")

                    if hasReceiveLock then
                        do!
                            fromFunc(fun () -> receiveLock.Release() |> ignore)
                                .CatchAll(logAndSuppress "receiveLock release")
                }

            return! closeOp.Ensuring finalizer
        }

    /// <summary>Creates an effect that gracefully closes the connection with the specified status and description, observing the running fiber's cancellation token.</summary>
    /// <param name="closeStatus">The status code for the close handshake.</param>
    /// <param name="statusDescription">A human-readable description of the close reason.</param>
    /// <returns>An effect that completes when the close handshake finishes.</returns>
    member this.Close(closeStatus: WebSocketCloseStatus, statusDescription: string) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.Close(closeStatus, statusDescription, ct)
        }

    /// <summary>Creates an effect that gracefully closes the connection with normal closure status.</summary>
    /// <param name="ct">The cancellation token to observe during the close operation.</param>
    /// <returns>An effect that completes when the close handshake finishes.</returns>
    member this.Close(ct: CancellationToken) : FIO<unit, WsError> =
        this.Close(WebSocketCloseStatus.NormalClosure, "Normal closure", ct)

    /// <summary>Creates an effect that gracefully closes the connection with normal closure status, observing the running fiber's cancellation token.</summary>
    /// <returns>An effect that completes when the close handshake finishes.</returns>
    member this.Close() : FIO<unit, WsError> =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.Close ct
        }

    /// <summary>Creates an effect that closes the outgoing side of the connection with the specified status and description.</summary>
    /// <param name="closeStatus">The status code for the outgoing close.</param>
    /// <param name="statusDescription">A human-readable description of the close reason.</param>
    /// <param name="ct">The cancellation token to observe during the close operation.</param>
    /// <returns>An effect that completes when the outgoing side has been closed.</returns>
    member _.CloseOutput(closeStatus: WebSocketCloseStatus, statusDescription: string, ct: CancellationToken) =
        fio {
            let! sendLockTask = fromFunc <| fun () -> sendLock.WaitAsync ct
            do! awaitUnitTask sendLockTask

            let closeOp =
                fio {
                    let! closeTask =
                        fromFunc
                        <| fun () -> socket.CloseOutputAsync(closeStatus, statusDescription, ct)

                    do! awaitUnitTask closeTask
                }

            let finalizer =
                fromFunc(fun () -> sendLock.Release() |> ignore).CatchAll(logAndSuppress "sendLock release")

            return! closeOp.Ensuring finalizer
        }

    /// <summary>Creates an effect that closes the outgoing side of the connection with the specified status and description, observing the running fiber's cancellation token.</summary>
    /// <param name="closeStatus">The status code for the outgoing close.</param>
    /// <param name="statusDescription">A human-readable description of the close reason.</param>
    /// <returns>An effect that completes when the outgoing side has been closed.</returns>
    member this.CloseOutput(closeStatus: WebSocketCloseStatus, statusDescription: string) =
        fio {
            let! ct = FIO.cancellationToken ()
            return! this.CloseOutput(closeStatus, statusDescription, ct)
        }

    /// <summary>Creates an effect that closes the outgoing side of the connection with normal closure status, observing the running fiber's cancellation token.</summary>
    /// <returns>An effect that completes when the outgoing side has been closed.</returns>
    member this.CloseOutput() =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal closure")

    /// <summary>Creates an effect that aborts the connection immediately without a close handshake.</summary>
    /// <returns>An effect that completes when the connection has been aborted.</returns>
    member _.Abort() =
        fio { do! fromFunc <| fun () -> socket.Abort() }

    /// <summary>Returns an effect that produces the current connection state.</summary>
    /// <returns>An effect that produces the current <c>WebSocketState</c>.</returns>
    member _.State() =
        fio { return! fromFunc <| fun () -> socket.State }

    /// <summary>Returns an effect that produces the close status if the connection has been closed.</summary>
    /// <returns>An effect that produces <c>Some status</c> when closed, or <c>None</c> when still open.</returns>
    member _.CloseStatus() =
        fio { return! fromFunc <| fun () -> Option.ofNullable socket.CloseStatus }

    /// <summary>Returns an effect that produces the close status description if the connection has been closed.</summary>
    /// <returns>An effect that produces the description string associated with the close status.</returns>
    member _.CloseStatusDescription() =
        fio { return! fromFunc <| fun () -> socket.CloseStatusDescription }

    /// <summary>Returns an effect that produces the negotiated subprotocol, if any.</summary>
    /// <returns>An effect that produces the subprotocol string, or null when none was negotiated.</returns>
    member _.Subprotocol() =
        fio { return! fromFunc <| fun () -> socket.SubProtocol }

    /// <summary>Creates an effect that releases all resources associated with the connection.</summary>
    /// <returns>An effect that completes when all resources have been released.</returns>
    /// <remarks>Prefer <c>Close</c> in effect-based code for a graceful shutdown with a close handshake.</remarks>
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
