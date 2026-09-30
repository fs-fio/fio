namespace FIO.WebSockets

open FIO.DSL

open System
open System.Net
open System.Text
open System.Buffers
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// An open WebSocket connection for sending and receiving typed messages.
type WebSocket
    internal
    (
        socket: Net.WebSockets.WebSocket,
        config: WebSocketConfig,
        remoteEndPoint: EndPoint option,
        localEndPoint: EndPoint option
    ) =

    let sendLock = new SemaphoreSlim(1, 1)

    let receiveLock = new SemaphoreSlim(1, 1)

    let logAndSuppress (context: string) (error: WsError) =
        fio {
            let str = error.ToString()
            do! FIO.attempt (fun () ->
                eprintfn $"WebSocket encountered error during {context}: {str}") WsError.fromException
            return ()
        }

    let attempt (func: unit -> 'A) =
        FIO.attempt func WsError.fromException

    // Finalizers run once per message, so each is one action that attempts every release in turn and
    // logs a failure the way logAndSuppress does.
    let releaseLogged (context: string) (release: unit -> unit) =
        try
            release ()
        with ex ->
            try eprintfn $"WebSocket encountered error during {context}: {WsError.fromException ex}"
            with _ -> ()

    // After an abort the exception varies; callers need Closed regardless.
    let closedOr (classify: exn -> WsError) (ex: exn) =
        let state =
            try socket.State
            with _ -> WebSocketState.None

        match state with
        | WebSocketState.Closed
        | WebSocketState.Aborted -> Closed ex.Message
        | _ -> classify ex

    let receiveError = closedOr WsError.receiveFailed

    let sendError = closedOr WsError.sendFailed

    let stateError (state: WebSocketState) (closedStates: WebSocketState list) (otherwise: string -> WsError) (operation: string) =
        let message = $"Cannot {operation} - WebSocket state is {state}"
        if List.contains state closedStates then Closed message else otherwise message

    // Releases a semaphore permit exactly when the wait actually granted one. A wait that ends
    // cancelled never took a permit; a wait granted after this fiber has already given up must still
    // hand it back, or the connection's lock stays held for the life of the socket.
    let releasePermitWhenGranted (semaphore: SemaphoreSlim) (permit: Task) =
        if permit.IsCompletedSuccessfully then
            try semaphore.Release() |> ignore
            with _ -> ()
        else
            permit.ContinueWith(
                (fun (completed: Task) ->
                    if completed.IsCompletedSuccessfully then
                        try semaphore.Release() |> ignore
                        with _ -> ()),
                TaskContinuationOptions.ExecuteSynchronously)
            |> ignore

    // The closing handshake waits for the peer's close frame, so like a send it must be bounded: a
    // peer that has stopped reading would otherwise hold a finalizer, and with it an app's shutdown.
    // The sources are created uninterruptibly by acquireReleaseWith, so an interruption cannot strand them.
    let boundedBySendTimeout (cancellationToken: CancellationToken) (label: string) (operation: CancellationToken -> FIO<unit, WsError>) =
        let setup =
            attempt <| fun () ->
                let timeoutCts: CancellationTokenSource =
                    if config.SendTimeout > 0 then new CancellationTokenSource(config.SendTimeout)
                    else null

                let linkedCts: CancellationTokenSource =
                    if isNull timeoutCts then null
                    else CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeoutCts.Token)

                struct (timeoutCts, linkedCts)

        let release struct (timeoutCts: CancellationTokenSource, linkedCts: CancellationTokenSource) =
            FIO.succeedWith <| fun () ->
                releaseLogged "linkedCts disposal" (fun () -> if not (isNull linkedCts) then linkedCts.Dispose())
                releaseLogged "timeoutCts disposal" (fun () -> if not (isNull timeoutCts) then timeoutCts.Dispose())

        let run struct (timeoutCts: CancellationTokenSource, linkedCts: CancellationTokenSource) =
            let remapTimeout (error: WsError) =
                if not (isNull timeoutCts) && timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"{label} timed out after {config.SendTimeout}ms")
                else
                    FIO.fail error

            let effectiveToken = if isNull linkedCts then cancellationToken else linkedCts.Token
            (operation effectiveToken).CatchAll remapTimeout

        FIO.acquireReleaseWith setup release run

    /// Receives the next complete message, using the given cancellation token; a close frame from the peer is
    /// yielded as <c>ConnectionClosed</c>. Interrupting a pending receive aborts the connection.
    member _.ReceiveMessage (cancellationToken: CancellationToken) =
        let bufferSize = config.ReceiveBufferSize

        // One setup action per message, run uninterruptibly by acquireReleaseWith, so the lock wait, the token
        // sources and the pooled buffer always reach the release. Without a receive timeout the fiber's token is
        // the only one, so no source is needed. Fragments are received into one pooled array that grows as
        // needed, so a message is decoded straight from it; the release returns whichever array is current.
        let setup =
            (attempt <| fun () ->
                let state = socket.State

                if state <> WebSocketState.Open && state <> WebSocketState.CloseSent then
                    Error(
                        stateError
                            state
                            [ WebSocketState.Closed; WebSocketState.Aborted; WebSocketState.CloseReceived ]
                            ReceiveFailed
                            "receive message")
                else
                    let timeoutCts: CancellationTokenSource =
                        if config.ReceiveTimeout > 0 then new CancellationTokenSource(config.ReceiveTimeout)
                        else null

                    let linkedCts: CancellationTokenSource =
                        if isNull timeoutCts then null
                        else CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeoutCts.Token)

                    let effectiveToken = if isNull linkedCts then cancellationToken else linkedCts.Token
                    let lockTask = receiveLock.WaitAsync effectiveToken
                    let pending: Task<WebSocketReceiveResult> ref = ref null
                    Ok struct (timeoutCts, linkedCts, effectiveToken, lockTask, ref (ArrayPool<byte>.Shared.Rent bufferSize), pending))
                .FlatMap FIO.fromResult

        // A receive the fiber gave up on, through an interruption or a caller's token that is not the fiber's, still
        // reads into the rented array and holds the lock, so the clean-up waits for it instead of handing the array
        // to the next renter under a pending read.
        let release struct (timeoutCts: CancellationTokenSource, linkedCts: CancellationTokenSource, _: CancellationToken, lockTask: Task, rented: byte[] ref, pending: Task<WebSocketReceiveResult> ref) =
            FIO.succeedWith <| fun () ->
                let cleanUp () =
                    ArrayPool<byte>.Shared.Return rented.Value
                    releaseLogged "receiveLock release" (fun () -> releasePermitWhenGranted receiveLock lockTask)
                    releaseLogged "linkedCts disposal" (fun () -> if not (isNull linkedCts) then linkedCts.Dispose())
                    releaseLogged "timeoutCts disposal" (fun () -> if not (isNull timeoutCts) then timeoutCts.Dispose())

                match pending.Value with
                | null -> cleanUp ()
                | receive when receive.IsCompleted -> cleanUp ()
                | receive ->
                    receive.ContinueWith(
                        (fun (completed: Task<WebSocketReceiveResult>) ->
                            completed.Exception |> ignore
                            cleanUp ()),
                        TaskContinuationOptions.ExecuteSynchronously)
                    |> ignore

        let receive struct (timeoutCts: CancellationTokenSource, _: CancellationTokenSource, effectiveToken: CancellationToken, lockTask: Task, rented: byte[] ref, pending: Task<WebSocketReceiveResult> ref) =
            let computation =
                fio {
                    do! FIO.awaitUnitTask lockTask receiveError

                    let mutable length = 0
                    let mutable endOfMessage = false
                    let mutable messageType = WebSocketMessageType.Text
                    let mutable isCloseFrame = false
                    let mutable totalSize = 0L

                    while not endOfMessage do
                        do! FIO.attempt (fun () ->
                                effectiveToken.ThrowIfCancellationRequested()
                                if rented.Value.Length - length < bufferSize then
                                    let larger = ArrayPool<byte>.Shared.Rent(max (rented.Value.Length * 2) (length + bufferSize))
                                    Buffer.BlockCopy(rented.Value, 0, larger, 0, length)
                                    ArrayPool<byte>.Shared.Return rented.Value
                                    rented.Value <- larger) receiveError

                        let! receiveTask =
                            FIO.attempt (fun () ->
                                pending.Value <- socket.ReceiveAsync(ArraySegment(rented.Value, length, bufferSize), effectiveToken)
                                pending.Value) receiveError

                        let! receiveResult = FIO.awaitTask receiveTask receiveError

                        messageType <- receiveResult.MessageType
                        endOfMessage <- receiveResult.EndOfMessage

                        if messageType = WebSocketMessageType.Close then
                            isCloseFrame <- true
                            endOfMessage <- true
                        else
                            let count = receiveResult.Count
                            totalSize <- totalSize + int64 count

                            // The rest of the message is unread, and a later receive would take it for a new one.
                            if totalSize > config.MaxMessageSize then
                                do! attempt (fun () -> socket.Abort())
                                return! FIO.fail (MessageTooLarge(totalSize, config.MaxMessageSize))

                            length <- length + count

                    if isCloseFrame then
                        let status = Option.ofNullable socket.CloseStatus
                        let desc = socket.CloseStatusDescription
                        return ConnectionClosed(status, desc)
                    else
                        match messageType with
                        | WebSocketMessageType.Text ->
                            let text = Encoding.UTF8.GetString(rented.Value, 0, length)
                            return Frame(Text text)
                        | WebSocketMessageType.Binary ->
                            return Frame(Binary(rented.Value.AsSpan(0, length).ToArray()))
                        | _ ->
                            return! FIO.fail (ReceiveFailed "Unexpected message type")
                }

            let remapTimeout (error: WsError) =
                if not (isNull timeoutCts) && timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"Receive operation timed out after {config.ReceiveTimeout}ms")
                else
                    FIO.fail error

            computation.CatchAll remapTimeout

        FIO.acquireReleaseWith setup release receive

    /// Receives the next complete message, using the fiber's cancellation token.
    member this.ReceiveMessage () =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.ReceiveMessage cancellationToken
        }

    /// Sends a frame, using the given cancellation token.
    member _.SendFrame (frame: WebSocketFrame, cancellationToken: CancellationToken) =
        // One setup action per message, run uninterruptibly by acquireReleaseWith, so the lock wait, the token
        // sources and the pooled text buffer always reach the release. Without a send timeout the fiber's token
        // is the only one, so no source is needed.
        let setup =
            (attempt <| fun () ->
                let state = socket.State

                let canSend =
                    match frame with
                    | Close _ ->
                        state = WebSocketState.Open || state = WebSocketState.CloseReceived
                    | _ ->
                        state = WebSocketState.Open

                if not canSend then
                    Error(
                        stateError
                            state
                            [ WebSocketState.Closed; WebSocketState.Aborted; WebSocketState.CloseReceived ]
                            SendFailed
                            "send frame")
                else
                    let timeoutCts: CancellationTokenSource =
                        if config.SendTimeout > 0 then new CancellationTokenSource(config.SendTimeout)
                        else null

                    let linkedCts: CancellationTokenSource =
                        if isNull timeoutCts then null
                        else CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeoutCts.Token)

                    let effectiveToken = if isNull linkedCts then cancellationToken else linkedCts.Token
                    let lockTask = sendLock.WaitAsync effectiveToken

                    let buffer =
                        match frame with
                        | Text text -> ArrayPool<byte>.Shared.Rent(Encoding.UTF8.GetMaxByteCount text.Length)
                        | _ -> null

                    Ok struct (timeoutCts, linkedCts, effectiveToken, lockTask, buffer))
                .FlatMap FIO.fromResult

        let release struct (timeoutCts: CancellationTokenSource, linkedCts: CancellationTokenSource, _: CancellationToken, lockTask: Task, buffer: byte[]) =
            FIO.succeedWith <| fun () ->
                if not (isNull buffer) then ArrayPool<byte>.Shared.Return buffer
                releaseLogged "sendLock release" (fun () -> releasePermitWhenGranted sendLock lockTask)
                releaseLogged "linkedCts disposal" (fun () -> if not (isNull linkedCts) then linkedCts.Dispose())
                releaseLogged "timeoutCts disposal" (fun () -> if not (isNull timeoutCts) then timeoutCts.Dispose())

        let send struct (timeoutCts: CancellationTokenSource, _: CancellationTokenSource, effectiveToken: CancellationToken, lockTask: Task, buffer: byte[]) =
            let computation =
                fio {
                    do! FIO.awaitUnitTask lockTask sendError

                    match frame with
                    | Text text ->
                        let! actualByteCount = attempt <| fun () ->
                            Encoding.UTF8.GetBytes(text, 0, text.Length, buffer, 0)

                        let! sendTask = FIO.attempt (fun () ->
                            socket.SendAsync(
                                ArraySegment(buffer, 0, actualByteCount),
                                WebSocketMessageType.Text,
                                true,
                                effectiveToken)) sendError

                        do! FIO.awaitUnitTask sendTask sendError
                    | Binary data ->
                        let! sendTask = FIO.attempt (fun () ->
                            socket.SendAsync(ArraySegment data, WebSocketMessageType.Binary, true, effectiveToken)) sendError
                        do! FIO.awaitUnitTask sendTask sendError
                    | Close(status, description) ->
                        let! closeTask = FIO.attempt (fun () ->
                            socket.CloseAsync(status, description, effectiveToken)) sendError
                        do! FIO.awaitUnitTask closeTask sendError
                }

            let remapTimeout (error: WsError) =
                if not (isNull timeoutCts) && timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"Send operation timed out after {config.SendTimeout}ms")
                else
                    FIO.fail error

            computation.CatchAll remapTimeout

        FIO.acquireReleaseWith setup release send

    /// Sends a frame, using the fiber's cancellation token.
    member this.SendFrame (frame: WebSocketFrame) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.SendFrame(frame, cancellationToken)
        }

    /// Sends a text message, using the given cancellation token.
    member this.SendText (text: string, cancellationToken: CancellationToken) =
        this.SendFrame(Text text, cancellationToken)

    /// Sends a text message, using the fiber's cancellation token.
    member this.SendText (text: string) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.SendText(text, cancellationToken)
        }

    /// Sends a binary message, using the given cancellation token.
    member this.SendBinary (data: byte[], cancellationToken: CancellationToken) =
        this.SendFrame(Binary data, cancellationToken)

    /// Sends a binary message, using the fiber's cancellation token.
    member this.SendBinary (data: byte[]) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.SendBinary(data, cancellationToken)
        }

    /// Sends a value encoded with the given codec, using the given cancellation token.
    member this.Send<'A> (codec: WebSocketCodec<'A>, value: 'A, cancellationToken: CancellationToken) =
        fio {
            let! frameResult = codec.Encode value
            do! this.SendFrame(frameResult, cancellationToken)
        }

    /// Sends a value encoded with the given codec, using the fiber's cancellation token.
    member this.Send<'A> (codec: WebSocketCodec<'A>, value: 'A) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.Send(codec, value, cancellationToken)
        }

    /// Receives and decodes a value with the given codec, using the given cancellation token.
    member this.Receive<'A> (codec: WebSocketCodec<'A>, cancellationToken: CancellationToken) =
        fio {
            match! this.ReceiveMessage cancellationToken with
            | Frame frame ->
                return! codec.Decode frame
            | ConnectionClosed(status, desc) ->
                return! FIO.fail (Closed(WsError.describeClose status desc))
        }

    /// Receives and decodes a value with the given codec, using the fiber's cancellation token.
    member this.Receive<'A> (codec: WebSocketCodec<'A>) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.Receive(codec, cancellationToken)
        }

    /// Receives and decodes a value with the given codec, using the given cancellation token; a close or an
    /// undecodable frame is an outcome, not a failure.
    member this.TryReceive<'A> (codec: WebSocketCodec<'A>, cancellationToken: CancellationToken) : FIO<ReceiveOutcome<'A>, WsError> =
        this.Receive(codec, cancellationToken).Map(Received).CatchAll(function
            | Closed reason -> FIO.succeed (PeerClosed reason)
            | CodecError reason -> FIO.succeed (Undecodable reason)
            | error -> FIO.fail error)

    /// Receives and decodes a value with the given codec, using the fiber's cancellation token; a close or an
    /// undecodable frame is an outcome, not a failure.
    member this.TryReceive<'A> (codec: WebSocketCodec<'A>) : FIO<ReceiveOutcome<'A>, WsError> =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.TryReceive(codec, cancellationToken)
        }

    /// Closes the connection with the given status and description, using the given cancellation token and bounded
    /// by the configured send timeout. While another fiber is receiving, only the outgoing side is closed and that
    /// receive ends with <c>ConnectionClosed</c>.
    member _.Close (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) =
        boundedBySendTimeout cancellationToken "Close operation" <| fun effectiveToken ->
            fio {
                let! sendLockTask = attempt <| fun () ->
                    sendLock.WaitAsync effectiveToken

                let hasReceiveLock = ref false

                let closeOp =
                    fio {
                        do! FIO.awaitUnitTask sendLockTask sendError

                        let! takenReceiveLock = attempt <| fun () -> receiveLock.Wait 0
                        hasReceiveLock.Value <- takenReceiveLock

                        let! closeTask = FIO.attempt (fun () ->
                            if takenReceiveLock then
                                socket.CloseAsync(closeStatus, statusDescription, effectiveToken)
                            else
                                socket.CloseOutputAsync(closeStatus, statusDescription, effectiveToken)) sendError
                        do! FIO.awaitUnitTask closeTask sendError
                    }

                let finalizer =
                    fio {
                        do! attempt(fun () -> releasePermitWhenGranted sendLock sendLockTask)
                                .CatchAll(logAndSuppress "sendLock release")

                        if hasReceiveLock.Value then
                            do! attempt(fun () -> receiveLock.Release() |> ignore)
                                    .CatchAll(logAndSuppress "receiveLock release")
                    }

                return! closeOp.Ensuring finalizer
            }

    /// Closes the connection with the given status and description, using the fiber's cancellation token.
    member this.Close (closeStatus: WebSocketCloseStatus, statusDescription: string) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.Close(closeStatus, statusDescription, cancellationToken)
        }

    /// Closes the connection normally, using the given cancellation token.
    member this.Close (cancellationToken: CancellationToken) =
        this.Close(WebSocketCloseStatus.NormalClosure, "Normal closure", cancellationToken)

    /// Closes the connection normally, using the fiber's cancellation token.
    member this.Close () =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.Close cancellationToken
        }

    /// Closes the outgoing side of the connection with the given status, using the given cancellation token and
    /// bounded by the configured send timeout.
    member _.CloseOutput (closeStatus: WebSocketCloseStatus, statusDescription: string, cancellationToken: CancellationToken) =
        boundedBySendTimeout cancellationToken "Close output operation" <| fun effectiveToken ->
            fio {
                let! sendLockTask = attempt <| fun () ->
                    sendLock.WaitAsync effectiveToken

                let closeOp =
                    fio {
                        do! FIO.awaitUnitTask sendLockTask sendError

                        let! closeTask = FIO.attempt (fun () ->
                            socket.CloseOutputAsync(closeStatus, statusDescription, effectiveToken)) sendError
                        do! FIO.awaitUnitTask closeTask sendError
                    }

                let finalizer =
                    attempt(fun () -> releasePermitWhenGranted sendLock sendLockTask)
                        .CatchAll(logAndSuppress "sendLock release")

                return! closeOp.Ensuring finalizer
            }

    /// Closes the outgoing side of the connection with the given status, using the fiber's cancellation token.
    member this.CloseOutput (closeStatus: WebSocketCloseStatus, statusDescription: string) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()
            return! this.CloseOutput(closeStatus, statusDescription, cancellationToken)
        }

    /// Closes the outgoing side of the connection normally.
    member this.CloseOutput () =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal closure")

    /// Aborts the connection immediately without a closing handshake.
    member _.Abort () =
        fio {
            do! attempt <| fun () -> socket.Abort()
        }

    /// Returns an effect that yields this connection's current state.
    member _.State () =
        fio {
            return! attempt <| fun () -> socket.State
        }

    /// Returns an effect that yields this connection's close status, if it has closed.
    member _.CloseStatus () =
        fio {
            return! attempt <| fun () -> Option.ofNullable socket.CloseStatus
        }

    /// Returns an effect that yields this connection's close status description, if any.
    member _.CloseStatusDescription () =
        fio {
            return! attempt <| fun () -> socket.CloseStatusDescription
        }

    /// Returns an effect that yields the negotiated subprotocol, if any.
    member _.Subprotocol () =
        fio {
            return! attempt <| fun () -> socket.SubProtocol
        }

    /// The peer's address for a server-accepted connection; None for a client.
    member _.RemoteEndPoint =
        remoteEndPoint

    /// The local address for a server-accepted connection; None for a client.
    member _.LocalEndPoint =
        localEndPoint

    /// Closes this connection if it is still open, logging a failed close instead of failing.
    member this.CloseIfOpen () : FIO<unit, WsError> =
        fio {
            // An interrupted receive leaves the socket Aborted, which is unclosable and not worth reporting.
            match! this.State().CatchAll(fun _ -> FIO.succeed WebSocketState.Closed) with
            | WebSocketState.Open
            | WebSocketState.CloseReceived
            | WebSocketState.CloseSent ->
                // A peer that closed first is not an error.
                do! this.Close().CatchAll(function
                        | Closed _ -> FIO.unit ()
                        | error -> logAndSuppress "close on release" error)
            | _ -> ()
        }

    /// Releases the resources held by this connection.
    member _.Dispose () =
        fio {
            do! attempt(fun () -> socket.Dispose())
                    .CatchAll(logAndSuppress "socket disposal")
            do! attempt(fun () -> sendLock.Dispose())
                    .CatchAll(logAndSuppress "sendLock disposal")
            do! attempt(fun () -> receiveLock.Dispose())
                    .CatchAll(logAndSuppress "receiveLock disposal")
        }

    interface IDisposable with

        member _.Dispose () =
            try
                socket.Dispose()
                sendLock.Dispose()
                receiveLock.Dispose()
            with ex ->
                eprintfn $"WebSocket encountered error during IDisposable.Dispose: {ex.Message}"
