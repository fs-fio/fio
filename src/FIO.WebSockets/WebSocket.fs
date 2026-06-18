namespace FIO.WebSockets

open FIO.DSL

open System
open System.Text
open System.Buffers
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

/// An open WebSocket connection for sending and receiving typed messages.
type WebSocket internal (socket: Net.WebSockets.WebSocket, config: WebSocketConfig) =

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

    let awaitUnitTask (task: Task) =
        FIO.awaitUnitTask task WsError.fromException

    let awaitTask (task: Task<'A>) =
        FIO.awaitTask task WsError.fromException

    /// Receives the next complete message, using the given cancellation token.
    member _.ReceiveMessage (cancelToken: CancellationToken) =
        fio {
            let! state = attempt <| fun () -> socket.State

            if state <> WebSocketState.Open && state <> WebSocketState.CloseSent then
                return! FIO.fail (WsError.fromException
                    (Exception $"Cannot receive message - WebSocket state is {state}"))

            let! timeoutCts = attempt <| fun () ->
                if config.ReceiveTimeout > 0 then
                    new CancellationTokenSource(config.ReceiveTimeout)
                else
                    new CancellationTokenSource()

            let! linkedCts = attempt <| fun () ->
                CancellationTokenSource.CreateLinkedTokenSource(cancelToken, timeoutCts.Token)

            let effectiveToken = linkedCts.Token

            let! lockTask = attempt <| fun () ->
                receiveLock.WaitAsync effectiveToken

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
                        do! attempt <| fun () -> effectiveToken.ThrowIfCancellationRequested()

                        let! receiveTask = attempt <| fun () ->
                            socket.ReceiveAsync(ArraySegment(buffer, 0, bufferSize), effectiveToken)

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
                        | WebSocketMessageType.Binary ->
                            return Frame(Binary data)
                        | _ ->
                            return! FIO.fail (WsError.fromException
                                (Exception "Unexpected message type"))
                }

            let finalizer =
                fio {
                    do! attempt <| fun () ->
                        ArrayPool<byte>.Shared.Return buffer
                    do! attempt(fun () -> receiveLock.Release() |> ignore)
                            .CatchAll(logAndSuppress "receiveLock release")
                    do! attempt(fun () -> linkedCts.Dispose())
                            .CatchAll(logAndSuppress "linkedCts disposal")
                    do! attempt(fun () -> timeoutCts.Dispose())
                            .CatchAll(logAndSuppress "timeoutCts disposal")
                }

            let remapTimeout (error: WsError) =
                if timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"Receive operation timed out after {config.ReceiveTimeout}ms")
                else
                    FIO.fail error

            return! (computation.CatchAll remapTimeout).Ensuring finalizer
        }

    /// Receives the next complete message, using the fiber's cancellation token.
    member this.ReceiveMessage () =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.ReceiveMessage cancelToken
        }

    /// Sends a frame, using the given cancellation token.
    member _.SendFrame (frame: WebSocketFrame, cancelToken: CancellationToken) =
        fio {
            let! state = attempt <| fun () -> socket.State

            let canSend =
                match frame with
                | Close _ ->
                    state = WebSocketState.Open || state = WebSocketState.CloseReceived
                | _ ->
                    state = WebSocketState.Open

            if not canSend then
                return! FIO.fail (WsError.fromException
                    (Exception $"Cannot send frame - WebSocket state is {state}"))

            let! timeoutCts = attempt <| fun () ->
                if config.SendTimeout > 0 then
                    new CancellationTokenSource(config.SendTimeout)
                else
                    new CancellationTokenSource()

            let! linkedCts = attempt <| fun () ->
                CancellationTokenSource.CreateLinkedTokenSource(cancelToken, timeoutCts.Token)

            let effectiveToken = linkedCts.Token

            let! lockTask = attempt <| fun () ->
                sendLock.WaitAsync effectiveToken

            do! awaitUnitTask lockTask

            let computation =
                fio {
                    match frame with
                    | Text text ->
                        let maxByteCount = Encoding.UTF8.GetMaxByteCount text.Length
                        let buffer = ArrayPool<byte>.Shared.Rent maxByteCount

                        let sendOp =
                            fio {
                                let! actualByteCount = attempt <| fun () ->
                                    Encoding.UTF8.GetBytes(text, 0, text.Length, buffer, 0)

                                let! sendTask = attempt <| fun () ->
                                    socket.SendAsync(
                                        ArraySegment(buffer, 0, actualByteCount),
                                        WebSocketMessageType.Text,
                                        true,
                                        effectiveToken)

                                do! awaitUnitTask sendTask
                            }
                        let returnBuffer = attempt <| fun () ->
                            ArrayPool<byte>.Shared.Return buffer
                        do! sendOp.Ensuring returnBuffer
                    | Binary data ->
                        let! sendTask = attempt <| fun () ->
                            socket.SendAsync(ArraySegment data, WebSocketMessageType.Binary, true, effectiveToken)
                        do! awaitUnitTask sendTask
                    | Close(status, description) ->
                        let! closeTask = attempt <| fun () ->
                            socket.CloseAsync(status, description, effectiveToken)
                        do! awaitUnitTask closeTask
                }

            let finalizer =
                fio {
                    do! attempt(fun () -> sendLock.Release() |> ignore)
                            .CatchAll(logAndSuppress "sendLock release")
                    do! attempt(fun () -> linkedCts.Dispose())
                            .CatchAll(logAndSuppress "linkedCts disposal")
                    do! attempt(fun () -> timeoutCts.Dispose())
                            .CatchAll(logAndSuppress "timeoutCts disposal")
                }

            let remapTimeout (error: WsError) =
                if timeoutCts.IsCancellationRequested then
                    FIO.fail (TimeoutError $"Send operation timed out after {config.SendTimeout}ms")
                else
                    FIO.fail error

            return! (computation.CatchAll remapTimeout).Ensuring finalizer
        }

    /// Sends a frame, using the fiber's cancellation token.
    member this.SendFrame (frame: WebSocketFrame) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.SendFrame(frame, cancelToken)
        }

    /// Sends a text message, using the given cancellation token.
    member this.SendText (text: string, cancelToken: CancellationToken) =
        this.SendFrame(Text text, cancelToken)

    /// Sends a text message, using the fiber's cancellation token.
    member this.SendText (text: string) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.SendText(text, cancelToken)
        }

    /// Sends a binary message, using the given cancellation token.
    member this.SendBinary (data: byte[], cancelToken: CancellationToken) =
        this.SendFrame(Binary data, cancelToken)

    /// Sends a binary message, using the fiber's cancellation token.
    member this.SendBinary (data: byte[]) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.SendBinary(data, cancelToken)
        }

    /// Sends a value encoded with the given codec, using the given cancellation token.
    member this.Send<'A> (codec: WebSocketCodec<'A>, value: 'A, cancelToken: CancellationToken) =
        fio {
            let! frameResult = codec.Encode value
            do! this.SendFrame(frameResult, cancelToken)
        }

    /// Sends a value encoded with the given codec, using the fiber's cancellation token.
    member this.Send<'A> (codec: WebSocketCodec<'A>, value: 'A) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.Send(codec, value, cancelToken)
        }

    /// Receives and decodes a value with the given codec, using the given cancellation token.
    member this.Receive<'A> (codec: WebSocketCodec<'A>, cancelToken: CancellationToken) =
        fio {
            match! this.ReceiveMessage cancelToken with
            | Frame frame ->
                return! codec.Decode frame
            | ConnectionClosed(status, desc) ->
                return! FIO.fail (WsError.fromException
                    (Exception $"Connection closed. Status: {status}, Description: {desc}"))
        }

    /// Receives and decodes a value with the given codec, using the fiber's cancellation token.
    member this.Receive<'A> (codec: WebSocketCodec<'A>) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.Receive(codec, cancelToken)
        }

    /// Closes the connection with the given status and description, using the given cancellation token.
    member _.Close (closeStatus: WebSocketCloseStatus, statusDescription: string, cancelToken: CancellationToken) =
        fio {
            let! sendLockTask = attempt <| fun () ->
                sendLock.WaitAsync cancelToken

            do! awaitUnitTask sendLockTask

            let! hasReceiveLock = attempt <| fun () ->
                receiveLock.Wait 0

            let closeOp =
                fio {
                    let! closeTask = attempt <| fun () ->
                        if hasReceiveLock then
                            socket.CloseAsync(closeStatus, statusDescription, cancelToken)
                        else
                            socket.CloseOutputAsync(closeStatus, statusDescription, cancelToken)
                    do! awaitUnitTask closeTask
                }

            let finalizer =
                fio {
                    do! attempt(fun () -> sendLock.Release() |> ignore)
                            .CatchAll(logAndSuppress "sendLock release")

                    if hasReceiveLock then
                        do! attempt(fun () -> receiveLock.Release() |> ignore)
                                .CatchAll(logAndSuppress "receiveLock release")
                }

            return! closeOp.Ensuring finalizer
        }

    /// Closes the connection with the given status and description, using the fiber's cancellation token.
    member this.Close (closeStatus: WebSocketCloseStatus, statusDescription: string) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.Close(closeStatus, statusDescription, cancelToken)
        }

    /// Closes the connection normally, using the given cancellation token.
    member this.Close (cancelToken: CancellationToken) =
        this.Close(WebSocketCloseStatus.NormalClosure, "Normal closure", cancelToken)

    /// Closes the connection normally, using the fiber's cancellation token.
    member this.Close () =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.Close cancelToken
        }

    /// Closes the outgoing side of the connection with the given status, using the given cancellation token.
    member _.CloseOutput (closeStatus: WebSocketCloseStatus, statusDescription: string, cancelToken: CancellationToken) =
        fio {
            let! sendLockTask = attempt <| fun () ->
                sendLock.WaitAsync cancelToken

            do! awaitUnitTask sendLockTask

            let closeOp =
                fio {
                    let! closeTask = attempt <| fun () ->
                        socket.CloseOutputAsync(closeStatus, statusDescription, cancelToken)
                    do! awaitUnitTask closeTask
                }

            let finalizer =
                attempt(fun () -> sendLock.Release() |> ignore)
                    .CatchAll(logAndSuppress "sendLock release")

            return! closeOp.Ensuring finalizer
        }

    /// Closes the outgoing side of the connection with the given status, using the fiber's cancellation token.
    member this.CloseOutput (closeStatus: WebSocketCloseStatus, statusDescription: string) =
        fio {
            let! cancelToken = FIO.cancellationToken ()
            return! this.CloseOutput(closeStatus, statusDescription, cancelToken)
        }

    /// Closes the outgoing side of the connection normally.
    member this.CloseOutput () =
        this.CloseOutput(WebSocketCloseStatus.NormalClosure, "Normal closure")

    /// Aborts the connection immediately without a closing handshake.
    member _.Abort () =
        fio {
            do! attempt <| fun () -> socket.Abort()
        }

    /// Gets the current connection state.
    member _.State () =
        fio {
            return! attempt <| fun () -> socket.State
        }

    /// Gets the close status, if the connection has closed.
    member _.CloseStatus () =
        fio {
            return! attempt <| fun () -> Option.ofNullable socket.CloseStatus
        }

    /// Gets the close status description, if any.
    member _.CloseStatusDescription () =
        fio {
            return! attempt <| fun () -> socket.CloseStatusDescription
        }

    /// Gets the negotiated subprotocol, if any.
    member _.Subprotocol () =
        fio {
            return! attempt <| fun () -> socket.SubProtocol
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
