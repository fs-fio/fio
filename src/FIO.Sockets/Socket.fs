namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Buffers
open System.Threading
open System.Threading.Tasks

/// An open TCP socket connection for sending and receiving typed messages.
type Socket internal (netSocket: Sockets.Socket, config: SocketConfig) =

    let stream = new Sockets.NetworkStream(netSocket, ownsSocket = false)

    let cleanupLock = obj ()
    let mutable disposed = false

    let releaseResources () =
        lock cleanupLock <| fun () ->
            if not disposed then
                disposed <- true

                try
                    stream.Dispose()
                with _ ->
                    ()

                try
                    netSocket.Dispose()
                with _ ->
                    ()

    // Set when a receive gives up part-way through a message: the bytes it consumed are gone, so any later receive would
    // start mid-message and misread the stream.
    [<VolatileField>]
    let mutable desynced : string = null

    let poison (reason: string) =
        if isNull desynced then
            desynced <- reason

    let ensureInSync : FIO<unit, SocketError> =
        FIO.suspend <| fun () ->
            match desynced with
            | null -> FIO.unit ()
            | reason -> FIO.fail (ConnectionClosed $"Receive side out of sync after {reason}; close the connection")

    let logAndSuppress (context: string) (error: SocketError) =
        fio {
            let str = error.ToString()
            do! FIO.attempt (fun () -> eprintfn $"Socket encountered error during {context}: {str}") SocketError.fromException
            return ()
        }

    let attempt (func: unit -> 'A) =
        FIO.attempt func SocketError.fromException

    let runWithTimeout
        (timeoutMs: int)
        (taskFactory: CancellationToken -> Task<'A>)
        (onError: exn -> SocketError) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()

            if timeoutMs <= 0 then
                let task =
                    try
                        taskFactory cancellationToken
                    with ex ->
                        Task.FromException<'A> ex
                return! FIO.awaitTask task onError
            else
                return!
                    FIO.suspend <| fun () ->
                        let linked = CancellationTokenSource.CreateLinkedTokenSource cancellationToken
                        linked.CancelAfter timeoutMs

                        let task =
                            try
                                taskFactory linked.Token
                            with ex ->
                                Task.FromException<'A> ex

                        let mapError (ex: exn) =
                            if linked.IsCancellationRequested && not cancellationToken.IsCancellationRequested then
                                TimeoutError $"Operation timed out after {timeoutMs} ms"
                            else
                                onError ex

                        (FIO.awaitTask task mapError)
                            .Ensuring(FIO.attempt (fun () -> linked.Dispose()) SocketError.fromException)
        }

    let runWithTimeoutUnit
        (timeoutMs: int)
        (taskFactory: CancellationToken -> Task)
        (onError: exn -> SocketError) =
        fio {
            let! cancellationToken = FIO.cancellationToken ()

            if timeoutMs <= 0 then
                let task =
                    try
                        taskFactory cancellationToken
                    with ex ->
                        Task.FromException ex

                return! FIO.awaitUnitTask task onError
            else
                return!
                    FIO.suspend <| fun () ->
                        let linked = CancellationTokenSource.CreateLinkedTokenSource cancellationToken
                        linked.CancelAfter timeoutMs

                        let task =
                            try
                                taskFactory linked.Token
                            with ex ->
                                Task.FromException ex

                        let mapError (ex: exn) =
                            if linked.IsCancellationRequested && not cancellationToken.IsCancellationRequested then
                                TimeoutError $"Operation timed out after {timeoutMs} ms"
                            else
                                onError ex

                        (FIO.awaitUnitTask task mapError)
                            .Ensuring(FIO.attempt (fun () -> linked.Dispose()) SocketError.fromException)
        }

    /// Sends a buffer of raw bytes over this socket.
    member _.SendBytes (buffer: byte[]) : FIO<unit, SocketError> =
        fio {
            if isNull buffer then
                return! FIO.fail (InvalidState("non-null buffer", "null"))

            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            do!
                runWithTimeoutUnit
                    config.SendTimeout
                    (fun ct -> stream.WriteAsync(buffer, 0, buffer.Length, ct))
                    SocketError.fromException

            do! runWithTimeoutUnit config.SendTimeout (fun ct -> stream.FlushAsync ct) SocketError.fromException
        }

    /// Receives up to the given number of bytes from this socket, returning the bytes read and their count.
    member _.ReceiveBytes (maxBytes: int) : FIO<byte[] * int, SocketError> =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))

            do! ensureInSync

            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            let! pooledBuffer = attempt (fun () -> ArrayPool<byte>.Shared.Rent maxBytes)
            let received = ref 0
            let delivered = ref false

            let readAndCopy =
                fio {
                    let! bytesRead =
                        runWithTimeout
                            config.ReceiveTimeout
                            (fun ct -> stream.ReadAsync(pooledBuffer, 0, maxBytes, ct))
                            SocketError.fromException

                    received.Value <- bytesRead

                    if bytesRead = 0 then
                        return! FIO.fail (ConnectionClosed "Connection closed by peer")

                    let result = Array.zeroCreate<byte> bytesRead
                    Buffer.BlockCopy(pooledBuffer, 0, result, 0, bytesRead)
                    delivered.Value <- true
                    return result, bytesRead
                }

            return!
                readAndCopy
                    .Ensuring(attempt (fun () -> ArrayPool<byte>.Shared.Return(pooledBuffer, true)))
                    .Ensuring(FIO.succeedWith (fun () ->
                        if received.Value > 0 && not delivered.Value then
                            poison "an interrupted read"))
        }

    /// Receives exactly the given number of bytes from this socket, blocking until they arrive.
    member _.ReceiveExactly (numBytes: int) : FIO<byte[], SocketError> =
        fio {
            if numBytes <= 0 then
                return! FIO.fail (InvalidState("positive byte count", $"{numBytes}"))

            do! ensureInSync

            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            let! pooledBuffer = attempt (fun () -> ArrayPool<byte>.Shared.Rent numBytes)
            let totalRead = ref 0
            let delivered = ref false

            let readLoop =
                fio {
                    while totalRead.Value < numBytes do
                        let! bytesRead =
                            runWithTimeout
                                config.ReceiveTimeout
                                (fun ct -> stream.ReadAsync(pooledBuffer, totalRead.Value, numBytes - totalRead.Value, ct))
                                SocketError.fromException

                        if bytesRead = 0 then
                            return!
                                FIO.fail (ConnectionClosed $"Connection closed after {totalRead.Value} of {numBytes} bytes")

                        totalRead.Value <- totalRead.Value + bytesRead

                    let result = Array.zeroCreate<byte> numBytes
                    Buffer.BlockCopy(pooledBuffer, 0, result, 0, numBytes)
                    delivered.Value <- true
                    return result
                }

            return!
                readLoop
                    .Ensuring(attempt (fun () -> ArrayPool<byte>.Shared.Return(pooledBuffer, true)))
                    .Ensuring(FIO.succeedWith (fun () ->
                        if totalRead.Value > 0 && not delivered.Value then
                            poison $"a read cut off after {totalRead.Value} of {numBytes} bytes"))
        }

    /// Sends a value over this socket, encoded with the given codec.
    member this.Send<'A> (codec: SocketCodec<'A>, value: 'A) : FIO<unit, SocketError> =
        fio {
            let! bytes = codec.Encode value
            do! this.SendBytes bytes
        }

    /// Receives a value from this socket, decoded with the given codec.
    member this.Receive<'A> (codec: SocketCodec<'A>, maxBytes: int) : FIO<'A, SocketError> =
        fio {
            let! bytes, _ = this.ReceiveBytes maxBytes
            return! codec.Decode bytes
        }

    /// Sends a UTF-8 string over this socket.
    member this.SendString (str: string) : FIO<unit, SocketError> =
        this.Send(Codec.string, str)

    /// Receives a UTF-8 string from this socket.
    member this.ReceiveString (maxBytes: int) : FIO<string, SocketError> =
        this.Receive(Codec.string, maxBytes)

    /// Sends a newline-terminated string over this socket.
    member this.SendLine (line: string) : FIO<unit, SocketError> =
        this.Send(Codec.line, line)

    /// Receives a single newline-terminated line from this socket.
    member this.ReceiveLine (maxBytes: int) : FIO<string, SocketError> =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))

            do! ensureInSync

            let accumulator = Collections.Generic.List<byte>()
            let complete = ref false

            let readLine =
                fio {
                    while not complete.Value do
                        if accumulator.Count >= maxBytes then
                            return! FIO.fail (BufferOverflow(maxBytes + 1, maxBytes))

                        let! chunk = this.ReceiveExactly 1
                        let received = chunk[0]
                        accumulator.Add received

                        if received = byte '\n' then
                            complete.Value <- true
                }

            do!
                readLine.Ensuring(FIO.succeedWith (fun () ->
                    if accumulator.Count > 0 && not complete.Value then
                        poison (
                            if accumulator.Count >= maxBytes then $"a line longer than {maxBytes} bytes"
                            else "a line cut off part-way")))

            let! text = Codec.string.Decode(accumulator.ToArray())
            return text.TrimEnd('\n', '\r')
        }

    /// Sends a value as JSON over this socket.
    member this.SendJson<'A> (value: 'A) : FIO<unit, SocketError> =
        this.Send(Codec.json, value)

    /// Receives a JSON value from this socket.
    member this.ReceiveJson<'A> (maxBytes: int) : FIO<'A, SocketError> =
        this.Receive<'A>(Codec.json, maxBytes)

    /// Sends a value as a newline-terminated JSON message over this socket.
    member this.SendJsonLine<'A> (value: 'A) : FIO<unit, SocketError> =
        this.Send(Codec.jsonLine None, value)

    /// Receives a newline-terminated JSON value from this socket; maxBytes bounds the line, newline included.
    member this.ReceiveJsonLine<'A> (maxBytes: int) : FIO<'A, SocketError> =
        fio {
            let! line = this.ReceiveLine maxBytes
            return! (Codec.jsonLine<'A> None).Decode(Text.Encoding.UTF8.GetBytes line)
        }

    /// Sends a value as a length-prefixed frame, encoded with the given codec.
    member this.SendFramed<'A> (codec: SocketCodec<'A>, value: 'A) : FIO<unit, SocketError> =
        fio {
            let! frame = (Codec.lengthPrefixed codec).Encode value
            do! this.SendBytes frame
        }

    /// Receives a length-prefixed frame, decoded with the given codec, rejecting frames larger than the maximum size.
    member this.ReceiveFramed<'A> (codec: SocketCodec<'A>, maxFrameSize: int) : FIO<'A, SocketError> =
        fio {
            do! ensureInSync

            let length = ref None
            let complete = ref false

            let readFrame =
                fio {
                    let! header = this.ReceiveExactly 4
                    let frameLength = IPAddress.NetworkToHostOrder(BitConverter.ToInt32(header, 0))
                    length.Value <- Some frameLength

                    if frameLength < 0 then
                        return!
                            FIO.fail (CodecError($"Negative frame length: {frameLength}", ArgumentOutOfRangeException "length"))

                    if frameLength > maxFrameSize then
                        return! FIO.fail (BufferOverflow(frameLength, maxFrameSize))

                    let! payload =
                        if frameLength = 0 then FIO.succeed [||]
                        else this.ReceiveExactly frameLength

                    complete.Value <- true
                    return payload
                }

            let! payload =
                readFrame.Ensuring(FIO.succeedWith (fun () ->
                    match length.Value with
                    | Some frameLength when not complete.Value ->
                        poison (
                            if frameLength < 0 then "a negative frame length"
                            elif frameLength > maxFrameSize then $"a {frameLength}-byte frame over the {maxFrameSize}-byte limit"
                            else "a frame cut off part-way")
                    | _ -> ()))

            return! codec.Decode payload
        }

    /// Receives a length-prefixed frame, decoded with the given codec, using a default 16 MB frame limit.
    member this.ReceiveFramed<'A> (codec: SocketCodec<'A>) : FIO<'A, SocketError> =
        this.ReceiveFramed(codec, 16 * 1024 * 1024)

    /// Gracefully shuts down and closes this socket, suppressing errors.
    member _.Close () : FIO<unit, SocketError> =
        fio {
            do! (attempt <| fun () ->
                    if not disposed then
                        try
                            netSocket.LingerState <- Sockets.LingerOption(config.LingerEnabled, config.LingerTimeout)
                        with _ ->
                            ()

                        if netSocket.Connected then
                            try
                                netSocket.Shutdown Sockets.SocketShutdown.Both
                            with _ ->
                                ())
                    .CatchAll(logAndSuppress "socket shutdown")

            do! (attempt releaseResources).CatchAll(logAndSuppress "socket close")
        }

    /// Indicates whether this socket is currently connected.
    member _.IsConnected () : bool =
        try
            netSocket.Connected
        with _ ->
            false

    /// Returns an effect that yields the remote endpoint this socket is connected to.
    member _.GetRemoteEndPoint () : FIO<EndPoint, SocketError> =
        attempt <| fun () -> netSocket.RemoteEndPoint

    /// Returns an effect that yields the local endpoint this socket is bound to.
    member _.GetLocalEndPoint () : FIO<EndPoint, SocketError> =
        attempt <| fun () -> netSocket.LocalEndPoint

    /// The configuration this socket was created with.
    member _.GetConfig () : SocketConfig =
        config

    member internal _.NetSocket : Sockets.Socket =
        netSocket

    /// Releases the resources held by this socket.
    member _.Dispose () : FIO<unit, SocketError> =
        attempt releaseResources

    interface IDisposable with

        member this.Dispose () : unit =
            releaseResources ()
            GC.SuppressFinalize this

    override _.Finalize () : unit =
        releaseResources ()
