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
            let! cancelToken = FIO.cancellationToken ()

            if timeoutMs <= 0 then
                let task =
                    try
                        taskFactory cancelToken
                    with ex ->
                        Task.FromException<'A> ex
                return! FIO.awaitTask task onError
            else
                return!
                    FIO.suspend <| fun () ->
                        let linked = CancellationTokenSource.CreateLinkedTokenSource cancelToken
                        linked.CancelAfter timeoutMs

                        let task =
                            try
                                taskFactory linked.Token
                            with ex ->
                                Task.FromException<'A> ex

                        let mapError (ex: exn) =
                            if linked.IsCancellationRequested && not cancelToken.IsCancellationRequested then
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
            let! cancelToken = FIO.cancellationToken ()

            if timeoutMs <= 0 then
                let task =
                    try
                        taskFactory cancelToken
                    with ex ->
                        Task.FromException ex

                return! FIO.awaitUnitTask task onError
            else
                return!
                    FIO.suspend <| fun () ->
                        let linked = CancellationTokenSource.CreateLinkedTokenSource cancelToken
                        linked.CancelAfter timeoutMs

                        let task =
                            try
                                taskFactory linked.Token
                            with ex ->
                                Task.FromException ex

                        let mapError (ex: exn) =
                            if linked.IsCancellationRequested && not cancelToken.IsCancellationRequested then
                                TimeoutError $"Operation timed out after {timeoutMs} ms"
                            else
                                onError ex

                        (FIO.awaitUnitTask task mapError)
                            .Ensuring(FIO.attempt (fun () -> linked.Dispose()) SocketError.fromException)
        }

    /// Sends a buffer of raw bytes over this socket.
    member _.SendBytes (buffer: byte[]) =
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
    member _.ReceiveBytes (maxBytes: int) =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))

            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            let! pooledBuffer = attempt (fun () -> ArrayPool<byte>.Shared.Rent maxBytes)

            let readAndCopy =
                fio {
                    let! bytesRead =
                        runWithTimeout
                            config.ReceiveTimeout
                            (fun ct -> stream.ReadAsync(pooledBuffer, 0, maxBytes, ct))
                            SocketError.fromException

                    if bytesRead = 0 then
                        return! FIO.fail (ConnectionClosed "Connection closed by peer")

                    let result = Array.zeroCreate<byte> bytesRead
                    Buffer.BlockCopy(pooledBuffer, 0, result, 0, bytesRead)
                    return result, bytesRead
                }

            return! readAndCopy.Ensuring(attempt (fun () -> ArrayPool<byte>.Shared.Return(pooledBuffer, true)))
        }

    /// Receives exactly the given number of bytes from this socket, blocking until they arrive.
    member _.ReceiveExactly (numBytes: int) =
        fio {
            if numBytes <= 0 then
                return! FIO.fail (InvalidState("positive byte count", $"{numBytes}"))

            let! pooledBuffer = attempt (fun () -> ArrayPool<byte>.Shared.Rent numBytes)

            let readLoop =
                fio {
                    let mutable totalRead = 0

                    while totalRead < numBytes do
                        let! bytesRead =
                            runWithTimeout
                                config.ReceiveTimeout
                                (fun ct -> stream.ReadAsync(pooledBuffer, totalRead, numBytes - totalRead, ct))
                                SocketError.fromException

                        if bytesRead = 0 then
                            return!
                                FIO.fail (ConnectionClosed $"Connection closed after {totalRead} of {numBytes} bytes")

                        totalRead <- totalRead + bytesRead

                    let result = Array.zeroCreate<byte> numBytes
                    Buffer.BlockCopy(pooledBuffer, 0, result, 0, numBytes)
                    return result
                }

            return! readLoop.Ensuring(attempt (fun () -> ArrayPool<byte>.Shared.Return(pooledBuffer, true)))
        }

    /// Sends a value over this socket, encoded with the given codec.
    member this.Send<'A> (codec: SocketCodec<'A>, value: 'A) =
        fio {
            let! bytes = codec.Encode value
            do! this.SendBytes bytes
        }

    /// Receives a value from this socket, decoded with the given codec.
    member this.Receive<'A> (codec: SocketCodec<'A>, maxBytes: int) =
        fio {
            let! bytes, _ = this.ReceiveBytes maxBytes
            return! codec.Decode bytes
        }

    /// Sends a UTF-8 string over this socket.
    member this.SendString (str: string) =
        this.Send(Codec.string, str)

    /// Receives a UTF-8 string from this socket.
    member this.ReceiveString (maxBytes: int) =
        this.Receive(Codec.string, maxBytes)

    /// Sends a newline-terminated string over this socket.
    member this.SendLine (line: string) =
        this.Send(Codec.line, line)

    /// Receives a single newline-terminated line from this socket.
    member this.ReceiveLine (maxBytes: int) =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))

            let accumulator = Collections.Generic.List<byte>()
            let mutable complete = false

            while not complete do
                if accumulator.Count >= maxBytes then
                    return! FIO.fail (BufferOverflow(maxBytes + 1, maxBytes))

                let! chunk = this.ReceiveExactly 1
                let received = chunk[0]
                accumulator.Add received

                if received = byte '\n' then
                    complete <- true

            let! text = Codec.string.Decode(accumulator.ToArray())
            return text.TrimEnd('\n', '\r')
        }

    /// Sends a value as JSON over this socket.
    member this.SendJson<'A> (value: 'A) =
        this.Send(Codec.json, value)

    /// Receives a JSON value from this socket.
    member this.ReceiveJson<'A> (maxBytes: int) =
        this.Receive<'A>(Codec.json, maxBytes)

    /// Sends a value as a newline-terminated JSON message over this socket.
    member this.SendJsonLine<'A> (value: 'A) =
        this.Send(Codec.jsonLine None, value)

    /// Receives a newline-terminated JSON value from this socket.
    member this.ReceiveJsonLine<'A> (maxBytes: int) =
        this.Receive<'A>(Codec.jsonLine None, maxBytes)

    /// Sends a value as a length-prefixed frame, encoded with the given codec.
    member this.SendFramed<'A> (codec: SocketCodec<'A>, value: 'A) =
        fio {
            let! frame = (Codec.lengthPrefixed codec).Encode value
            do! this.SendBytes frame
        }

    /// Receives a length-prefixed frame, decoded with the given codec, rejecting frames larger than the maximum size.
    member this.ReceiveFramed<'A> (codec: SocketCodec<'A>, maxFrameSize: int) =
        fio {
            let! header = this.ReceiveExactly 4
            let length = IPAddress.NetworkToHostOrder(BitConverter.ToInt32(header, 0))

            if length < 0 then
                return! FIO.fail (CodecError($"Negative frame length: {length}", ArgumentOutOfRangeException "length"))

            if length > maxFrameSize then
                return! FIO.fail (BufferOverflow(length, maxFrameSize))

            if length = 0 then
                return! codec.Decode [||]
            else
                let! payload = this.ReceiveExactly length
                return! codec.Decode payload
        }

    /// Receives a length-prefixed frame, decoded with the given codec, using a default 16 MB frame limit.
    member this.ReceiveFramed<'A> (codec: SocketCodec<'A>) =
        this.ReceiveFramed(codec, 16 * 1024 * 1024)

    /// Gracefully shuts down and closes this socket, suppressing errors.
    member _.Close () =
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
    member _.IsConnected () =
        try
            netSocket.Connected
        with _ ->
            false

    /// Gets the remote endpoint this socket is connected to.
    member _.GetRemoteEndPoint () =
        attempt <| fun () -> netSocket.RemoteEndPoint

    /// Gets the local endpoint this socket is bound to.
    member _.GetLocalEndPoint () =
        attempt <| fun () -> netSocket.LocalEndPoint

    /// Gets the configuration this socket was created with.
    member _.GetConfig () =
        config

    /// Releases the resources held by this socket.
    member _.Dispose () =
        attempt releaseResources

    interface IDisposable with

        member this.Dispose () =
            releaseResources ()
            GC.SuppressFinalize this

    override _.Finalize () =
        releaseResources ()
