namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Buffers

/// <summary>Represents a TCP socket connection with effectful send and receive operations.</summary>
type Socket internal (netSocket: Sockets.Socket, config: SocketConfig) =

    /// <summary>Represents the network stream wrapping the underlying socket for stream-based I/O.</summary>
    let stream = new Sockets.NetworkStream(netSocket, ownsSocket = false)

    let cleanupLock = obj ()
    let mutable disposed = false

    let releaseResources () =
        lock cleanupLock (fun () ->
            if not disposed then
                disposed <- true

                try
                    stream.Dispose()
                with _ ->
                    ()

                try
                    netSocket.Dispose()
                with _ ->
                    ())

    /// <summary>Transforms a socket error into a logged-and-suppressed unit effect for use in cleanup paths.</summary>
    /// <param name="context">A description of the operation being cleaned up.</param>
    /// <param name="error">The socket error to log.</param>
    /// <returns>An effect that logs the error to standard error and succeeds with unit.</returns>
    let logAndSuppress (context: string) (error: SocketError) =
        fio {
            let str = error.ToString()
            do! FIO.attempt (fun () -> eprintfn $"[Socket] Error during {context}: {str}") SocketError.fromException
            return ()
        }

    /// <summary>Lifts a synchronous function into a socket effect, mapping exceptions to <c>SocketError</c>.</summary>
    /// <param name="func">The synchronous function to execute.</param>
    /// <returns>An effect that produces the function's result or fails with a <c>GeneralError</c>.</returns>
    let fromFunc (func: unit -> 'T) =
        FIO.attempt func SocketError.fromException

    let runWithTimeout
        (timeoutMs: int)
        (mkTask: Threading.CancellationToken -> Threading.Tasks.Task<'T>)
        (onError: exn -> SocketError)
        : FIO<'T, SocketError> =
        fio {
            let! fiberToken = FIO.cancellationToken ()

            if timeoutMs <= 0 then
                let task =
                    try
                        mkTask fiberToken
                    with exn ->
                        Threading.Tasks.Task.FromException<'T> exn

                return! FIO.awaitTask task onError
            else
                return!
                    FIO.suspend (fun () ->
                        let linked = Threading.CancellationTokenSource.CreateLinkedTokenSource fiberToken
                        linked.CancelAfter timeoutMs

                        let task =
                            try
                                mkTask linked.Token
                            with exn ->
                                Threading.Tasks.Task.FromException<'T> exn

                        let mapError (exn: exn) =
                            if linked.IsCancellationRequested && not fiberToken.IsCancellationRequested then
                                TimeoutError $"Operation timed out after {timeoutMs} ms"
                            else
                                onError exn

                        (FIO.awaitTask task mapError)
                            .Ensuring(FIO.attempt (fun () -> linked.Dispose()) SocketError.fromException))
        }

    let runWithTimeoutUnit
        (timeoutMs: int)
        (mkTask: Threading.CancellationToken -> Threading.Tasks.Task)
        (onError: exn -> SocketError)
        : FIO<unit, SocketError> =
        fio {
            let! fiberToken = FIO.cancellationToken ()

            if timeoutMs <= 0 then
                let task =
                    try
                        mkTask fiberToken
                    with exn ->
                        Threading.Tasks.Task.FromException exn

                return! FIO.awaitUnitTask task onError
            else
                return!
                    FIO.suspend (fun () ->
                        let linked = Threading.CancellationTokenSource.CreateLinkedTokenSource fiberToken
                        linked.CancelAfter timeoutMs

                        let task =
                            try
                                mkTask linked.Token
                            with exn ->
                                Threading.Tasks.Task.FromException exn

                        let mapError (exn: exn) =
                            if linked.IsCancellationRequested && not fiberToken.IsCancellationRequested then
                                TimeoutError $"Operation timed out after {timeoutMs} ms"
                            else
                                onError exn

                        (FIO.awaitUnitTask task mapError)
                            .Ensuring(FIO.attempt (fun () -> linked.Dispose()) SocketError.fromException))
        }

    /// <summary>Creates an effect that sends raw bytes over the connection.</summary>
    /// <param name="buffer">The byte array to send.</param>
    /// <returns>An effect that completes when the bytes have been sent, or fails if the connection is closed.</returns>
    member _.SendBytes(buffer: byte[]) =
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

    /// <summary>Creates an effect that receives up to a specified number of bytes from the connection.</summary>
    /// <param name="maxBytes">The maximum number of bytes to receive; must be positive.</param>
    /// <returns>An effect that produces a tuple of the received byte array and the actual number of bytes read, or fails if the connection is closed.</returns>
    member _.ReceiveBytes(maxBytes: int) =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))

            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            let! pooledBuffer = fromFunc (fun () -> ArrayPool<byte>.Shared.Rent maxBytes)

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

            return! readAndCopy.Ensuring(fromFunc (fun () -> ArrayPool<byte>.Shared.Return(pooledBuffer, true)))
        }

    /// <summary>Creates an effect that receives exactly the specified number of bytes from the connection.</summary>
    /// <param name="numBytes">The exact number of bytes to receive; must be positive.</param>
    /// <returns>An effect that produces a byte array of exactly the requested length, or fails if the connection closes before all bytes arrive.</returns>
    member _.ReceiveExactly(numBytes: int) =
        fio {
            if numBytes <= 0 then
                return! FIO.fail (InvalidState("positive byte count", $"{numBytes}"))

            let! pooledBuffer = fromFunc (fun () -> ArrayPool<byte>.Shared.Rent numBytes)

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

            return! readLoop.Ensuring(fromFunc (fun () -> ArrayPool<byte>.Shared.Return(pooledBuffer, true)))
        }

    /// <summary>Creates an effect that encodes a value with the given codec and sends it over the connection.</summary>
    /// <param name="codec">The codec to use for encoding the value to bytes.</param>
    /// <param name="value">The value to encode and send.</param>
    /// <returns>An effect that completes when the encoded value has been sent.</returns>
    member this.Send<'T>(codec: SocketCodec<'T>, value: 'T) =
        fio {
            let! bytes = codec.Encode value
            do! this.SendBytes bytes
        }

    /// <summary>Creates an effect that receives bytes from the connection and decodes them with the given codec.</summary>
    /// <param name="codec">The codec to use for decoding bytes to a value.</param>
    /// <param name="maxBytes">The maximum number of bytes to receive.</param>
    /// <returns>An effect that produces the decoded value.</returns>
    member this.Receive<'T>(codec: SocketCodec<'T>, maxBytes: int) =
        fio {
            let! bytes, _ = this.ReceiveBytes maxBytes
            return! codec.Decode bytes
        }

    /// <summary>Creates an effect that sends a UTF-8 encoded string over the connection.</summary>
    /// <param name="str">The string to send.</param>
    /// <returns>An effect that completes when the string has been sent.</returns>
    member this.SendString(str: string) = this.Send(Codec.string, str)

    /// <summary>Creates an effect that receives bytes and decodes them as a UTF-8 string.</summary>
    /// <param name="maxBytes">The maximum number of bytes to receive.</param>
    /// <returns>An effect that produces the received string.</returns>
    member this.ReceiveString(maxBytes: int) = this.Receive(Codec.string, maxBytes)

    /// <summary>Creates an effect that sends a string with a trailing newline over the connection.</summary>
    /// <param name="line">The line to send.</param>
    /// <returns>An effect that completes when the line has been sent.</returns>
    member this.SendLine(line: string) = this.Send(Codec.line, line)

    /// <summary>Creates an effect that receives a complete newline-delimited line from the connection.</summary>
    /// <param name="maxBytes">The maximum number of bytes to read before failing with a buffer overflow.</param>
    /// <returns>An effect that produces the received line with trailing newline characters removed.</returns>
    /// <remarks>Reads until a newline so that exactly one line is consumed regardless of TCP segmentation; bytes belonging to subsequent messages are left unread.</remarks>
    member this.ReceiveLine(maxBytes: int) =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))

            let accumulator = Collections.Generic.List<byte>()
            let mutable complete = false

            while not complete do
                if accumulator.Count >= maxBytes then
                    return! FIO.fail (BufferOverflow(maxBytes + 1, maxBytes))

                let! chunk = this.ReceiveExactly 1
                let b = chunk.[0]
                accumulator.Add b

                if b = byte '\n' then
                    complete <- true

            let! text = Codec.string.Decode(accumulator.ToArray())
            return text.TrimEnd('\n', '\r')
        }

    /// <summary>Creates an effect that serializes a value to JSON and sends it over the connection.</summary>
    /// <param name="value">The value to serialize and send.</param>
    /// <returns>An effect that completes when the JSON-encoded value has been sent.</returns>
    member this.SendJson<'T>(value: 'T) = this.Send(Codec.json, value)

    /// <summary>Creates an effect that receives bytes and deserializes them from JSON.</summary>
    /// <param name="maxBytes">The maximum number of bytes to receive.</param>
    /// <returns>An effect that produces the deserialized value.</returns>
    member this.ReceiveJson<'T>(maxBytes: int) = this.Receive<'T>(Codec.json, maxBytes)

    /// <summary>Creates an effect that serializes a value to newline-terminated JSON and sends it over the connection.</summary>
    /// <param name="value">The value to serialize and send.</param>
    /// <returns>An effect that completes when the JSON line has been sent.</returns>
    member this.SendJsonLine<'T>(value: 'T) = this.Send(Codec.jsonLine None, value)

    /// <summary>Creates an effect that receives bytes and deserializes them from newline-terminated JSON.</summary>
    /// <param name="maxBytes">The maximum number of bytes to receive.</param>
    /// <returns>An effect that produces the deserialized value.</returns>
    member this.ReceiveJsonLine<'T>(maxBytes: int) =
        this.Receive<'T>(Codec.jsonLine None, maxBytes)

    /// <summary>Creates an effect that encodes a value and sends it as a single length-prefixed frame.</summary>
    /// <param name="codec">The codec to use for encoding the value to bytes.</param>
    /// <param name="value">The value to encode and send.</param>
    /// <returns>An effect that completes when the framed message has been sent.</returns>
    member this.SendFramed<'T>(codec: SocketCodec<'T>, value: 'T) =
        fio {
            let! frame = (Codec.lengthPrefixed codec).Encode value
            do! this.SendBytes frame
        }

    /// <summary>Creates an effect that receives exactly one length-prefixed frame and decodes it with the given codec.</summary>
    /// <param name="codec">The codec to use for decoding the frame payload.</param>
    /// <param name="maxFrameSize">The maximum allowed payload size in bytes; larger frames fail with BufferOverflow.</param>
    /// <returns>An effect that produces the decoded value for one complete frame, regardless of how the bytes were segmented.</returns>
    member this.ReceiveFramed<'T>(codec: SocketCodec<'T>, maxFrameSize: int) =
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

    /// <summary>Creates an effect that receives exactly one length-prefixed frame using a default maximum frame size of 16 MiB.</summary>
    /// <param name="codec">The codec to use for decoding the frame payload.</param>
    /// <returns>An effect that produces the decoded value for one complete frame.</returns>
    member this.ReceiveFramed<'T>(codec: SocketCodec<'T>) =
        this.ReceiveFramed(codec, 16 * 1024 * 1024)

    /// <summary>Creates an effect that gracefully shuts down and closes the connection, releasing associated resources.</summary>
    /// <returns>An effect that performs a graceful shutdown and releases resources, suppressing any cleanup errors.</returns>
    member _.Close () =
        fio {
            do!
                (fromFunc (fun () ->
                    if not disposed then
                        try
                            netSocket.LingerState <- Sockets.LingerOption(config.LingerEnabled, config.LingerTimeout)
                        with _ ->
                            ()

                        if netSocket.Connected then
                            try
                                netSocket.Shutdown Sockets.SocketShutdown.Both
                            with _ ->
                                ()))
                    .CatchAll(logAndSuppress "socket shutdown")

            do! (fromFunc releaseResources).CatchAll(logAndSuppress "socket close")
        }

    /// <summary>Returns whether the connection is currently active.</summary>
    /// <returns><c>true</c> if the connection is active; <c>false</c> otherwise.</returns>
    member _.IsConnected() =
        try
            netSocket.Connected
        with _ ->
            false

    /// <summary>Returns the remote endpoint of the connection.</summary>
    /// <returns>An effect that produces the remote endpoint address.</returns>
    member _.GetRemoteEndPoint() =
        fromFunc (fun () -> netSocket.RemoteEndPoint)

    /// <summary>Returns the local endpoint of the connection.</summary>
    /// <returns>An effect that produces the local endpoint address.</returns>
    member _.GetLocalEndPoint() =
        fromFunc (fun () -> netSocket.LocalEndPoint)

    /// <summary>Returns the configuration used to establish the connection.</summary>
    /// <returns>The SocketConfig associated with this connection.</returns>
    member _.GetConfig() = config

    /// <summary>Creates an effect that releases all resources associated with the connection.</summary>
    /// <returns>An effect that disposes the connection resources.</returns>
    /// <remarks>Prefer Close() for graceful shutdown with linger handling.</remarks>
    member _.Dispose() = fromFunc releaseResources

    /// <summary>Provides resource cleanup for the socket.</summary>
    interface IDisposable with
        /// <summary>Releases the socket's stream and network resources and suppresses finalization.</summary>
        member this.Dispose() =
            releaseResources ()
            GC.SuppressFinalize this

    override _.Finalize() = releaseResources ()
