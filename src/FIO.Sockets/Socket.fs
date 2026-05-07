namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Buffers

/// <summary>Represents a TCP socket connection with effectful send and receive operations.</summary>
type Socket internal (netSocket: Sockets.Socket, config: SocketConfig) =

    /// <summary>Represents the network stream wrapping the underlying socket for stream-based I/O.</summary>
    let stream = new Sockets.NetworkStream(netSocket, ownsSocket = false)
    /// <summary>Represents whether this socket has been disposed.</summary>
    let mutable disposed = false

    /// <summary>Transforms a socket error into a logged-and-suppressed unit effect for use in cleanup paths.</summary>
    /// <param name="context">A description of the operation being cleaned up.</param>
    /// <param name="err">The socket error to log.</param>
    /// <returns>An effect that logs the error to standard error and succeeds with unit.</returns>
    let logAndSuppress (context: string) (err: SocketError) =
        fio {
            let str = err.ToString()
            do! FIO.attempt ((fun () -> eprintfn $"[Socket] Error during {context}: {str}"), SocketError.fromException)
            return ()
        }

    /// <summary>Lifts a synchronous function into a socket effect, mapping exceptions to <c>SocketError</c>.</summary>
    /// <param name="func">The synchronous function to execute.</param>
    /// <returns>An effect that produces the function's result or fails with a <c>GeneralError</c>.</returns>
    let fromFunc (func: unit -> 'T) =
        FIO.attempt (func, SocketError.fromException)

    /// <summary>Lifts a non-generic .NET Task into a socket effect, mapping exceptions to <c>SocketError</c>.</summary>
    /// <param name="task">The task to await.</param>
    /// <returns>An effect that completes when the task finishes or fails with a <c>GeneralError</c>.</returns>
    let awaitTask (task: Threading.Tasks.Task) =
        FIO.awaitTask (task, SocketError.fromException)

    /// <summary>Lifts a generic .NET Task into a socket effect, mapping exceptions to <c>SocketError</c>.</summary>
    /// <param name="task">The task to await.</param>
    /// <returns>An effect that produces the task's result or fails with a <c>GeneralError</c>.</returns>
    let awaitTaskT (task: Threading.Tasks.Task<'T>) =
        FIO.awaitGenericTask (task, SocketError.fromException)

    /// <summary>Creates an effect that sends raw bytes over the connection.</summary>
    /// <param name="buffer">The byte array to send.</param>
    /// <returns>An effect that completes when the bytes have been sent, or fails if the connection is closed.</returns>
    member _.SendBytes(buffer: byte[]) =
        fio {
            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            do! awaitTask (stream.WriteAsync(buffer, 0, buffer.Length))
            do! awaitTask (stream.FlushAsync())
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
                    let! bytesRead = awaitTaskT (stream.ReadAsync(pooledBuffer, 0, maxBytes))

                    if bytesRead = 0 then
                        return! FIO.fail (ConnectionClosed "Connection closed by peer")

                    let result = Array.zeroCreate<byte> bytesRead
                    Buffer.BlockCopy(pooledBuffer, 0, result, 0, bytesRead)
                    return result, bytesRead
                }

            return! readAndCopy.Ensuring(fromFunc (fun () -> ArrayPool<byte>.Shared.Return pooledBuffer))
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
                            awaitTaskT (stream.ReadAsync(pooledBuffer, totalRead, numBytes - totalRead))

                        if bytesRead = 0 then
                            return!
                                FIO.fail (ConnectionClosed $"Connection closed after {totalRead} of {numBytes} bytes")

                        totalRead <- totalRead + bytesRead

                    let result = Array.zeroCreate<byte> numBytes
                    Buffer.BlockCopy(pooledBuffer, 0, result, 0, numBytes)
                    return result
                }

            return! readLoop.Ensuring(fromFunc (fun () -> ArrayPool<byte>.Shared.Return pooledBuffer))
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
            let! bytes, bytesRead = this.ReceiveBytes maxBytes
            let actualBytes = bytes.[0 .. bytesRead - 1]
            return! codec.Decode actualBytes
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

    /// <summary>Creates an effect that receives bytes and decodes them as a line-delimited string.</summary>
    /// <param name="maxBytes">The maximum number of bytes to receive.</param>
    /// <returns>An effect that produces the received line with trailing newline characters removed.</returns>
    member this.ReceiveLine(maxBytes: int) = this.Receive(Codec.line, maxBytes)

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

    /// <summary>Creates an effect that closes the connection and releases associated resources.</summary>
    /// <returns>An effect that shuts down and closes the connection, suppressing any cleanup errors.</returns>
    member _.Close() =
        fio {
            do!
                fromFunc(fun () ->
                    netSocket.LingerState <- Sockets.LingerOption(config.LingerEnabled, config.LingerTimeout)

                    if stream <> null then
                        stream.Close()
                        stream.Dispose())
                    .CatchAll(logAndSuppress "stream close")

            do!
                fromFunc(fun () ->
                    if netSocket.Connected then
                        netSocket.Shutdown Sockets.SocketShutdown.Both

                    netSocket.Close()
                    netSocket.Dispose())
                    .CatchAll(logAndSuppress "socket close")
        }

    /// <summary>Returns whether the connection is currently active.</summary>
    /// <returns><c>true</c> if the connection is active; <c>false</c> otherwise.</returns>
    member _.IsConnected() = netSocket.Connected

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
    /// <remarks>Prefer Close() for effectful cleanup with proper error handling.</remarks>
    member _.Dispose() =
        fio {
            do!
                fromFunc (fun () ->
                    try
                        stream.Dispose()
                        netSocket.Dispose()
                    with exn ->
                        eprintfn $"[Socket] Error during synchronous dispose: {exn.Message}")
        }

    /// <summary>Builds the synchronous dispose logic, releasing the stream and socket resources.</summary>
    /// <param name="disposing">Whether this call originates from an explicit <c>Dispose</c> rather than a finalizer.</param>
    member private _.Dispose(disposing: bool) =
        if not disposed then
            disposed <- true

            if disposing then
                try
                    stream.Dispose()
                    netSocket.Dispose()
                with exn ->
                    eprintfn $"[Socket] Error during dispose: {exn.Message}"

    /// <summary>Provides resource cleanup for the socket.</summary>
    interface IDisposable with
        /// <summary>Transforms the socket by releasing its stream and network resources and suppressing finalization.</summary>
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize() = this.Dispose false
