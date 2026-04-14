namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Buffers

/// <summary>
/// A functional, effectful, and type-safe abstraction over a TCP socket connection.
/// </summary>
type Socket internal (netSocket: Sockets.Socket, config: SocketConfig) =

    /// <summary>
    /// Network stream for reading/writing over the socket.
    /// </summary>
    let stream = new Sockets.NetworkStream(netSocket, ownsSocket = false)
    /// <summary>
    /// Tracks disposal state to prevent double-disposal.
    /// </summary>
    let mutable disposed = false

    /// <summary>
    /// Internal: Logs an error and suppresses it.
    /// Used for cleanup operations where errors should not propagate.
    /// </summary>
    /// <param name="context">The context description for the error.</param>
    /// <param name="err">The error to log.</param>
    /// <returns>Effect that logs the error and succeeds with unit.</returns>
    let logAndSuppress (context: string) (err: SocketError) =
        fio {
            let str = err.ToString()
            do! FIO.attempt ((fun () -> eprintfn $"[Socket] Error during {context}: {str}"), SocketError.fromException)
            return ()
        }

    /// <summary>
    /// Wraps a function as an FIO effect with socket error handling.
    /// </summary>
    /// <param name="func">The function to wrap.</param>
    /// <returns>Effect returning the function's result.</returns>
    let fromFunc (func: unit -> 'T) =
        FIO.attempt (func, SocketError.fromException)

    /// <summary>
    /// Awaits a Task as an FIO effect with socket error handling.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <returns>Effect that completes when the Task finishes.</returns>
    let awaitTask (task: Threading.Tasks.Task) =
        FIO.awaitTask (task, SocketError.fromException)

    /// <summary>
    /// Awaits a generic Task as an FIO effect with socket error handling.
    /// </summary>
    /// <param name="task">The Task to await.</param>
    /// <returns>Effect returning the Task's result.</returns>
    let awaitTaskT (task: Threading.Tasks.Task<'T>) =
        FIO.awaitGenericTask (task, SocketError.fromException)

    /// <summary>
    /// Sends raw bytes over the socket.
    /// </summary>
    /// <param name="buffer">The byte array to send.</param>
    /// <returns>Effect that sends the bytes.</returns>
    member _.SendBytes(buffer: byte[]) =
        fio {
            if not netSocket.Connected then
                return! FIO.fail (ConnectionClosed "Socket is not connected")

            do! awaitTask (stream.WriteAsync(buffer, 0, buffer.Length))
            do! awaitTask (stream.FlushAsync())
        }

    /// <summary>
    /// Receives up to maxBytes from the socket.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <returns>The buffer and the actual number of bytes read.</returns>
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

    /// <summary>
    /// Receives exactly the specified number of bytes.
    /// </summary>
    /// <param name="numBytes">Exact number of bytes to receive.</param>
    /// <returns>The byte array containing exactly the requested number of bytes.</returns>
    member _.ReceiveExactly(numBytes: int) =
        fio {
            if numBytes <= 0 then
                return! FIO.fail (InvalidState("positive byte count", $"{numBytes}"))

            let! pooledBuffer = fromFunc (fun () -> ArrayPool<byte>.Shared.Rent numBytes)

            let readLoop =
                fio {
                    let mutable totalRead = 0

                    while totalRead < numBytes do
                        let! bytesRead = awaitTaskT (stream.ReadAsync(pooledBuffer, totalRead, numBytes - totalRead))

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

    /// <summary>
    /// Sends data using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    /// <returns>Effect that sends the encoded value.</returns>
    member this.Send<'T>(codec: SocketCodec<'T>, value: 'T) =
        fio {
            let! bytes = codec.Encode value
            do! this.SendBytes bytes
        }

    /// <summary>
    /// Receives data using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <returns>The decoded value.</returns>
    member this.Receive<'T>(codec: SocketCodec<'T>, maxBytes: int) =
        fio {
            let! bytes, bytesRead = this.ReceiveBytes maxBytes
            let actualBytes = bytes.[0 .. bytesRead - 1]
            return! codec.Decode actualBytes
        }

    /// <summary>
    /// Sends a string (UTF-8).
    /// </summary>
    /// <param name="str">The string to send.</param>
    /// <returns>Effect that sends the string.</returns>
    member this.SendString(str: string) = this.Send(Codec.string, str)

    /// <summary>
    /// Receives a string (UTF-8).
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <returns>The received string.</returns>
    member this.ReceiveString(maxBytes: int) = this.Receive(Codec.string, maxBytes)

    /// <summary>
    /// Sends a line-delimited string.
    /// </summary>
    /// <param name="line">The line to send.</param>
    /// <returns>Effect that sends the line.</returns>
    member this.SendLine(line: string) = this.Send(Codec.line, line)

    /// <summary>
    /// Receives a line-delimited string.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <returns>The received line.</returns>
    member this.ReceiveLine(maxBytes: int) = this.Receive(Codec.line, maxBytes)

    /// <summary>
    /// Sends JSON.
    /// </summary>
    /// <param name="value">The value to send as JSON.</param>
    /// <returns>Effect that sends the JSON value.</returns>
    member this.SendJson<'T>(value: 'T) = this.Send(Codec.json, value)

    /// <summary>
    /// Receives JSON.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <returns>The deserialized JSON value.</returns>
    member this.ReceiveJson<'T>(maxBytes: int) = this.Receive<'T>(Codec.json, maxBytes)

    /// <summary>
    /// Sends line-delimited JSON.
    /// </summary>
    /// <param name="value">The value to send as JSON.</param>
    /// <returns>Effect that sends the JSON line.</returns>
    member this.SendJsonLine<'T>(value: 'T) = this.Send(Codec.jsonLine None, value)

    /// <summary>
    /// Receives line-delimited JSON.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    /// <returns>The deserialized JSON value.</returns>
    member this.ReceiveJsonLine<'T>(maxBytes: int) =
        this.Receive<'T>(Codec.jsonLine None, maxBytes)

    /// <summary>
    /// Closes and disposes socket resources according to configured linger options.
    /// This is the preferred method for closing sockets in FIO code.
    /// </summary>
    /// <returns>Effect that closes the socket.</returns>
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

    /// <summary>
    /// Checks if the socket is connected.
    /// </summary>
    /// <returns>True if the socket is connected, false otherwise.</returns>
    member _.IsConnected() = netSocket.Connected

    /// <summary>
    /// Gets the remote endpoint of the socket.
    /// </summary>
    /// <returns>The remote endpoint.</returns>
    member _.GetRemoteEndPoint() =
        fromFunc (fun () -> netSocket.RemoteEndPoint)

    /// <summary>
    /// Gets the local endpoint of the socket.
    /// </summary>
    /// <returns>The local endpoint.</returns>
    member _.GetLocalEndPoint() =
        fromFunc (fun () -> netSocket.LocalEndPoint)

    /// <summary>
    /// Gets the socket configuration.
    /// </summary>
    /// <returns>The socket configuration.</returns>
    member _.GetConfig() = config

    /// <summary>
    /// Disposes the underlying socket and releases all resources.
    /// This is a synchronous FIO effect wrapper for cleanup.
    /// Note: Prefer using Close() in FIO code for async cleanup with proper error handling.
    /// </summary>
    /// <returns>Effect that disposes the socket.</returns>
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

    /// <summary>
    /// Releases resources based on disposal pattern.
    /// </summary>
    /// <param name="disposing">True if called from Dispose, false if from finalizer.</param>
    member private _.Dispose(disposing: bool) =
        if not disposed then
            disposed <- true

            if disposing then
                try
                    stream.Dispose()
                    netSocket.Dispose()
                with exn ->
                    eprintfn $"[Socket] Error during dispose: {exn.Message}"

    interface IDisposable with
        member this.Dispose() =
            this.Dispose true
            GC.SuppressFinalize this

    override this.Finalize() = this.Dispose false
