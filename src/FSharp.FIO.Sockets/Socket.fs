namespace FSharp.FIO.Sockets

open FSharp.FIO.DSL

open System
open System.Buffers
open System.Net

/// <summary>
/// A functional, effectful, and type-safe abstraction over a TCP socket connection.
/// </summary>
type Socket internal (netSocket: Sockets.Socket, config: SocketConfig) =

    let stream = new Sockets.NetworkStream(netSocket, ownsSocket = false)

    // Internal: Logs an error and suppresses it (used for cleanup)
    let logAndSuppress (context: string) (err: SocketError) : FIO<unit, SocketError> =
        fio {
            let str = SocketError.ToString err
            do! FIO.attempt((fun () ->
                eprintfn $"[Socket] Error during {context}: {str}"), SocketError.FromException)
            return ()
        }

    // Partially applied functions for consistent error handling
    let fromFunc (func: unit -> 'T) : FIO<'T, SocketError> =
        FIO.attempt(func, SocketError.FromException)

    let awaitTask (task: Threading.Tasks.Task) : FIO<unit, SocketError> =
        FIO.awaitTask(task, SocketError.FromException)

    let awaitTaskT (task: Threading.Tasks.Task<'T>) : FIO<'T, SocketError> =
        FIO.awaitGenericTask(task, SocketError.FromException)

    /// <summary>
    /// Sends raw bytes over the socket.
    /// </summary>
    /// <param name="buffer">The byte array to send.</param>
    member _.SendBytes(buffer: byte[]) : FIO<unit, SocketError> =
        fio {
            if not netSocket.Connected then
                return! FIO.fail(ConnectionClosed "Socket is not connected")
            do! awaitTask (stream.WriteAsync(buffer, 0, buffer.Length))
            do! awaitTask (stream.FlushAsync())
        }

    /// <summary>
    /// Receives up to maxBytes from the socket.
    /// Returns the buffer and the actual number of bytes read.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    member _.ReceiveBytes(maxBytes: int) : FIO<byte[] * int, SocketError> =
        fio {
            if maxBytes <= 0 then
                return! FIO.fail(InvalidState("positive buffer size", $"{maxBytes}"))

            if not netSocket.Connected then
                return! FIO.fail(ConnectionClosed "Socket is not connected")

            // Use ArrayPool for efficient buffer management
            let! pooledBuffer = fromFunc (fun () -> ArrayPool<byte>.Shared.Rent maxBytes)

            let readAndCopy = fio {
                let! bytesRead = awaitTaskT (stream.ReadAsync(pooledBuffer, 0, maxBytes))

                if bytesRead = 0 then
                    return! FIO.fail(ConnectionClosed "Connection closed by peer")

                // Copy exact bytes to result array (can't return pooled buffer)
                let result = Array.zeroCreate<byte> bytesRead
                Buffer.BlockCopy(pooledBuffer, 0, result, 0, bytesRead)
                return result, bytesRead
            }

            // Ensure buffer is returned to pool even on error
            return! readAndCopy.Ensuring(fromFunc (fun () -> ArrayPool<byte>.Shared.Return pooledBuffer))
        }

    /// <summary>
    /// Receives exactly the specified number of bytes.
    /// Blocks until all bytes are received or connection closes.
    /// </summary>
    /// <param name="numBytes">Exact number of bytes to receive.</param>
    member _.ReceiveExactly(numBytes: int) : FIO<byte[], SocketError> =
        fio {
            if numBytes <= 0 then
                return! FIO.fail(InvalidState("positive byte count", $"{numBytes}"))

            // Use ArrayPool for efficient buffer management
            let! pooledBuffer = fromFunc (fun () -> ArrayPool<byte>.Shared.Rent numBytes)

            let readLoop = fio {
                let mutable totalRead = 0

                while totalRead < numBytes do
                    let! bytesRead = awaitTaskT (stream.ReadAsync(pooledBuffer, totalRead, numBytes - totalRead))

                    if bytesRead = 0 then
                        return! FIO.fail(ConnectionClosed $"Connection closed after {totalRead} of {numBytes} bytes")

                    totalRead <- totalRead + bytesRead

                // Copy exact bytes to result array (pooled buffer may be larger)
                let result = Array.zeroCreate<byte> numBytes
                Buffer.BlockCopy(pooledBuffer, 0, result, 0, numBytes)
                return result
            }

            // Ensure buffer is returned to pool even on error
            return! readLoop.Ensuring(fromFunc (fun () -> ArrayPool<byte>.Shared.Return pooledBuffer))
        }

    /// <summary>
    /// Sends data using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for encoding.</param>
    /// <param name="value">The value to send.</param>
    member this.Send<'T>(codec: SocketCodec<'T>, value: 'T) : FIO<unit, SocketError> =
        fio {
            let! bytes = codec.Encode value
            do! this.SendBytes bytes
        }

    /// <summary>
    /// Receives data using a codec.
    /// </summary>
    /// <param name="codec">The codec to use for decoding.</param>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    member this.Receive<'T>(codec: SocketCodec<'T>, maxBytes: int) : FIO<'T, SocketError> =
        fio {
            let! bytes, bytesRead = this.ReceiveBytes maxBytes
            let actualBytes = bytes.[0..bytesRead-1]
            return! codec.Decode actualBytes
        }

    /// <summary>
    /// Sends a string (UTF-8).
    /// </summary>
    /// <param name="str">The string to send.</param>
    member this.SendString(str: string) : FIO<unit, SocketError> =
        this.Send(Codec.string, str)

    /// <summary>
    /// Receives a string (UTF-8).
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    member this.ReceiveString(maxBytes: int) : FIO<string, SocketError> =
        this.Receive(Codec.string, maxBytes)

    /// <summary>
    /// Sends a line-delimited string.
    /// </summary>
    /// <param name="line">The line to send.</param>
    member this.SendLine(line: string) : FIO<unit, SocketError> =
        this.Send(Codec.line, line)

    /// <summary>
    /// Receives a line-delimited string.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    member this.ReceiveLine(maxBytes: int) : FIO<string, SocketError> =
        this.Receive(Codec.line, maxBytes)

    /// <summary>
    /// Sends JSON.
    /// </summary>
    /// <param name="value">The value to send as JSON.</param>
    member this.SendJson<'T>(value: 'T) : FIO<unit, SocketError> =
        this.Send(Codec.json, value)

    /// <summary>
    /// Receives JSON.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    member this.ReceiveJson<'T>(maxBytes: int) : FIO<'T, SocketError> =
        this.Receive(Codec.json, maxBytes)

    /// <summary>
    /// Sends line-delimited JSON.
    /// </summary>
    /// <param name="value">The value to send as JSON.</param>
    member this.SendJsonLine<'T>(value: 'T) : FIO<unit, SocketError> =
        this.Send(Codec.jsonLine None, value)

    /// <summary>
    /// Receives line-delimited JSON.
    /// </summary>
    /// <param name="maxBytes">Maximum number of bytes to receive.</param>
    member this.ReceiveJsonLine<'T>(maxBytes: int) : FIO<'T, SocketError> =
        this.Receive(Codec.jsonLine None, maxBytes)

    /// <summary>
    /// Closes and disposes socket resources according to configured linger options.
    /// This is the preferred method for closing sockets in FIO code.
    /// </summary>
    member _.Close() : FIO<unit, SocketError> =
        fio {
            do! fromFunc(fun () ->
                // Apply linger configuration from SocketConfig
                netSocket.LingerState <- Sockets.LingerOption(config.LingerEnabled, config.LingerTimeout)
                if stream <> null then
                    stream.Close()
                    stream.Dispose()
                ).CatchAll(logAndSuppress "stream close")

            do! fromFunc(fun () ->
                if netSocket.Connected then
                    netSocket.Shutdown Sockets.SocketShutdown.Both
                netSocket.Close()
                netSocket.Dispose()
                ).CatchAll(logAndSuppress "socket close")
        }


    /// <summary>
    /// Checks if the socket is connected.
    /// </summary>
    member _.IsConnected() : bool =
        netSocket.Connected

    /// <summary>
    /// Gets the remote endpoint of the socket.
    /// </summary>
    member _.GetRemoteEndPoint() : FIO<EndPoint, SocketError> =
        fromFunc (fun () -> netSocket.RemoteEndPoint)

    /// <summary>
    /// Gets the local endpoint of the socket.
    /// </summary>
    member _.GetLocalEndPoint() : FIO<EndPoint, SocketError> =
        fromFunc (fun () -> netSocket.LocalEndPoint)

    /// <summary>
    /// Gets the socket configuration.
    /// </summary>
    member _.GetConfig() : SocketConfig =
        config

    /// <summary>
    /// Disposes the underlying socket and releases all resources.
    /// This is a synchronous FIO effect wrapper for cleanup.
    /// Note: Prefer using Close() in FIO code for async cleanup with proper error handling.
    /// </summary>
    member _.Dispose() : FIO<unit, SocketError> =
        fio {
            do! fromFunc(fun () ->
                try
                    stream.Dispose()
                    netSocket.Dispose()
                with exn ->
                    eprintfn $"[Socket] Error during synchronous dispose: {exn.Message}"
                )
        }

    interface IDisposable with
        member _.Dispose() =
            try
                stream.Dispose()
                netSocket.Dispose()
            with exn ->
                eprintfn $"[Socket] Error during IDisposable.Dispose: {exn.Message}"
