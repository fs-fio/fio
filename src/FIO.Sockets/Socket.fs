namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Threading
open System.Threading.Tasks

// What a receive finds in the bytes buffered so far: its item, how much room the next read needs, or a reason to fail.
[<Struct; NoComparison; NoEquality>]
type private Take<'T> =
    | Taken of value: 'T * consumed: int
    | Wanted of free: int
    | Rejected of error: SocketError

/// An open TCP socket connection for sending and receiving typed messages.
type Socket internal (netSocket: Sockets.Socket, config: SocketConfig) =

    let stream = new Sockets.NetworkStream(netSocket, ownsSocket = false)

    let cleanupLock = obj ()

    [<VolatileField>]
    let mutable disposed = false

    // Not SO_RCVBUF itself: accepted sockets inherit the OS value, which can pass the large-object threshold.
    let initialCapacity = if config.ReceiveBufferSize > 0 then min config.ReceiveBufferSize 16384 else 8192

    // buffer[head .. head + count) holds bytes not yet handed out; the one read in flight belongs to the socket and writes
    // behind them, so a receive that gives up leaves both for the next.
    let mutable buffer : byte[] = null

    let mutable head = 0

    let mutable count = 0

    let mutable pendingRead : Task<int> = null

    // Ends, never faulting, with the read in flight.
    let mutable pendingEnded : Task = null

    let mutable readFailure : exn = null

    let mutable ended = false

    // Receives take turns, so each takes whole items; a receive that was interrupted hands its turn on as it unwinds.
    let receiveLock = new SemaphoreSlim(1, 1)

    let releaseWhenGranted (permit: Task) =
        if permit.IsCompletedSuccessfully then
            receiveLock.Release() |> ignore
        else
            permit.ContinueWith(
                (fun (granted: Task) -> if granted.IsCompletedSuccessfully then receiveLock.Release() |> ignore),
                TaskContinuationOptions.ExecuteSynchronously)
            |> ignore

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

                match pendingRead with
                | null -> ()
                | read ->
                    read.ContinueWith(
                        (fun (ended: Task<int>) -> ended.Exception |> ignore),
                        TaskContinuationOptions.OnlyOnFaulted ||| TaskContinuationOptions.ExecuteSynchronously)
                    |> ignore

    // Set when a receive rejects what the peer sent (a line or a frame over its limit, a negative frame length): the
    // stream cannot be read past it, so every later receive fails.
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

    // An interrupted write must be cancelled before its token source goes away, or it would go on writing; the source
    // is disposed once the write has ended.
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

                        let release () =
                            if task.IsCompleted then
                                linked.Dispose()
                            else
                                try
                                    linked.Cancel()
                                with _ ->
                                    ()

                                task.ContinueWith(
                                    (fun (ended: Task) ->
                                        ended.Exception |> ignore
                                        linked.Dispose()),
                                    TaskContinuationOptions.ExecuteSynchronously)
                                |> ignore

                        (FIO.awaitUnitTask task mapError).Ensuring(FIO.attempt release SocketError.fromException)
        }

    let makeRoom (wanted: int) =
        if isNull buffer then
            buffer <- Array.zeroCreate (max initialCapacity wanted)
        else
            if count = 0 then
                head <- 0

                if buffer.Length > 4 * initialCapacity && wanted <= initialCapacity then
                    buffer <- Array.zeroCreate initialCapacity

            if buffer.Length - (head + count) < wanted then
                if buffer.Length - count >= wanted then
                    Buffer.BlockCopy(buffer, head, buffer, 0, count)
                else
                    let grown = Array.zeroCreate<byte> (max (buffer.Length * 2) (count + wanted))
                    Buffer.BlockCopy(buffer, head, grown, 0, count)
                    buffer <- grown

                head <- 0

    // How much room to ask for when an item still needs the given number of bytes: no more than has already arrived,
    // so a peer has to send data to make the buffer grow.
    let roomFor (missing: int) =
        min missing (max initialCapacity count)

    let startRead () =
        let offset = head + count

        pendingRead <-
            try
                stream.ReadAsync(buffer, offset, buffer.Length - offset, CancellationToken.None)
            with ex ->
                Task.FromException<int> ex

        pendingEnded <- pendingRead.ContinueWith(Action<Task<int>>(ignore), TaskContinuationOptions.ExecuteSynchronously)

    // Takes a finished read into the buffer. A failure is kept until no buffered item can be taken.
    let harvest () =
        match pendingRead with
        | null -> ()
        | read when not read.IsCompleted -> ()
        | read ->
            pendingRead <- null
            pendingEnded <- null

            if read.IsCompletedSuccessfully then
                if read.Result = 0 then ended <- true
                else count <- count + read.Result
            else
                readFailure <-
                    match read.Exception with
                    | null -> OperationCanceledException() :> exn
                    | aggregate ->
                        match aggregate.InnerException with
                        | null -> aggregate :> exn
                        | inner -> inner

    // True once the read has ended, false when the time ran out or the receive gave up first; never fails.
    let endsWithin (readEnded: Task) (remainingMs: int64) (cancellationToken: CancellationToken) : Task<bool> =
        task {
            do!
                readEnded
                    .WaitAsync(TimeSpan.FromMilliseconds(float remainingMs), cancellationToken)
                    .ConfigureAwait ConfigureAwaitOptions.SuppressThrowing

            return readEnded.IsCompleted
        }

    let timedOut () : FIO<'T, SocketError> =
        FIO.fail (TimeoutError $"Operation timed out after {config.ReceiveTimeout} ms")

    // Each receive waits on a promise of its own, which its interruption removes, so a receive that gives up leaves
    // nothing attached to the read.
    let rec receiveLoop
        (deadline: int64)
        (cancellationToken: CancellationToken)
        (taken: int ref)
        (tryTake: bool -> Take<'T>)
        : FIO<'T, SocketError> =
        FIO.suspend <| fun () ->
            if disposed then
                FIO.fail (ConnectionClosed "Socket is not connected")
            else
                harvest ()

                match tryTake ended with
                | Taken(value, consumed) ->
                    head <- head + consumed
                    count <- count - consumed
                    taken.Value <- consumed
                    FIO.succeed value
                | Rejected error -> FIO.fail error
                | Wanted _ when not (isNull readFailure) ->
                    let failure = readFailure
                    readFailure <- null
                    FIO.fail (SocketError.fromException failure)
                | Wanted _ when ended -> FIO.fail (ConnectionClosed "Connection closed by peer")
                | Wanted free ->
                    if isNull pendingRead then
                        makeRoom free
                        startRead ()

                    let readEnded = pendingEnded

                    if readEnded.IsCompleted then
                        receiveLoop deadline cancellationToken taken tryTake
                    elif deadline = 0L then
                        (FIO.awaitUnitTask (readEnded.WaitAsync cancellationToken) SocketError.fromException).FlatMap(fun () ->
                            receiveLoop deadline cancellationToken taken tryTake)
                    else
                        let remaining = deadline - Environment.TickCount64

                        if remaining <= 0L then
                            timedOut ()
                        else
                            (FIO.awaitTask (endsWithin readEnded remaining cancellationToken) SocketError.fromException).FlatMap(fun onTime ->
                                if onTime then receiveLoop deadline cancellationToken taken tryTake
                                else timedOut ())

    // What a receive took, decoded inside the region, is put back when its fiber was interrupted before using it; the
    // turn is handed on only after that, so no other receive sees the gap.
    let receive (tryTake: bool -> Take<'T>) (decode: 'T -> FIO<'U, SocketError>) : FIO<'U, SocketError> =
        FIO.cancellationToken().FlatMap <| fun cancellationToken ->
            let taken = ref 0

            let enter = FIO.succeedWith (fun () -> receiveLock.WaitAsync cancellationToken)

            let leave (permit: Task) =
                FIO.succeedWith <| fun () ->
                    if taken.Value > 0 && cancellationToken.IsCancellationRequested then
                        head <- head - taken.Value
                        count <- count + taken.Value

                    releaseWhenGranted permit

            let run (permit: Task) =
                (FIO.awaitUnitTask permit SocketError.fromException).FlatMap(fun () ->
                    ensureInSync.FlatMap(fun () ->
                        let deadline =
                            if config.ReceiveTimeout > 0 then Environment.TickCount64 + int64 config.ReceiveTimeout
                            else 0L

                        (receiveLoop deadline cancellationToken taken tryTake).FlatMap decode))

            FIO.acquireReleaseWith enter leave run

    let takeUpTo (maxBytes: int) (ended: bool) =
        if count > 0 then
            let length = min count maxBytes
            Taken((buffer.AsSpan(head, length).ToArray(), length), length)
        elif ended then
            Rejected(ConnectionClosed "Connection closed by peer")
        else
            Wanted(roomFor maxBytes)

    let receiveLine (maxBytes: int) (decode: byte[] -> FIO<'U, SocketError>) : FIO<'U, SocketError> =
        if maxBytes <= 0 then
            FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))
        else
            FIO.suspend <| fun () ->
                let scanned = ref 0

                let takeLine (ended: bool) =
                    let limit = min count maxBytes

                    let newline =
                        if limit > scanned.Value then
                            Array.IndexOf(buffer, byte '\n', head + scanned.Value, limit - scanned.Value)
                        else
                            -1

                    if newline >= 0 then
                        let length = newline - head + 1
                        Taken(buffer.AsSpan(head, length).ToArray(), length)
                    else
                        scanned.Value <- limit

                        if count >= maxBytes then
                            poison $"a line longer than {maxBytes} bytes"
                            Rejected(BufferOverflow(maxBytes + 1, maxBytes))
                        elif ended then
                            Rejected(ConnectionClosed $"Connection closed after {count} bytes of an unfinished line")
                        else
                            Wanted(roomFor (maxBytes - count))

                receive takeLine decode

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
        if maxBytes <= 0 then
            FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))
        else
            receive (takeUpTo maxBytes) FIO.succeed

    /// Receives exactly the given number of bytes from this socket, blocking until they arrive.
    member _.ReceiveExactly (numBytes: int) : FIO<byte[], SocketError> =
        if numBytes <= 0 then
            FIO.fail (InvalidState("positive byte count", $"{numBytes}"))
        else
            let takeExactly (ended: bool) =
                if count >= numBytes then
                    Taken(buffer.AsSpan(head, numBytes).ToArray(), numBytes)
                elif ended then
                    Rejected(ConnectionClosed $"Connection closed after {count} of {numBytes} bytes")
                else
                    Wanted(roomFor (numBytes - count))

            receive takeExactly FIO.succeed

    /// Sends a value over this socket, encoded with the given codec.
    member this.Send<'A> (codec: SocketCodec<'A>, value: 'A) : FIO<unit, SocketError> =
        fio {
            let! bytes = codec.Encode value
            do! this.SendBytes bytes
        }

    /// Receives a value from this socket, decoded with the given codec.
    member _.Receive<'A> (codec: SocketCodec<'A>, maxBytes: int) : FIO<'A, SocketError> =
        if maxBytes <= 0 then
            FIO.fail (InvalidState("positive buffer size", $"{maxBytes}"))
        else
            receive (takeUpTo maxBytes) (fun (bytes, _) -> codec.Decode bytes)

    /// Sends a UTF-8 string over this socket.
    member this.SendString (str: string) : FIO<unit, SocketError> =
        this.Send(Codec.string, str)

    /// Receives a UTF-8 string from this socket.
    member this.ReceiveString (maxBytes: int) : FIO<string, SocketError> =
        this.Receive(Codec.string, maxBytes)

    /// Sends a newline-terminated string over this socket.
    member this.SendLine (line: string) : FIO<unit, SocketError> =
        this.Send(Codec.line, line)

    /// Receives a single newline-terminated line from this socket; maxBytes bounds the line, newline included.
    member _.ReceiveLine (maxBytes: int) : FIO<string, SocketError> =
        receiveLine maxBytes Codec.line.Decode

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
    member _.ReceiveJsonLine<'A> (maxBytes: int) : FIO<'A, SocketError> =
        receiveLine maxBytes (Codec.jsonLine<'A> None).Decode

    /// Sends a value as a length-prefixed frame, encoded with the given codec.
    member this.SendFramed<'A> (codec: SocketCodec<'A>, value: 'A) : FIO<unit, SocketError> =
        fio {
            let! frame = (Codec.lengthPrefixed codec).Encode value
            do! this.SendBytes frame
        }

    /// Receives a length-prefixed frame, decoded with the given codec, rejecting frames larger than the maximum size.
    member _.ReceiveFramed<'A> (codec: SocketCodec<'A>, maxFrameSize: int) : FIO<'A, SocketError> =
        let takeFrame (ended: bool) =
            if count < 4 then
                if ended then Rejected(ConnectionClosed $"Connection closed after {count} of 4 bytes")
                else Wanted(4 - count)
            else
                let frameLength = IPAddress.NetworkToHostOrder(BitConverter.ToInt32(buffer, head))

                if frameLength < 0 then
                    poison "a negative frame length"
                    Rejected(CodecError($"Negative frame length: {frameLength}", ArgumentOutOfRangeException "length"))
                elif frameLength > maxFrameSize then
                    poison $"a {frameLength}-byte frame over the {maxFrameSize}-byte limit"
                    Rejected(BufferOverflow(frameLength, maxFrameSize))
                elif count - 4 >= frameLength then
                    Taken(buffer.AsSpan(head + 4, frameLength).ToArray(), 4 + frameLength)
                elif ended then
                    Rejected(ConnectionClosed $"Connection closed after {count - 4} of {frameLength} bytes")
                else
                    Wanted(roomFor (frameLength - (count - 4)))

        receive takeFrame codec.Decode

    /// Receives a length-prefixed frame, decoded with the given codec, using a default 16 MB frame limit.
    member this.ReceiveFramed<'A> (codec: SocketCodec<'A>) : FIO<'A, SocketError> =
        this.ReceiveFramed(codec, 16 * 1024 * 1024)

    /// Closes this socket, suppressing errors: gracefully, or with a reset when linger is enabled with a zero timeout.
    member _.Close () : FIO<unit, SocketError> =
        fio {
            do! (attempt <| fun () ->
                    if not disposed then
                        try
                            netSocket.LingerState <- Sockets.LingerOption(config.LingerEnabled, config.LingerTimeout)
                        with _ ->
                            ()

                        // A zero linger resets the connection; a shutdown first would end it gracefully instead.
                        if netSocket.Connected && not (config.LingerEnabled && config.LingerTimeout = 0) then
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
