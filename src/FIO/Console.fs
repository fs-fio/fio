namespace FIO.Console

open FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent

// Stdin is process-global and its reads block, so one background thread owns every read. A read whose
// fiber gave up still completes here; its input is stashed for the next request of the same kind.
module private StdinReader =

    type Request =
        | Line of TaskCompletionSource<string>
        | Key of intercept: bool * TaskCompletionSource<ConsoleKeyInfo>

    let private requests = new BlockingCollection<Request>()

    let mutable private pendingLine: string voption = ValueNone

    let mutable private pendingKey: ConsoleKeyInfo voption = ValueNone

    let private serve (tcs: TaskCompletionSource<'T>) (stash: byref<'T voption>) (read: unit -> 'T) =
        match stash with
        | ValueSome value ->
            stash <- ValueNone

            if not (tcs.TrySetResult value) then
                stash <- ValueSome value
        | ValueNone ->
            let mutable value = Unchecked.defaultof<'T>
            let mutable thrown: exn = null

            try
                value <- read ()
            with ex ->
                thrown <- ex

            if not (isNull thrown) then
                tcs.TrySetException thrown |> ignore
            elif not (tcs.TrySetResult value) then
                stash <- ValueSome value

    let private readLineOrEnd () =
        match Console.In.ReadLine() with
        | null -> raise (IO.EndOfStreamException "End of standard input")
        | line -> line

    let private loop () =
        for request in requests.GetConsumingEnumerable() do
            match request with
            | Line tcs -> serve tcs &pendingLine readLineOrEnd
            | Key(intercept, tcs) -> serve tcs &pendingKey (fun () -> Console.ReadKey intercept)

    let private thread =
        lazy (Thread(loop, IsBackground = true, Name = "FIO stdin reader").Start())

    let enqueue (request: Request) =
        thread.Force()
        requests.Add request

[<RequireQualifiedAccess>]
module Console =

    // The registration is not what interrupts the fiber — the runtime's await is. It marks the
    // request abandoned so the reader stashes its input instead of dropping it.
    let private awaitStdin (request: TaskCompletionSource<'T> -> StdinReader.Request) (onError: exn -> 'E) =
        FIO.cancellationToken().FlatMap <| fun cancelToken ->
            let tcs = TaskCompletionSource<'T> TaskCreationOptions.RunContinuationsAsynchronously
            let registration = cancelToken.Register(fun () -> tcs.TrySetCanceled cancelToken |> ignore)
            StdinReader.enqueue (request tcs)

            (FIO.awaitTask tcs.Task onError)
                .Ensuring(FIO.succeedWith (fun () -> registration.Dispose()))

    /// Returns an effect that writes formatted text to standard output.
    let print<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintf Console.Out format) onError

    /// Returns an effect that writes formatted text followed by a newline to standard output.
    let printLine<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintfn Console.Out format) onError

    /// Returns an effect that reads a line from standard input, failing through onError with an
    /// <c>EndOfStreamException</c> at end of input. The fiber can be interrupted while waiting; input typed for an
    /// interrupted read goes to the next one, so do not mix this with direct <c>System.Console.ReadLine</c> calls
    /// once a read has been interrupted.
    let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
        awaitStdin StdinReader.Line onError

    /// Returns an effect that reads the next key press, without echoing it when intercept is true; fails when
    /// input is redirected. The fiber can be interrupted while waiting; a key typed for an interrupted read
    /// goes to the next one, echoed or not as the interrupted read asked.
    let readKey<'E> (intercept: bool) (onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
        awaitStdin (fun tcs -> StdinReader.Key(intercept, tcs)) onError

    /// Returns an effect that writes text to standard output.
    let write<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Write text) onError

    /// Returns an effect that writes text followed by a newline to standard output.
    let writeLine<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.WriteLine text) onError

    /// Returns an effect that clears the console.
    let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Clear()) onError
