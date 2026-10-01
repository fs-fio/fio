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

    let mutable private pendingLine = ValueNone

    let mutable private pendingKey = ValueNone

    let private serve (tcs: TaskCompletionSource<'T>) (stash: byref<'T voption>) (read: unit -> 'T) =
        match stash with
        | ValueSome value ->
            stash <- ValueNone

            if not (tcs.TrySetResult value) then
                stash <- ValueSome value
        | ValueNone ->
            let mutable value = Unchecked.defaultof<'T>
            let mutable thrown = null

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

    // The runtime's await is what interrupts the fiber; the registration only marks the request abandoned so the
    // reader stashes its input.
    let private awaitStdin (request: TaskCompletionSource<'T> -> StdinReader.Request) (onError: exn -> 'E) =
        FIO.cancellationToken().FlatMap <| fun cancellationToken ->
            let tcs = TaskCompletionSource<'T> TaskCreationOptions.RunContinuationsAsynchronously
            let registration = cancellationToken.Register(fun () -> tcs.TrySetCanceled cancellationToken |> ignore)
            StdinReader.enqueue (request tcs)

            (FIO.awaitTask tcs.Task onError)
                .Ensuring(FIO.succeedWith (fun () -> registration.Dispose()))

    /// Returns an effect that writes formatted text to standard output.
    let print<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintf Console.Out format) onError

    /// Returns an effect that writes formatted text followed by a newline to standard output.
    let printLine<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintfn Console.Out format) onError

    /// Returns an effect that reads a line from standard input, failing through onError with
    /// <c>EndOfStreamException</c> at end of input. Input typed for an interrupted read is delivered to the next one.
    let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
        awaitStdin StdinReader.Line onError

    /// Returns an effect that reads a line from standard input, yielding None at end of input; interruption behaves
    /// as in readLine.
    let tryReadLine<'E> (onError: exn -> 'E) : FIO<string option, 'E> =
        (awaitStdin StdinReader.Line id).Map(Some).CatchAll(function
            | :? IO.EndOfStreamException -> FIO.succeed None
            | ex -> FIO.fail (onError ex))

    /// Returns an effect that reads the next key press, without echoing it when intercept is true; fails through
    /// onError when input is redirected. A key typed for an interrupted read is delivered to the next one.
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
