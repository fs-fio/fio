namespace FIO.Console

open FIO.DSL

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent

// Stdin is process-global and its reads block, so one background thread owns every read. Input that reached no
// reader, because its fiber gave up, waits here for the next request.
module internal StdinReader =

    type Request =
        | Line of TaskCompletionSource<string>
        | Key of intercept: bool * TaskCompletionSource<ConsoleKeyInfo>

    let private requests = new BlockingCollection<Request>()

    let private undeliveredLines = ConcurrentQueue<string>()

    let private undeliveredKeys = ConcurrentQueue<ConsoleKeyInfo>()

    // Values a fiber received and then gave back, owed before anything read later.
    let private returnedLines = ConcurrentQueue<string>()

    let private returnedKeys = ConcurrentQueue<ConsoleKeyInfo>()

    // Replaced by tests: Console.ReadKey cannot be fed without a terminal.
    let mutable readKey : bool -> ConsoleKeyInfo = fun intercept -> Console.ReadKey intercept

    let private readLineOrEnd () =
        match Console.In.ReadLine() with
        | null -> raise (IO.EndOfStreamException "End of standard input")
        | line -> line

    // A key that an abandoned key read took was the start of the line now being read.
    let private readLineAfter (key: ConsoleKeyInfo) =
        if key.Key = ConsoleKey.Enter then ""
        elif Char.IsControl key.KeyChar then readLineOrEnd ()
        else string key.KeyChar + readLineOrEnd ()

    // Only this thread dequeues, so the value it peeked is the one it removes. A request abandoned before its turn
    // reads nothing.
    let private serve
        (tcs: TaskCompletionSource<'T>)
        (returned: ConcurrentQueue<'T>)
        (undelivered: ConcurrentQueue<'T>)
        (read: unit -> 'T) =
        let mutable value = Unchecked.defaultof<'T>

        if returned.TryPeek &value then
            if tcs.TrySetResult value then
                returned.TryDequeue &value |> ignore
        elif undelivered.TryPeek &value then
            if tcs.TrySetResult value then
                undelivered.TryDequeue &value |> ignore
        elif not tcs.Task.IsCompleted then
            let mutable thrown = null

            try
                value <- read ()
            with ex ->
                thrown <- ex

            if not (isNull thrown) then
                tcs.TrySetException thrown |> ignore
            elif not (tcs.TrySetResult value) then
                undelivered.Enqueue value

    // The key is removed only once its line was read, so a failed read leaves it for the next request.
    let private readLine () =
        let mutable key = Unchecked.defaultof<ConsoleKeyInfo>

        if returnedKeys.TryPeek &key then
            let line = readLineAfter key
            returnedKeys.TryDequeue &key |> ignore
            line
        elif undeliveredKeys.TryPeek &key then
            let line = readLineAfter key
            undeliveredKeys.TryDequeue &key |> ignore
            line
        else
            readLineOrEnd ()

    let private loop () =
        for request in requests.GetConsumingEnumerable() do
            match request with
            | Line tcs -> serve tcs returnedLines undeliveredLines readLine
            | Key(intercept, tcs) -> serve tcs returnedKeys undeliveredKeys (fun () -> readKey intercept)

    let private thread =
        lazy (Thread(loop, IsBackground = true, Name = "FIO stdin reader").Start())

    let enqueue (request: Request) =
        thread.Force()
        requests.Add request

    let giveBackLine (line: string) =
        returnedLines.Enqueue line

    let giveBackKey (key: ConsoleKeyInfo) =
        returnedKeys.Enqueue key

[<RequireQualifiedAccess>]
module Console =

    // The finalizer marks the request abandoned as well as the registration, which a runtime resuming inline may skip;
    // input that arrived for an interrupted fiber goes back, as the interruption would drop it when the region ends.
    let private awaitStdin
        (request: TaskCompletionSource<'T> -> StdinReader.Request)
        (giveBack: 'T -> unit)
        (onError: exn -> 'E) =
        FIO.cancellationToken().FlatMap <| fun cancellationToken ->
            let tcs = TaskCompletionSource<'T> TaskCreationOptions.RunContinuationsAsynchronously
            let registration = cancellationToken.Register(fun () -> tcs.TrySetCanceled cancellationToken |> ignore)
            StdinReader.enqueue (request tcs)

            (FIO.awaitTask tcs.Task onError)
                .Ensuring(FIO.succeedWith (fun () ->
                    registration.Dispose()

                    if not (tcs.TrySetCanceled())
                       && cancellationToken.IsCancellationRequested
                       && tcs.Task.IsCompletedSuccessfully then
                        giveBack tcs.Task.Result))

    /// Returns an effect that writes formatted text to standard output.
    let print<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintf Console.Out format) onError

    /// Returns an effect that writes formatted text followed by a newline to standard output.
    let printLine<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintfn Console.Out format) onError

    /// Returns an effect that reads a line from standard input, failing through onError with EndOfStreamException at end
    /// of input; input typed for an interrupted read goes to the next read, not to System.Console.ReadLine.
    let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
        awaitStdin StdinReader.Line StdinReader.giveBackLine onError

    /// Returns an effect that reads a line from standard input, yielding None at end of input; interruption behaves
    /// as in readLine.
    let tryReadLine<'E> (onError: exn -> 'E) : FIO<string option, 'E> =
        (awaitStdin StdinReader.Line StdinReader.giveBackLine id).Map(Some).CatchAll(function
            | :? IO.EndOfStreamException -> FIO.succeed None
            | ex -> FIO.fail (onError ex))

    /// Returns an effect that reads the next key press, echoed unless intercept is true; fails through onError when input
    /// is redirected. A key typed for an interrupted read goes to the next readKey, or begins the next readLine.
    let readKey<'E> (intercept: bool) (onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
        awaitStdin (fun tcs -> StdinReader.Key(intercept, tcs)) StdinReader.giveBackKey onError

    /// Returns an effect that writes text to standard output.
    let write<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Write text) onError

    /// Returns an effect that writes text followed by a newline to standard output.
    let writeLine<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.WriteLine text) onError

    /// Returns an effect that clears the console.
    let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Clear()) onError
