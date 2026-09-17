module FIO.Tests.ConsoleTests

open FIO.Tests.Utilities

open FIO.DSL
open FIO.Console
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.IO
open System.Threading
open System.Diagnostics

type private ThrowingWriter(message: string) =
    inherit StringWriter()
    override _.Write(_: char) : unit = raise (InvalidOperationException message)
    override _.Write(value: string) : unit =
        if isNull value then () else raise (InvalidOperationException message)

type private ThrowingReader(message: string) =
    inherit StringReader("")
    override _.Read() : int = raise (InvalidOperationException message)
    override _.ReadLine() : string = raise (InvalidOperationException message)

type private BlockingReader(gate: ManualResetEventSlim, line: string) =
    inherit StringReader("")
    override _.ReadLine() : string =
        gate.Wait()
        line

// Releases and drains the abandoned read afterwards, or its line would reach the next test's readLine.
let private withBlockingStdIn (line: string) (test: ManualResetEventSlim -> unit) =
    let originalIn = Console.In
    use gate = new ManualResetEventSlim(false)
    use reader = new BlockingReader(gate, line)
    Console.SetIn reader

    try
        test gate
    finally
        gate.Set()

        try
            (new DirectRuntime()).Run(Console.readLine<exn> id).UnsafeSuccess() |> ignore
        with _ ->
            ()

        Console.SetIn originalIn

type private Capture =
    { StdOut: StringWriter }

    member this.Output = this.StdOut.ToString()

let private withCapturedOut (test: Capture -> FIORuntime -> unit) (runtime: FIORuntime) =
    let originalOut = Console.Out
    let writer = new StringWriter()
    Console.SetOut writer

    try
        test { StdOut = writer } runtime
    finally
        Console.SetOut originalOut
        writer.Dispose()

let private withStdIn (input: string) (test: FIORuntime -> unit) (runtime: FIORuntime) =
    let originalIn = Console.In
    let reader = new StringReader(input)
    Console.SetIn reader

    try
        test runtime
    finally
        Console.SetIn originalIn
        reader.Dispose()

let private testCapturedOut name (f: Capture -> FIORuntime -> unit) =
    testList
        name
        [
            for rt in allRuntimes () -> testCase (rt.GetType().Name) (fun () -> withCapturedOut f rt)
        ]

let private testCapturedIn name (input: string) (f: FIORuntime -> unit) =
    testList
        name
        [
            for rt in allRuntimes () -> testCase (rt.GetType().Name) (fun () -> withStdIn input f rt)
        ]

// All console tests must run sequentially because System.Console has process-global state
[<Tests>]
let consoleTests =
    testSequenced (
        testList
            "Console"
            [

                testCapturedOut "print - writes formatted text to stdout without newline" (fun cap runtime ->
                    runtime.Run(Console.print "hello" id).UnsafeSuccess()

                    Expect.equal cap.Output "hello" "Should write to stdout without newline")

                testCapturedOut "printLine - writes formatted text with newline to stdout" (fun cap runtime ->
                    runtime.Run(Console.printLine "line" id).UnsafeSuccess()

                    Expect.stringContains cap.Output "line" "Should write line content"
                    Expect.stringContains cap.Output Environment.NewLine "Should write trailing newline")

                testCapturedOut "print - interpolates format arguments" (fun cap runtime ->
                    runtime.Run(Console.print $"count={42}" id).UnsafeSuccess()

                    Expect.equal cap.Output "count=42" "Should render interpolated format")

                testCapturedIn "readLine - reads a line from stdin" "hello world" (fun runtime ->
                    let result = runtime.Run(Console.readLine id).UnsafeSuccess()

                    Expect.equal result "hello world" "Should read the queued input line")

                testCapturedIn "readLine - reads consecutive lines in order" "first\nsecond" (fun runtime ->
                    let effect =
                        fio {
                            let! first = Console.readLine id
                            let! second = Console.readLine id
                            return first, second
                        }

                    Expect.equal (runtime.Run(effect).UnsafeSuccess()) ("first", "second") "Lines should arrive in input order")

                testCapturedIn "readLine - fails through onError at end of input" "" (fun runtime ->
                    let effect = Console.readLine (fun ex -> ex.GetType().Name)

                    match runtime.Run(effect).UnsafeResult() with
                    | Failed name -> Expect.equal name "EndOfStreamException" "End of input should be a typed failure, not a null line"
                    | other -> failtest $"Expected Failed but got {other}")

                testAllRuntimes "readLine - an abandoned read that hit end of input leaves the next read at end of input" (fun runtime ->
                    let originalIn = Console.In
                    use gate = new ManualResetEventSlim(false)
                    use reader = { new StringReader("") with override _.ReadLine () = gate.Wait(); null }
                    Console.SetIn reader

                    try
                        let effect =
                            fio {
                                let! fiber = (Console.readLine id).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                return! fiber.InterruptAwaitNow ()
                            }

                        match runtime.Run(effect).UnsafeSuccess() with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}"

                        gate.Set()
                        let next = Console.readLine (fun ex -> ex.GetType().Name)

                        match runtime.Run(next).UnsafeResult() with
                        | Failed name -> Expect.equal name "EndOfStreamException" "Nothing is stashed for an abandoned read that hit end of input"
                        | other -> failtest $"Expected Failed but got {other}"
                    finally
                        Console.SetIn originalIn)

                testAllRuntimes "readLine - an interrupted read frees the fiber and keeps the line for the next read" (fun runtime ->
                    withBlockingStdIn "typed after the interrupt" (fun gate ->
                        let effect =
                            fio {
                                let! fiber = (Console.readLine id).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                return! fiber.InterruptAwaitNow ()
                            }

                        let sw = Stopwatch.StartNew()
                        let result = runtime.Run(effect).UnsafeSuccess()
                        sw.Stop()

                        match result with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got {other}"

                        Expect.isLessThan sw.Elapsed.TotalSeconds 5.0 "Interrupting a pending readLine must not wait for input"

                        gate.Set()
                        let next = runtime.Run(Console.readLine<exn> id).UnsafeSuccess()

                        Expect.equal next "typed after the interrupt" "The line typed for the interrupted read must reach the next read"))

                testCase "readLine - a pending read does not occupy an evaluation worker" (fun () ->
                    withBlockingStdIn "released" (fun gate ->
                        use runtime = new WorkStealingRuntime { WorkerConfig.Default with EvaluationWorkers = 1 }
                        let pending = runtime.Run(Console.readLine<exn> id)
                        Thread.Sleep 50

                        let other = runtime.Run(FIO.succeed 42)

                        Expect.isTrue
                            (other.Task().Wait(TimeSpan.FromSeconds 5.0))
                            "With one worker, an effect must still run while readLine is pending"

                        pending.Context.Interrupt(ExplicitInterrupt, "test finished")
                        gate.Set()
                        Expect.equal (runtime.Run(Console.readLine<exn> id).UnsafeSuccess()) "released" "The released line reaches the next read"))

                testAllRuntimes "readKey - fails through onError when input is redirected" (fun runtime ->
                    if Console.IsInputRedirected then
                        let effect = Console.readKey true (fun ex -> ex.GetType().Name)

                        match runtime.Run(effect).UnsafeResult() with
                        | Failed name -> Expect.equal name "InvalidOperationException" "Console.ReadKey rejects redirected input"
                        | other -> failtest $"Expected Failed but got {other}"
                    else
                        skiptest "standard input is a terminal, so readKey would wait for a key press")

                testAllRuntimes "readLine - maps exception with custom error handler" (fun runtime ->
                    let originalIn = Console.In
                    let throwingReader = new ThrowingReader("read fail")
                    Console.SetIn throwingReader

                    try
                        let effect = Console.readLine<string> (fun ex -> ex.Message)
                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Failed msg -> Expect.equal msg "read fail" "Should map exception on read path"
                        | other -> failtest $"Expected Failed but got: {other}"
                    finally
                        Console.SetIn originalIn
                        throwingReader.Dispose())

                testCapturedOut "write - writes text to stdout" (fun cap runtime ->
                    runtime.Run(Console.write "hello" id).UnsafeSuccess()

                    Expect.equal cap.Output "hello" "Should write text to stdout")

                testCapturedOut "writeLine - writes text with newline to stdout" (fun cap runtime ->
                    runtime.Run(Console.writeLine "world" id).UnsafeSuccess()

                    Expect.stringContains cap.Output "world" "Should contain text"
                    Expect.stringContains cap.Output Environment.NewLine "Should write trailing newline")

                testAllRuntimes "write - maps exception with custom error handler" (fun runtime ->
                    let originalOut = Console.Out
                    let throwingWriter = new ThrowingWriter("boom")
                    Console.SetOut throwingWriter

                    try
                        let effect = Console.write "x" (fun ex -> $"mapped: {ex.Message}")
                        let result = runtime.Run(effect).UnsafeResult()

                        match result with
                        | Failed msg -> Expect.equal msg "mapped: boom" "Should map exception"
                        | other -> failtest $"Expected Failed but got: {other}"
                    finally
                        Console.SetOut originalOut
                        throwingWriter.Dispose())

                testAllRuntimes "clear - effect either succeeds or maps to a typed error" (fun runtime ->
                    // Console.Clear may throw IOException when stdout is redirected (typical in test hosts).
                    // Verify the effect machinery handles both outcomes without leaking an unmapped exception.
                    let effect = Console.clear (fun ex -> ex.Message)
                    let result = runtime.Run(effect).UnsafeResult()

                    match result with
                    | Succeeded () -> ()
                    | Failed msg -> Expect.isNotEmpty msg "Mapped error message should be non-empty"
                    | other -> failtest $"Expected Succeeded or Failed but got: {other}")
            ]
    )
