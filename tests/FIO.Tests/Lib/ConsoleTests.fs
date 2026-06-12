/// <summary>Provides tests for console I/O effects using <c>Console.SetOut</c>/<c>SetIn</c> for deterministic capture.</summary>
module FIO.Tests.ConsoleTests

open FIO.DSL
open FIO.Console
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System
open System.IO

type private ThrowingWriter(message: string) =
    inherit StringWriter()
    override _.Write(_: char) : unit = raise (InvalidOperationException message)
    override _.Write(value: string) : unit =
        if isNull value then () else raise (InvalidOperationException message)

type private ThrowingReader(message: string) =
    inherit StringReader("")
    override _.Read() : int = raise (InvalidOperationException message)
    override _.ReadLine() : string = raise (InvalidOperationException message)

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

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
            for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> withCapturedOut f rt)
        ]

let private testCapturedIn name (input: string) (f: FIORuntime -> unit) =
    testList
        name
        [
            for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> withStdIn input f rt)
        ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList
        name
        [
            for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt)
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
