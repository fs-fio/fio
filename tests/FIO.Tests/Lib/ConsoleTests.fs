/// <summary>Provides tests for console I/O effects using a mock backend for deterministic verification.</summary>
module FIO.Tests.ConsoleTests

open FIO.Tests.MockConsoleBackend

open FIO.DSL
open FIO.Console

open Expecto

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open System

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private withMockBackend (test: MockConsoleBackend -> FIORuntime -> unit) (runtime: FIORuntime) =
    let mock = MockConsoleBackend()
    ConsoleBackend.set mock

    try
        test mock runtime
    finally
        ConsoleBackend.reset ()

let private testAllRuntimes name (f: MockConsoleBackend -> FIORuntime -> unit) =
    testList
        name
        [
            for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> withMockBackend f rt)
        ]

// All console tests must run sequentially because ConsoleBackend has process-global state
[<Tests>]
let consoleTests =
    testSequenced (
        testList
            "Console"
            [

                testAllRuntimes "printExn - writes formatted text to stdout" (fun mock runtime ->
                    runtime.Run(Console.print ("hello", id)).UnsafeSuccess()

                    Expect.stringContains mock.StdOut "hello" "Should write to stdout")

                testAllRuntimes "printErrorExn - writes formatted text to stderr" (fun mock runtime ->
                    runtime.Run(Console.printError ("err", id)).UnsafeSuccess()

                    Expect.stringContains mock.StdErr "err" "Should write to stderr")

                testAllRuntimes "printLineExn - writes formatted text with newline to stdout" (fun mock runtime ->
                    runtime.Run(Console.printLine ("line", id)).UnsafeSuccess()

                    Expect.stringContains mock.StdOut "line" "Should write line to stdout")

                testAllRuntimes "printErrorLineExn - writes formatted text with newline to stderr" (fun mock runtime ->
                    runtime.Run(Console.printErrorLine ("errline", id)).UnsafeSuccess()

                    Expect.stringContains mock.StdErr "errline" "Should write line to stderr")

                testAllRuntimes "readLineExn - reads queued input line" (fun mock runtime ->
                    mock.QueueInputLine "hello world"

                    let result = runtime.Run(Console.readLine id).UnsafeSuccess()

                    Expect.equal result "hello world" "Should read queued input")

                testAllRuntimes "readLine - maps exception with custom error handler" (fun mock runtime ->
                    mock.ShouldThrow <- Some(InvalidOperationException "read fail" :> exn)
                    let eff = Console.readLine<string> (fun ex -> ex.Message)

                    let result = runtime.Run(eff).UnsafeResult()

                    match result with
                    | Failed msg -> Expect.equal msg "read fail" "Should map exception on read path"
                    | other -> failtest $"Expected Failed but got: {other}")

                testAllRuntimes "readKeyExn - reads queued key" (fun mock runtime ->
                    mock.QueueCharKey 'Z'

                    let result = runtime.Run(Console.readKey (false, id)).UnsafeSuccess()

                    Expect.equal result.KeyChar 'Z' "Should read queued key")

                testAllRuntimes "keyAvailableExn - reflects key queue state" (fun mock runtime ->
                    let empty = runtime.Run(Console.keyAvailable id).UnsafeSuccess()

                    Expect.isFalse empty "Should be false when no key queued"

                    mock.QueueCharKey 'A'
                    let full = runtime.Run(Console.keyAvailable id).UnsafeSuccess()

                    Expect.isTrue full "Should be true when key is queued")

                testAllRuntimes "readExn - reads first character as int" (fun mock runtime ->
                    mock.QueueInputLine "ABC"

                    let result = runtime.Run(Console.read id).UnsafeSuccess()

                    Expect.equal result (int 'A') "Should read first char as int")

                testAllRuntimes "writeExn - writes text to stdout" (fun mock runtime ->
                    runtime.Run(Console.write ("hello", id)).UnsafeSuccess()

                    Expect.equal mock.StdOut "hello" "Should write text to stdout")

                testAllRuntimes "write - maps exception with custom error handler" (fun mock runtime ->
                    mock.ShouldThrow <- Some(InvalidOperationException "boom" :> exn)
                    let eff = Console.write ("x", fun ex -> $"mapped: {ex.Message}")

                    let result = runtime.Run(eff).UnsafeResult()

                    match result with
                    | Failed msg -> Expect.equal msg "mapped: boom" "Should map exception"
                    | other -> failtest $"Expected Failed but got: {other}")

                testAllRuntimes "writeExn - surfaces original exception on error" (fun mock runtime ->
                    mock.ShouldThrow <- Some(InvalidOperationException "crash" :> exn)
                    let eff = Console.write ("x", id)

                    let result = runtime.Run(eff).UnsafeResult()

                    match result with
                    | Failed ex -> Expect.equal ex.Message "crash" "Should preserve original exception"
                    | other -> failtest $"Expected Failed but got: {other}")

                testAllRuntimes "writeLineExn - writes text with newline to stdout" (fun mock runtime ->
                    runtime.Run(Console.writeLine ("world", id)).UnsafeSuccess()

                    Expect.stringContains mock.StdOut "world" "Should write text with newline to stdout")

                testAllRuntimes "newLineExn - writes blank line to stdout" (fun mock runtime ->
                    runtime.Run(Console.newLine id).UnsafeSuccess()

                    Expect.isTrue
                        (mock.StdOut.Contains "\n" || mock.StdOut.Contains System.Environment.NewLine)
                        "Should write newline")

                testAllRuntimes "writeErrorExn - writes text to stderr" (fun mock runtime ->
                    runtime.Run(Console.writeError ("err1", id)).UnsafeSuccess()

                    Expect.equal mock.StdErr "err1" "Should write text to stderr")

                testAllRuntimes "writeErrorLineExn - writes text with newline to stderr" (fun mock runtime ->
                    runtime.Run(Console.writeErrorLine ("err2", id)).UnsafeSuccess()

                    Expect.stringContains mock.StdErr "err2" "Should write text with newline to stderr")

                testAllRuntimes "clearExn - clears the console" (fun mock runtime ->
                    runtime.Run(Console.clear id).UnsafeSuccess()

                    Expect.equal mock.ClearCount 1 "Should track clear call")

                testAllRuntimes "beepExn - triggers system bell" (fun mock runtime ->
                    runtime.Run(Console.beep id).UnsafeSuccess()

                    Expect.equal mock.BeepCount 1 "Should track beep call")

                testAllRuntimes "setCursorLeftExn - sets cursor column position" (fun _mock runtime ->
                    runtime.Run(Console.setCursorLeft (10, id)).UnsafeSuccess()

                    let left = runtime.Run(Console.getCursorLeft id).UnsafeSuccess()

                    Expect.equal left 10 "Should set cursor column")

                testAllRuntimes "setCursorTopExn - sets cursor row position" (fun _mock runtime ->
                    runtime.Run(Console.setCursorTop (20, id)).UnsafeSuccess()

                    let top = runtime.Run(Console.getCursorTop id).UnsafeSuccess()

                    Expect.equal top 20 "Should set cursor row")

                testAllRuntimes "setCursorPositionExn - sets both cursor coordinates" (fun _mock runtime ->
                    runtime.Run(Console.setCursorPosition (30, 40, id)).UnsafeSuccess()

                    let pos = runtime.Run(Console.getCursorPosition id).UnsafeSuccess()

                    Expect.equal pos (30, 40) "Should set both coordinates")

                testAllRuntimes "getCursorPositionExn - returns cursor position tuple" (fun _mock runtime ->
                    runtime.Run(Console.setCursorLeft (5, id)).UnsafeSuccess()
                    runtime.Run(Console.setCursorTop (15, id)).UnsafeSuccess()

                    let pos = runtime.Run(Console.getCursorPosition id).UnsafeSuccess()

                    Expect.equal pos (5, 15) "Should return position as tuple")

                testAllRuntimes "setCursorVisibleExn - sets cursor visibility" (fun _mock runtime ->
                    runtime.Run(Console.setCursorVisible (false, id)).UnsafeSuccess()

                    let v = runtime.Run(Console.getCursorVisible id).UnsafeSuccess()

                    Expect.isFalse v "Should hide cursor")

                testAllRuntimes "setForegroundColorExn - sets foreground color" (fun _mock runtime ->
                    runtime.Run(Console.setForegroundColor (ConsoleColor.Red, id)).UnsafeSuccess()

                    let fg = runtime.Run(Console.getForegroundColor id).UnsafeSuccess()

                    Expect.equal fg ConsoleColor.Red "Should set foreground color")

                testAllRuntimes "setBackgroundColorExn - sets background color" (fun _mock runtime ->
                    runtime.Run(Console.setBackgroundColor (ConsoleColor.Blue, id)).UnsafeSuccess()

                    let bg = runtime.Run(Console.getBackgroundColor id).UnsafeSuccess()

                    Expect.equal bg ConsoleColor.Blue "Should set background color")

                testAllRuntimes "resetColorExn - resets colors to defaults" (fun _mock runtime ->
                    runtime.Run(Console.setForegroundColor (ConsoleColor.Red, id)).UnsafeSuccess()
                    runtime.Run(Console.setBackgroundColor (ConsoleColor.Blue, id)).UnsafeSuccess()

                    runtime.Run(Console.resetColor id).UnsafeSuccess()
                    let fg = runtime.Run(Console.getForegroundColor id).UnsafeSuccess()
                    let bg = runtime.Run(Console.getBackgroundColor id).UnsafeSuccess()

                    Expect.equal fg ConsoleColor.Gray "Should reset foreground"
                    Expect.equal bg ConsoleColor.Black "Should reset background")

                testAllRuntimes "setTitleExn - sets console title" (fun _mock runtime ->
                    runtime.Run(Console.setTitle ("FIO App", id)).UnsafeSuccess()

                    let t = runtime.Run(Console.getTitle id).UnsafeSuccess()

                    Expect.equal t "FIO App" "Should set title")

                testAllRuntimes "isInputRedirectedExn - reflects input redirection state" (fun mock runtime ->
                    let before = runtime.Run(Console.isInputRedirected id).UnsafeSuccess()

                    Expect.isFalse before "Should be false by default"

                    mock.IsInputRedirected <- true
                    let after = runtime.Run(Console.isInputRedirected id).UnsafeSuccess()

                    Expect.isTrue after "Should reflect redirected state")

                testAllRuntimes "isOutputRedirectedExn - reflects output redirection state" (fun mock runtime ->
                    let before = runtime.Run(Console.isOutputRedirected id).UnsafeSuccess()

                    Expect.isFalse before "Should be false by default"

                    mock.IsOutputRedirected <- true
                    let after = runtime.Run(Console.isOutputRedirected id).UnsafeSuccess()

                    Expect.isTrue after "Should reflect redirected state")

                testAllRuntimes "isErrorRedirectedExn - reflects error redirection state" (fun mock runtime ->
                    let before = runtime.Run(Console.isErrorRedirected id).UnsafeSuccess()

                    Expect.isFalse before "Should be false by default"

                    mock.IsErrorRedirected <- true
                    let after = runtime.Run(Console.isErrorRedirected id).UnsafeSuccess()

                    Expect.isTrue after "Should reflect redirected state")

                testAllRuntimes "getWindowWidthExn - returns window width" (fun mock runtime ->
                    mock.SetWindowSize(200, 50)

                    let w = runtime.Run(Console.getWindowWidth id).UnsafeSuccess()

                    Expect.equal w 200 "Should return window width")

                testAllRuntimes "getWindowHeightExn - returns window height" (fun mock runtime ->
                    mock.SetWindowSize(200, 50)

                    let h = runtime.Run(Console.getWindowHeight id).UnsafeSuccess()

                    Expect.equal h 50 "Should return window height")

                testAllRuntimes "getBufferWidthExn - returns buffer width" (fun mock runtime ->
                    mock.SetBufferSize(300, 1000)

                    let w = runtime.Run(Console.getBufferWidth id).UnsafeSuccess()

                    Expect.equal w 300 "Should return buffer width")

                testAllRuntimes "getBufferHeightExn - returns buffer height" (fun mock runtime ->
                    mock.SetBufferSize(300, 1000)

                    let h = runtime.Run(Console.getBufferHeight id).UnsafeSuccess()

                    Expect.equal h 1000 "Should return buffer height")

                testAllRuntimes "printLinesExn - writes multiple lines to stdout" (fun mock runtime ->
                    runtime.Run(Console.printLines ([ "a"; "b"; "c" ], id)).UnsafeSuccess()

                    Expect.stringContains mock.StdOut "a" "Should contain first line"
                    Expect.stringContains mock.StdOut "b" "Should contain second line"
                    Expect.stringContains mock.StdOut "c" "Should contain third line")

                testAllRuntimes "readPasswordExn - reads masked input" (fun mock runtime ->
                    mock.QueueString "secret"

                    let result = runtime.Run(Console.readPassword id).UnsafeSuccess()

                    Expect.equal result "secret" "Should read password"
                    Expect.stringContains mock.StdOut "*" "Should mask with asterisks")

                testAllRuntimes "readPasswordExn - handles backspace" (fun mock runtime ->
                    mock.QueueCharKey 'a'
                    mock.QueueCharKey 'b'
                    mock.QueueSpecialKey ConsoleKey.Backspace
                    mock.QueueCharKey 'c'
                    mock.QueueSpecialKey ConsoleKey.Enter

                    let result = runtime.Run(Console.readPassword id).UnsafeSuccess()

                    Expect.equal result "ac" "Backspace should remove previous character")

                testAllRuntimes "readPasswordExn - returns empty string for immediate Enter" (fun mock runtime ->
                    mock.QueueSpecialKey ConsoleKey.Enter

                    let result = runtime.Run(Console.readPassword id).UnsafeSuccess()

                    Expect.equal result "" "Just Enter should yield empty password")

                testAllRuntimes "readPasswordExn - ignores backspace on empty buffer" (fun mock runtime ->
                    mock.QueueSpecialKey ConsoleKey.Backspace
                    mock.QueueCharKey 'x'
                    mock.QueueSpecialKey ConsoleKey.Enter

                    let result = runtime.Run(Console.readPassword id).UnsafeSuccess()

                    Expect.equal result "x" "Backspace on empty should be ignored")

                testAllRuntimes "withForegroundColorExn - restores color on success" (fun mock runtime ->
                    (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White

                    let eff =
                        Console.withForegroundColor (ConsoleColor.Red, Console.write ("red", id), id)

                    runtime.Run(eff).UnsafeSuccess()

                    Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.White "Should restore color")

                testAllRuntimes "withForegroundColorExn - restores color on failure" (fun mock runtime ->
                    (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
                    let eff = Console.withForegroundColor (ConsoleColor.Red, FIO.fail (exn "boom"), id)

                    let result = runtime.Run(eff).UnsafeResult()

                    match result with
                    | Failed _ ->
                        Expect.equal
                            (mock :> IConsoleBackend).ForegroundColor
                            ConsoleColor.White
                            "Should restore color on failure"
                    | other -> failtest $"Expected Failed but got: {other}")

                testAllRuntimes "withColorsExn - restores both colors on success" (fun mock runtime ->
                    (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
                    (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.Black

                    let eff =
                        Console.withColors (ConsoleColor.Red, ConsoleColor.Blue, Console.write ("colored", id), id)

                    runtime.Run(eff).UnsafeSuccess()

                    Expect.equal
                        (mock :> IConsoleBackend).ForegroundColor
                        ConsoleColor.White
                        "Should restore foreground"

                    Expect.equal
                        (mock :> IConsoleBackend).BackgroundColor
                        ConsoleColor.Black
                        "Should restore background")

                testAllRuntimes "withColorsExn - restores both colors on failure" (fun mock runtime ->
                    (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Cyan
                    (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.DarkMagenta

                    let eff =
                        Console.withColors (ConsoleColor.Red, ConsoleColor.Blue, FIO.fail (exn "boom"), id)

                    let result = runtime.Run(eff).UnsafeResult()

                    match result with
                    | Failed _ ->
                        Expect.equal
                            (mock :> IConsoleBackend).ForegroundColor
                            ConsoleColor.Cyan
                            "Should restore foreground"

                        Expect.equal
                            (mock :> IConsoleBackend).BackgroundColor
                            ConsoleColor.DarkMagenta
                            "Should restore background"
                    | other -> failtest $"Expected Failed but got: {other}")
            ]
    )
