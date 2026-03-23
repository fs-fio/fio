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

let private runtimes () = [
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
        ConsoleBackend.reset()

let private testAllRuntimes name (f: MockConsoleBackend -> FIORuntime -> unit) =
    testList name [
        for rt in runtimes () ->
            testCase (rt.GetType().Name) (fun () -> withMockBackend f rt)
    ]

// All console tests must run sequentially because ConsoleBackend has process-global state
[<Tests>]
let consoleTests =
    testSequenced (testList "Console" [

        testAllRuntimes "stdout output functions write to stdout" (fun mock runtime ->
            runtime.Run(Console.writeExn "hello").UnsafeSuccess()
            Expect.equal mock.StdOut "hello" "write should capture text"

            runtime.Run(Console.writeLineExn "world").UnsafeSuccess()
            Expect.stringContains mock.StdOut "world" "writeLine should capture text"

            runtime.Run(Console.printExn "fmt").UnsafeSuccess()
            Expect.stringContains mock.StdOut "fmt" "print should capture text"

            runtime.Run(Console.printLineExn "line").UnsafeSuccess()
            Expect.stringContains mock.StdOut "line" "printLine should capture text"

            runtime.Run(Console.newLineExn).UnsafeSuccess()
            Expect.isTrue (mock.StdOut.Contains "\n" || mock.StdOut.Contains(Environment.NewLine)) "newLine should write newline")

        testAllRuntimes "stderr output functions write to stderr" (fun mock runtime ->
            runtime.Run(Console.writeErrorExn "err1").UnsafeSuccess()
            Expect.equal mock.StdErr "err1" "writeError should capture text"

            runtime.Run(Console.writeErrorLineExn "err2").UnsafeSuccess()
            Expect.stringContains mock.StdErr "err2" "writeErrorLine should capture text"

            runtime.Run(Console.printErrorExn "err3").UnsafeSuccess()
            Expect.stringContains mock.StdErr "err3" "printError should capture text"

            runtime.Run(Console.printErrorLineExn "err4").UnsafeSuccess()
            Expect.stringContains mock.StdErr "err4" "printErrorLine should capture text")

        testAllRuntimes "printLines writes multiple lines" (fun mock runtime ->
            runtime.Run(Console.printLinesExn ["a"; "b"; "c"]).UnsafeSuccess()
            Expect.stringContains mock.StdOut "a" "Should contain first line"
            Expect.stringContains mock.StdOut "b" "Should contain second line"
            Expect.stringContains mock.StdOut "c" "Should contain third line")

        testAllRuntimes "error mapper transforms exception" (fun mock runtime ->
            mock.ShouldThrow <- Some (InvalidOperationException "boom" :> exn)
            let eff = Console.write<string>("x", fun ex -> $"mapped: {ex.Message}")
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Failed msg -> Expect.equal msg "mapped: boom" "onError mapper should transform exception"
            | _ -> failtest $"Expected Failed, got {result}")

        testAllRuntimes "Exn variant surfaces original exception" (fun mock runtime ->
            mock.ShouldThrow <- Some (InvalidOperationException "crash" :> exn)
            let eff = Console.writeExn "x"
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Failed ex -> Expect.equal ex.Message "crash" "Exn variant should preserve exception"
            | _ -> failtest $"Expected Failed, got {result}")

        testAllRuntimes "error on read path" (fun mock runtime ->
            mock.ShouldThrow <- Some (InvalidOperationException "read fail" :> exn)
            let eff = Console.readLine<string>(fun ex -> ex.Message)
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Failed msg -> Expect.equal msg "read fail" "Error should be mapped on read path"
            | _ -> failtest $"Expected Failed, got {result}")

        testAllRuntimes "readLine reads queued input" (fun mock runtime ->
            mock.QueueInputLine "hello world"
            let result = runtime.Run(Console.readLineExn).UnsafeSuccess()
            Expect.equal result "hello world" "Should read queued input")

        testAllRuntimes "readKey reads queued key" (fun mock runtime ->
            mock.QueueCharKey 'Z'
            let result = runtime.Run(Console.readKeyExn false).UnsafeSuccess()
            Expect.equal result.KeyChar 'Z' "Should read queued key")

        testAllRuntimes "read reads first character as int" (fun mock runtime ->
            mock.QueueInputLine "ABC"
            let result = runtime.Run(Console.readExn).UnsafeSuccess()
            Expect.equal result (int 'A') "Should read first char as int")

        testAllRuntimes "keyAvailable reflects queue state" (fun mock runtime ->
            let empty = runtime.Run(Console.keyAvailableExn).UnsafeSuccess()
            Expect.isFalse empty "Should be false when no key queued"
            mock.QueueCharKey 'A'
            let full = runtime.Run(Console.keyAvailableExn).UnsafeSuccess()
            Expect.isTrue full "Should be true when key is queued")

        testAllRuntimes "readPassword reads masked input" (fun mock runtime ->
            mock.QueueString "secret"
            let result = runtime.Run(Console.readPasswordExn).UnsafeSuccess()
            Expect.equal result "secret" "Should read password"
            Expect.stringContains mock.StdOut "*" "Should mask with asterisks")

        testAllRuntimes "readPassword handles backspace" (fun mock runtime ->
            mock.QueueCharKey 'a'
            mock.QueueCharKey 'b'
            mock.QueueSpecialKey ConsoleKey.Backspace
            mock.QueueCharKey 'c'
            mock.QueueSpecialKey ConsoleKey.Enter
            let result = runtime.Run(Console.readPasswordExn).UnsafeSuccess()
            Expect.equal result "ac" "Backspace should remove previous character")

        testAllRuntimes "readPassword empty input returns empty string" (fun mock runtime ->
            mock.QueueSpecialKey ConsoleKey.Enter
            let result = runtime.Run(Console.readPasswordExn).UnsafeSuccess()
            Expect.equal result "" "Just Enter should yield empty password")

        testAllRuntimes "readPassword backspace on empty buffer does not underflow" (fun mock runtime ->
            mock.QueueSpecialKey ConsoleKey.Backspace
            mock.QueueCharKey 'x'
            mock.QueueSpecialKey ConsoleKey.Enter
            let result = runtime.Run(Console.readPasswordExn).UnsafeSuccess()
            Expect.equal result "x" "Backspace on empty should be ignored")

        testAllRuntimes "cursor position get/set" (fun _mock runtime ->
            runtime.Run(Console.setCursorLeftExn 10).UnsafeSuccess()
            runtime.Run(Console.setCursorTopExn 20).UnsafeSuccess()
            let left = runtime.Run(Console.getCursorLeftExn).UnsafeSuccess()
            let top = runtime.Run(Console.getCursorTopExn).UnsafeSuccess()
            Expect.equal (left, top) (10, 20) "Should round-trip cursor position"

            runtime.Run(Console.setCursorPositionExn(30, 40)).UnsafeSuccess()
            let pos = runtime.Run(Console.getCursorPositionExn).UnsafeSuccess()
            Expect.equal pos (30, 40) "setCursorPosition should set both")

        testAllRuntimes "cursor visibility get/set" (fun _mock runtime ->
            runtime.Run(Console.setCursorVisibleExn false).UnsafeSuccess()
            let v = runtime.Run(Console.getCursorVisibleExn).UnsafeSuccess()
            Expect.isFalse v "Should be hidden")

        testAllRuntimes "window and buffer dimensions" (fun mock runtime ->
            mock.SetWindowSize(200, 50)
            mock.SetBufferSize(300, 1000)
            let ww = runtime.Run(Console.getWindowWidthExn).UnsafeSuccess()
            let wh = runtime.Run(Console.getWindowHeightExn).UnsafeSuccess()
            let bw = runtime.Run(Console.getBufferWidthExn).UnsafeSuccess()
            let bh = runtime.Run(Console.getBufferHeightExn).UnsafeSuccess()
            Expect.equal (ww, wh) (200, 50) "Window dimensions"
            Expect.equal (bw, bh) (300, 1000) "Buffer dimensions")

        testAllRuntimes "title get/set" (fun _mock runtime ->
            runtime.Run(Console.setTitleExn "FIO App").UnsafeSuccess()
            let t = runtime.Run(Console.getTitleExn).UnsafeSuccess()
            Expect.equal t "FIO App" "Should round-trip title")

        testAllRuntimes "foreground/background color get/set and resetColor" (fun _mock runtime ->
            runtime.Run(Console.setForegroundColorExn ConsoleColor.Red).UnsafeSuccess()
            runtime.Run(Console.setBackgroundColorExn ConsoleColor.Blue).UnsafeSuccess()
            let fg = runtime.Run(Console.getForegroundColorExn).UnsafeSuccess()
            let bg = runtime.Run(Console.getBackgroundColorExn).UnsafeSuccess()
            Expect.equal fg ConsoleColor.Red "Foreground should be red"
            Expect.equal bg ConsoleColor.Blue "Background should be blue"

            runtime.Run(Console.resetColorExn).UnsafeSuccess()
            let fg2 = runtime.Run(Console.getForegroundColorExn).UnsafeSuccess()
            let bg2 = runtime.Run(Console.getBackgroundColorExn).UnsafeSuccess()
            Expect.equal fg2 ConsoleColor.Gray "Should reset foreground"
            Expect.equal bg2 ConsoleColor.Black "Should reset background")

        testAllRuntimes "withForegroundColor restores color on success" (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
            let eff = Console.withForegroundColorExn(ConsoleColor.Red, Console.writeExn "red")
            runtime.Run(eff).UnsafeSuccess()
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.White "Should restore color")

        testAllRuntimes "withForegroundColor restores color on failure" (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
            let eff = Console.withForegroundColorExn(ConsoleColor.Red, FIO.fail (exn "boom"))
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Failed _ ->
                Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.White "Should restore color on failure"
            | _ -> failtest $"Expected Failed, got {result}")

        testAllRuntimes "withColors restores both colors on failure" (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Cyan
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.DarkMagenta
            let eff = Console.withColorsExn(ConsoleColor.Red, ConsoleColor.Blue, FIO.fail (exn "boom"))
            let result = runtime.Run(eff).UnsafeResult()
            match result with
            | Failed _ ->
                Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.Cyan "Should restore foreground"
                Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.DarkMagenta "Should restore background"
            | _ -> failtest $"Expected Failed, got {result}")

        testAllRuntimes "withColors restores both colors on success" (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.Black
            let eff = Console.withColorsExn(ConsoleColor.Red, ConsoleColor.Blue, Console.writeExn "colored")
            runtime.Run(eff).UnsafeSuccess()
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.White "Should restore foreground"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.Black "Should restore background")

        testAllRuntimes "clear and beep" (fun mock runtime ->
            runtime.Run(Console.clearExn).UnsafeSuccess()
            Expect.equal mock.ClearCount 1 "Should track clear"
            runtime.Run(Console.beepExn).UnsafeSuccess()
            Expect.equal mock.BeepCount 1 "Should track beep")

        testAllRuntimes "isInputRedirected / isOutputRedirected / isErrorRedirected" (fun mock runtime ->
            let ir = runtime.Run(Console.isInputRedirectedExn).UnsafeSuccess()
            let or' = runtime.Run(Console.isOutputRedirectedExn).UnsafeSuccess()
            let er = runtime.Run(Console.isErrorRedirectedExn).UnsafeSuccess()
            Expect.isFalse ir "Mock input not redirected"
            Expect.isFalse or' "Mock output not redirected"
            Expect.isFalse er "Mock error not redirected"

            mock.IsInputRedirected <- true
            mock.IsOutputRedirected <- true
            mock.IsErrorRedirected <- true
            let ir2 = runtime.Run(Console.isInputRedirectedExn).UnsafeSuccess()
            let or2 = runtime.Run(Console.isOutputRedirectedExn).UnsafeSuccess()
            let er2 = runtime.Run(Console.isErrorRedirectedExn).UnsafeSuccess()
            Expect.isTrue ir2 "Should reflect redirected state"
            Expect.isTrue or2 "Should reflect redirected state"
            Expect.isTrue er2 "Should reflect redirected state")
    ])
