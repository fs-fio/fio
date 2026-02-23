module FIO.Tests.ConsoleTests

open FIO.Tests.MockConsoleBackend
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Console

open Expecto

open FIO.Runtime

open System

// Setup and teardown helpers for mock backend
let private withMockBackend (test: MockConsoleBackend -> FIORuntime -> unit) (runtime: FIORuntime) =
    let mock = MockConsoleBackend()
    ConsoleBackend.set mock
    try
        test mock runtime
    finally
        ConsoleBackend.reset()

// All console tests must run sequentially because ConsoleBackend has process-global state
// Tests are ordered to match the function order in Console.fs (non-Exn first, then Exn)
[<Tests>]
let consoleTests =
    testSequenced (testList "Console" [

        // print / printExn
        testPropertyWithConfig fsCheckConfig "Console.print with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.print("test print", fun ex -> ex.Message)
            
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "print with error mapper should complete"
            Expect.stringContains mock.StdOut "test print" "Should write to stdout")

        testPropertyWithConfig fsCheckConfig "Console.printExn prints formatted"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printExn "test print"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printExn should complete without throwing"
            Expect.stringContains mock.StdOut "test print" "Should write to stdout")

        // printError / printErrorExn
        testPropertyWithConfig fsCheckConfig "Console.printError with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printError("error print", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printError with error mapper should complete"
            Expect.stringContains mock.StdErr "error print" "Should write to stderr")

        testPropertyWithConfig fsCheckConfig "Console.printErrorExn prints to stderr"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printErrorExn "error print"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printErrorExn should complete without throwing"
            Expect.stringContains mock.StdErr "error print" "Should write to stderr")

        // printLine / printLineExn
        testPropertyWithConfig fsCheckConfig "Console.printLine with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printLine("test print line", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printLine with error mapper should complete"
            Expect.stringContains mock.StdOut "test print line" "Should write to stdout")

        testPropertyWithConfig fsCheckConfig "Console.printLineExn prints with newline"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printLineExn "test print line"
            
            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printLineExn should complete without throwing"
            Expect.stringContains mock.StdOut "test print line" "Should write to stdout")

        // printErrorLine / printErrorLineExn
        testPropertyWithConfig fsCheckConfig "Console.printErrorLine with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printErrorLine("error line", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printErrorLine with error mapper should complete"
            Expect.stringContains mock.StdErr "error line" "Should write to stderr")

        testPropertyWithConfig fsCheckConfig "Console.printErrorLineExn prints error with newline"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.printErrorLineExn "error line"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printErrorLineExn should complete without throwing"
            Expect.stringContains mock.StdErr "error line" "Should write to stderr")

        // readLine / readLineExn
        testPropertyWithConfig fsCheckConfig "Console.readLine with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.QueueInputLine("test input")
            let eff = Console.readLine<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "test input" "Should read queued input")

        testPropertyWithConfig fsCheckConfig "Console.readLineExn reads input"
        <| withMockBackend (fun mock runtime ->
            mock.QueueInputLine("hello world")
            let eff = Console.readLineExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "hello world" "Should read queued input")

        // readKey / readKeyExn
        testPropertyWithConfig fsCheckConfig "Console.readKey with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.QueueCharKey('A')
            let eff = Console.readKey<string>(false, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result.KeyChar 'A' "Should read queued key")

        testPropertyWithConfig fsCheckConfig "Console.readKeyExn reads key"
        <| withMockBackend (fun mock runtime ->
            mock.QueueCharKey('X')
            let eff = Console.readKeyExn false

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result.KeyChar 'X' "Should read queued key")

        // keyAvailable / keyAvailableExn
        testPropertyWithConfig fsCheckConfig "Console.keyAvailable with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.keyAvailable<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Should return false when no key queued")

        testPropertyWithConfig fsCheckConfig "Console.keyAvailableExn checks key availability"
        <| withMockBackend (fun mock runtime ->
            mock.QueueCharKey('Z')
            let eff = Console.keyAvailableExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Should return true when key is queued")

        // read / readExn
        testPropertyWithConfig fsCheckConfig "Console.read with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.QueueInputLine "ABC"
            let eff = Console.read<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (int 'A') "Should read first character as int")

        testPropertyWithConfig fsCheckConfig "Console.readExn reads single character"
        <| withMockBackend (fun mock runtime ->
            mock.QueueInputLine "XYZ"
            let eff = Console.readExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (int 'X') "Should read first character as int")

        // write / writeExn
        testPropertyWithConfig fsCheckConfig "Console.write with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.write("test", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "write with error mapper should complete"
            Expect.equal mock.StdOut "test" "Should write exact text")

        testPropertyWithConfig fsCheckConfig "Console.writeExn writes without throwing"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeExn "test"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeExn should complete without throwing"
            Expect.equal mock.StdOut "test" "Should write exact text")

        // writeLine / writeLineExn
        testPropertyWithConfig fsCheckConfig "Console.writeLine with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeLine("test line", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeLine with error mapper should complete"
            Expect.stringContains mock.StdOut "test line" "Should write text")

        testPropertyWithConfig fsCheckConfig "Console.writeLineExn writes with newline"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeLineExn "test line"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeLineExn should complete without throwing"
            Expect.stringContains mock.StdOut "test line" "Should write text")

        // newLine / newLineExn
        testPropertyWithConfig fsCheckConfig "Console.newLine with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.newLine<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "newLine with error mapper should complete"
            Expect.isTrue (mock.StdOut.Contains("\n") || mock.StdOut.Contains(Environment.NewLine)) "Should write newline")

        testPropertyWithConfig fsCheckConfig "Console.newLineExn writes blank line"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.newLineExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "newLineExn should complete without throwing")

        // writeError / writeErrorExn
        testPropertyWithConfig fsCheckConfig "Console.writeError with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeError("error message", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeError with error mapper should complete"
            Expect.equal mock.StdErr "error message" "Should write to stderr")

        testPropertyWithConfig fsCheckConfig "Console.writeErrorExn writes to stderr"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeErrorExn "error message"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeErrorExn should complete without throwing"
            Expect.equal mock.StdErr "error message" "Should write to stderr")

        // writeErrorLine / writeErrorLineExn
        testPropertyWithConfig fsCheckConfig "Console.writeErrorLine with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeErrorLine("error line", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeErrorLine with error mapper should complete"
            Expect.stringContains mock.StdErr "error line" "Should write to stderr")

        testPropertyWithConfig fsCheckConfig "Console.writeErrorLineExn writes to stderr with newline"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.writeErrorLineExn "error line"

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "writeErrorLineExn should complete without throwing"
            Expect.stringContains mock.StdErr "error line" "Should write to stderr")

        // clear / clearExn
        testPropertyWithConfig fsCheckConfig "Console.clear with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.clear<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "clear with error mapper should complete"
            Expect.equal mock.ClearCount 1 "Should call Clear once")

        testPropertyWithConfig fsCheckConfig "Console.clearExn completes"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.clearExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "clearExn should complete"
            Expect.equal mock.ClearCount 1 "Should call Clear once")

        // setCursorLeft / setCursorLeftExn
        testPropertyWithConfig fsCheckConfig "Console.setCursorLeft with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setCursorLeft<string>(10, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setCursorLeft should complete"
            Expect.equal (mock :> IConsoleBackend).CursorLeft 10 "Cursor left should be set")

        testPropertyWithConfig fsCheckConfig "Console.setCursorLeftExn sets cursor"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setCursorLeftExn 15
                return! Console.getCursorLeftExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 15 "Cursor left should be 15")

        // setCursorTop / setCursorTopExn
        testPropertyWithConfig fsCheckConfig "Console.setCursorTop with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setCursorTop<string>(5, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setCursorTop should complete"
            Expect.equal (mock :> IConsoleBackend).CursorTop 5 "Cursor top should be set")

        testPropertyWithConfig fsCheckConfig "Console.setCursorTopExn sets cursor"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setCursorTopExn 8
                return! Console.getCursorTopExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 8 "Cursor top should be 8")

        // getCursorLeft / getCursorLeftExn
        testPropertyWithConfig fsCheckConfig "Console.getCursorLeft with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorLeft <- 20
            let eff = Console.getCursorLeft<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 20 "Should get cursor left")

        testPropertyWithConfig fsCheckConfig "Console.getCursorLeftExn returns position"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorLeft <- 25
            let eff = Console.getCursorLeftExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 25 "Should get cursor left")

        // getCursorTop / getCursorTopExn
        testPropertyWithConfig fsCheckConfig "Console.getCursorTop with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorTop <- 12
            let eff = Console.getCursorTop<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 12 "Should get cursor top")

        testPropertyWithConfig fsCheckConfig "Console.getCursorTopExn returns position"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorTop <- 18
            let eff = Console.getCursorTopExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 18 "Should get cursor top")

        // beep / beepExn
        testPropertyWithConfig fsCheckConfig "Console.beep with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.beep<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "beep should complete"
            Expect.equal mock.BeepCount 1 "Should track beep call")

        testPropertyWithConfig fsCheckConfig "Console.beepExn completes"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.beepExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "beepExn should complete"
            Expect.equal mock.BeepCount 1 "Should track beep call")

        // getForegroundColor / getForegroundColorExn
        testPropertyWithConfig fsCheckConfig "Console.getForegroundColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Red
            let eff = Console.getForegroundColor<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result ConsoleColor.Red "Should get foreground color")

        testPropertyWithConfig fsCheckConfig "Console.getForegroundColorExn returns color"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Blue
            let eff = Console.getForegroundColorExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result ConsoleColor.Blue "Should get foreground color")

        // setForegroundColor / setForegroundColorExn
        testPropertyWithConfig fsCheckConfig "Console.setForegroundColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setForegroundColor<string>(ConsoleColor.Green, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setForegroundColor should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.Green "Color should be set")

        testPropertyWithConfig fsCheckConfig "Console.setForegroundColorExn sets color"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setForegroundColorExn ConsoleColor.Yellow
                return! Console.getForegroundColorExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result ConsoleColor.Yellow "Color should be yellow")

        // getBackgroundColor / getBackgroundColorExn
        testPropertyWithConfig fsCheckConfig "Console.getBackgroundColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.DarkBlue
            let eff = Console.getBackgroundColor<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result ConsoleColor.DarkBlue "Should get background color")

        testPropertyWithConfig fsCheckConfig "Console.getBackgroundColorExn returns color"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.DarkGreen
            let eff = Console.getBackgroundColorExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result ConsoleColor.DarkGreen "Should get background color")

        // setBackgroundColor / setBackgroundColorExn
        testPropertyWithConfig fsCheckConfig "Console.setBackgroundColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setBackgroundColor<string>(ConsoleColor.Magenta, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setBackgroundColor should complete"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.Magenta "Color should be set")

        testPropertyWithConfig fsCheckConfig "Console.setBackgroundColorExn sets color"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setBackgroundColorExn ConsoleColor.Cyan
                return! Console.getBackgroundColorExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result ConsoleColor.Cyan "Color should be cyan")

        // getTitle / getTitleExn
        testPropertyWithConfig fsCheckConfig "Console.getTitle with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).Title <- "Test Title"
            let eff = Console.getTitle<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "Test Title" "Should get title")

        testPropertyWithConfig fsCheckConfig "Console.getTitleExn returns title"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).Title <- "My App"
            let eff = Console.getTitleExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "My App" "Should get title")

        // setTitle / setTitleExn
        testPropertyWithConfig fsCheckConfig "Console.setTitle with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setTitle<string>("New Title", fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setTitle should complete"
            Expect.equal (mock :> IConsoleBackend).Title "New Title" "Title should be set")

        testPropertyWithConfig fsCheckConfig "Console.setTitleExn sets title"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setTitleExn "FIO Console"
                return! Console.getTitleExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "FIO Console" "Title should be set")

        // isInputRedirected / isInputRedirectedExn
        testPropertyWithConfig fsCheckConfig "Console.isInputRedirected with error mapper"
        <| withMockBackend (fun _mock runtime ->
            let eff = Console.isInputRedirected<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Mock returns false for redirection")

        testPropertyWithConfig fsCheckConfig "Console.isInputRedirectedExn returns bool"
        <| withMockBackend (fun _mock runtime ->
            let eff = Console.isInputRedirectedExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Mock returns false for redirection")

        // isOutputRedirected / isOutputRedirectedExn
        testPropertyWithConfig fsCheckConfig "Console.isOutputRedirected with error mapper"
        <| withMockBackend (fun _mock runtime ->
            let eff = Console.isOutputRedirected<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Mock returns false for redirection")

        testPropertyWithConfig fsCheckConfig "Console.isOutputRedirectedExn returns bool"
        <| withMockBackend (fun _mock runtime ->
            let eff = Console.isOutputRedirectedExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Mock returns false for redirection")

        // isErrorRedirected / isErrorRedirectedExn
        testPropertyWithConfig fsCheckConfig "Console.isErrorRedirected with error mapper"
        <| withMockBackend (fun _mock runtime ->
            let eff = Console.isErrorRedirected<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Mock returns false for redirection")

        testPropertyWithConfig fsCheckConfig "Console.isErrorRedirectedExn returns bool"
        <| withMockBackend (fun _mock runtime ->
            let eff = Console.isErrorRedirectedExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Mock returns false for redirection")

        // resetColor / resetColorExn
        testPropertyWithConfig fsCheckConfig "Console.resetColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Red
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.Blue
            let eff = Console.resetColor<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "resetColor should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.Gray "Should reset to gray"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.Black "Should reset to black")

        testPropertyWithConfig fsCheckConfig "Console.resetColorExn resets colors"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Yellow
            let eff = Console.resetColorExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "resetColorExn should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.Gray "Should reset to gray")

        // setCursorPosition / setCursorPositionExn
        testPropertyWithConfig fsCheckConfig "Console.setCursorPosition with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setCursorPosition<string>(10, 20, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setCursorPosition should complete"
            Expect.equal ((mock :> IConsoleBackend).CursorLeft, (mock :> IConsoleBackend).CursorTop) (10, 20) "Position should be set")

        testPropertyWithConfig fsCheckConfig "Console.setCursorPositionExn sets position"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setCursorPositionExn(5, 10)
                return! Console.getCursorPositionExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (5, 10) "Position should be (5, 10)")

        // getCursorPosition / getCursorPositionExn
        testPropertyWithConfig fsCheckConfig "Console.getCursorPosition with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorLeft <- 15
            (mock :> IConsoleBackend).CursorTop <- 25
            let eff = Console.getCursorPosition<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (15, 25) "Should get cursor position")

        testPropertyWithConfig fsCheckConfig "Console.getCursorPositionExn returns tuple"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorLeft <- 30
            (mock :> IConsoleBackend).CursorTop <- 40
            let eff = Console.getCursorPositionExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result (30, 40) "Should get cursor position")

        // getCursorVisible / getCursorVisibleExn
        testPropertyWithConfig fsCheckConfig "Console.getCursorVisible with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorVisible <- false
            let eff = Console.getCursorVisible<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Should get cursor visibility")

        testPropertyWithConfig fsCheckConfig "Console.getCursorVisibleExn returns bool"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).CursorVisible <- true
            let eff = Console.getCursorVisibleExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Should get cursor visibility")

        // setCursorVisible / setCursorVisibleExn
        testPropertyWithConfig fsCheckConfig "Console.setCursorVisible with error mapper"
        <| withMockBackend (fun mock runtime ->
            let eff = Console.setCursorVisible<string>(false, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "setCursorVisible should complete"
            Expect.isFalse (mock :> IConsoleBackend).CursorVisible "Cursor should be hidden")

        testPropertyWithConfig fsCheckConfig "Console.setCursorVisibleExn sets visibility"
        <| withMockBackend (fun mock runtime ->
            let eff = fio {
                do! Console.setCursorVisibleExn false
                return! Console.getCursorVisibleExn
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isFalse result "Cursor should be hidden")

        // getWindowWidth / getWindowWidthExn
        testPropertyWithConfig fsCheckConfig "Console.getWindowWidth with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.SetWindowSize(200, 50)
            let eff = Console.getWindowWidth<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 200 "Should get window width")

        testPropertyWithConfig fsCheckConfig "Console.getWindowWidthExn returns width"
        <| withMockBackend (fun mock runtime ->
            mock.SetWindowSize(150, 40)
            let eff = Console.getWindowWidthExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 150 "Should get window width")

        // getWindowHeight / getWindowHeightExn
        testPropertyWithConfig fsCheckConfig "Console.getWindowHeight with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.SetWindowSize(100, 60)
            let eff = Console.getWindowHeight<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 60 "Should get window height")

        testPropertyWithConfig fsCheckConfig "Console.getWindowHeightExn returns height"
        <| withMockBackend (fun mock runtime ->
            mock.SetWindowSize(80, 24)
            let eff = Console.getWindowHeightExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 24 "Should get window height")

        // getBufferWidth / getBufferWidthExn
        testPropertyWithConfig fsCheckConfig "Console.getBufferWidth with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.SetBufferSize(250, 500)
            let eff = Console.getBufferWidth<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 250 "Should get buffer width")

        testPropertyWithConfig fsCheckConfig "Console.getBufferWidthExn returns width"
        <| withMockBackend (fun mock runtime ->
            mock.SetBufferSize(180, 400)
            let eff = Console.getBufferWidthExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 180 "Should get buffer width")

        // getBufferHeight / getBufferHeightExn
        testPropertyWithConfig fsCheckConfig "Console.getBufferHeight with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.SetBufferSize(120, 600)
            let eff = Console.getBufferHeight<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 600 "Should get buffer height")

        testPropertyWithConfig fsCheckConfig "Console.getBufferHeightExn returns height"
        <| withMockBackend (fun mock runtime ->
            mock.SetBufferSize(100, 1000)
            let eff = Console.getBufferHeightExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result 1000 "Should get buffer height")

        // printLines / printLinesExn
        testPropertyWithConfig fsCheckConfig "Console.printLines with error mapper"
        <| withMockBackend (fun mock runtime ->
            let lines = ["line1"; "line2"; "line3"]
            let eff = Console.printLines<string>(lines, fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printLines should complete"
            Expect.stringContains mock.StdOut "line1" "Should contain line1"
            Expect.stringContains mock.StdOut "line2" "Should contain line2"
            Expect.stringContains mock.StdOut "line3" "Should contain line3")

        testPropertyWithConfig fsCheckConfig "Console.printLinesExn writes multiple lines"
        <| withMockBackend (fun mock runtime ->
            let lines = ["first"; "second"]
            let eff = Console.printLinesExn lines

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "printLinesExn should complete"
            Expect.stringContains mock.StdOut "first" "Should contain first"
            Expect.stringContains mock.StdOut "second" "Should contain second")

        // readPassword / readPasswordExn
        testPropertyWithConfig fsCheckConfig "Console.readPassword with error mapper"
        <| withMockBackend (fun mock runtime ->
            mock.QueueString "secret123"
            let eff = Console.readPassword<string>(fun ex -> ex.Message)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "secret123" "Should read password"
            Expect.stringContains mock.StdOut "*" "Should mask with asterisks")

        testPropertyWithConfig fsCheckConfig "Console.readPassword handles backspace"
        <| withMockBackend (fun mock runtime ->
            mock.QueueCharKey 'a'
            mock.QueueCharKey 'b'
            mock.QueueSpecialKey ConsoleKey.Backspace
            mock.QueueCharKey 'c'
            mock.QueueSpecialKey ConsoleKey.Enter
            let eff = Console.readPasswordExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "ac" "Backspace should remove previous character")

        testPropertyWithConfig fsCheckConfig "Console.readPasswordExn reads masked input"
        <| withMockBackend (fun mock runtime ->
            mock.QueueString "mypass"
            let eff = Console.readPasswordExn

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result "mypass" "Should read password")

        // withForegroundColor / withForegroundColorExn
        testPropertyWithConfig fsCheckConfig "Console.withForegroundColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
            let eff = Console.withForegroundColor(ConsoleColor.Red, Console.writeExn "red text", id)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "withForegroundColor should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.White "Should restore original color")

        testPropertyWithConfig fsCheckConfig "Console.withForegroundColorExn restores color"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Gray
            let eff = Console.withForegroundColorExn(ConsoleColor.Green, Console.writeExn "green")

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "withForegroundColorExn should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.Gray "Should restore original color")

        // withBackgroundColor / withBackgroundColorExn
        testPropertyWithConfig fsCheckConfig "Console.withBackgroundColor with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.Black
            let eff = Console.withBackgroundColor(ConsoleColor.Blue, Console.writeExn "blue bg", id)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "withBackgroundColor should complete"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.Black "Should restore original color")

        testPropertyWithConfig fsCheckConfig "Console.withBackgroundColorExn restores color"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.DarkGray
            let eff = Console.withBackgroundColorExn(ConsoleColor.Yellow, Console.writeExn "yellow bg")

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "withBackgroundColorExn should complete"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.DarkGray "Should restore original color")

        // withColors / withColorsExn
        testPropertyWithConfig fsCheckConfig "Console.withColors with error mapper"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.White
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.Black
            let eff = Console.withColors(ConsoleColor.Red, ConsoleColor.Blue, Console.writeExn "colored", id)

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "withColors should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.White "Should restore foreground"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.Black "Should restore background")

        testPropertyWithConfig fsCheckConfig "Console.withColorsExn restores both colors"
        <| withMockBackend (fun mock runtime ->
            (mock :> IConsoleBackend).ForegroundColor <- ConsoleColor.Cyan
            (mock :> IConsoleBackend).BackgroundColor <- ConsoleColor.DarkMagenta
            let eff = Console.withColorsExn(ConsoleColor.Yellow, ConsoleColor.DarkRed, Console.writeExn "fancy")

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "withColorsExn should complete"
            Expect.equal (mock :> IConsoleBackend).ForegroundColor ConsoleColor.Cyan "Should restore foreground"
            Expect.equal (mock :> IConsoleBackend).BackgroundColor ConsoleColor.DarkMagenta "Should restore background")
    ])

