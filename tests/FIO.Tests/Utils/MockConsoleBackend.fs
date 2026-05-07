/// <summary>Provides a mock console backend for deterministic testing of console I/O effects.</summary>
module FIO.Tests.MockConsoleBackend

open FIO.Console

open System
open System.IO
open System.Text
open System.Collections.Generic

/// <summary>Represents a mock console backend that captures all output and allows injecting input for deterministic testing.</summary>
type MockConsoleBackend() =
    let mutable stdoutBuffer = StringBuilder()
    let mutable stderrBuffer = StringBuilder()
    let stdinQueue = Queue<string>()
    let keyQueue = Queue<ConsoleKeyInfo>()
    let mutable cursorLeft = 0
    let mutable cursorTop = 0
    let mutable cursorVisible = true
    let mutable foregroundColor = ConsoleColor.Gray
    let mutable backgroundColor = ConsoleColor.Black
    let mutable windowWidth = 120
    let mutable windowHeight = 30
    let mutable bufferWidth = 120
    let mutable bufferHeight = 300
    let mutable title = ""
    let mutable clearCount = 0
    let mutable beepCount = 0
    let mutable shouldThrow: exn option = None
    let mutable isInputRedirected = false
    let mutable isOutputRedirected = false
    let mutable isErrorRedirected = false

    let checkThrow () =
        match shouldThrow with
        | Some ex ->
            shouldThrow <- None
            raise ex
        | None -> ()

    let stdoutWriter =
        { new StringWriter(stdoutBuffer) with
            member _.Encoding = Encoding.UTF8
        }

    let stderrWriter =
        { new StringWriter(stderrBuffer) with
            member _.Encoding = Encoding.UTF8
        }

    /// <summary>Returns all text written to stdout.</summary>
    /// <returns>The accumulated stdout content as a string.</returns>
    member _.StdOut = stdoutBuffer.ToString()

    /// <summary>Returns all text written to stderr.</summary>
    /// <returns>The accumulated stderr content as a string.</returns>
    member _.StdErr = stderrBuffer.ToString()

    /// <summary>Returns the number of times Clear was called.</summary>
    /// <returns>The clear invocation count.</returns>
    member _.ClearCount = clearCount

    /// <summary>Returns the number of times Beep was called.</summary>
    /// <returns>The beep invocation count.</returns>
    member _.BeepCount = beepCount

    /// <summary>Returns or sets the exception to throw on the next backend call, resetting to None after throwing.</summary>
    member _.ShouldThrow
        with get () = shouldThrow
        and set v = shouldThrow <- v

    /// <summary>Returns or sets whether stdin is reported as redirected.</summary>
    member _.IsInputRedirected
        with get () = isInputRedirected
        and set v = isInputRedirected <- v

    /// <summary>Returns or sets whether stdout is reported as redirected.</summary>
    member _.IsOutputRedirected
        with get () = isOutputRedirected
        and set v = isOutputRedirected <- v

    /// <summary>Returns or sets whether stderr is reported as redirected.</summary>
    member _.IsErrorRedirected
        with get () = isErrorRedirected
        and set v = isErrorRedirected <- v

    /// <summary>Transforms the input queue by appending a line to be returned by ReadLine.</summary>
    /// <param name="line">The line of text to queue.</param>
    member _.QueueInputLine(line: string) = stdinQueue.Enqueue line

    /// <summary>Transforms the input queue by appending multiple lines to be returned by ReadLine.</summary>
    /// <param name="lines">The lines of text to queue.</param>
    member this.QueueInputLines(lines: string seq) =
        for line in lines do
            this.QueueInputLine line

    /// <summary>Transforms the key queue by appending a key press to be returned by ReadKey.</summary>
    /// <param name="key">The ConsoleKeyInfo to queue.</param>
    member _.QueueKey(key: ConsoleKeyInfo) = keyQueue.Enqueue key

    /// <summary>Transforms the key queue by appending a simple character key press.</summary>
    /// <param name="c">The character to queue as a key press.</param>
    member this.QueueCharKey(c: char) =
        this.QueueKey(ConsoleKeyInfo(c, enum<ConsoleKey> (int (Char.ToUpper c)), false, false, false))

    /// <summary>Transforms the key queue by appending a special key press (Enter, Backspace, Escape, etc.).</summary>
    /// <param name="key">The ConsoleKey to queue.</param>
    member this.QueueSpecialKey(key: ConsoleKey) =
        this.QueueKey(ConsoleKeyInfo('\000', key, false, false, false))

    /// <summary>Transforms the key queue by appending each character as a key press followed by Enter.</summary>
    /// <param name="s">The string to queue as individual key presses.</param>
    member this.QueueString(s: string) =
        for c in s do
            this.QueueCharKey c

        this.QueueSpecialKey ConsoleKey.Enter

    /// <summary>Transforms all captured state back to initial values, clearing buffers, queues, and counters.</summary>
    member _.Reset() =
        stdoutBuffer.Clear() |> ignore
        stderrBuffer.Clear() |> ignore
        stdinQueue.Clear()
        keyQueue.Clear()
        cursorLeft <- 0
        cursorTop <- 0
        cursorVisible <- true
        foregroundColor <- ConsoleColor.Gray
        backgroundColor <- ConsoleColor.Black
        clearCount <- 0
        beepCount <- 0
        title <- ""
        shouldThrow <- None
        isInputRedirected <- false
        isOutputRedirected <- false
        isErrorRedirected <- false

    /// <summary>Transforms the reported window dimensions to the specified size.</summary>
    /// <param name="width">The window width in columns.</param>
    /// <param name="height">The window height in rows.</param>
    member _.SetWindowSize(width: int, height: int) =
        windowWidth <- width
        windowHeight <- height

    /// <summary>Transforms the reported buffer dimensions to the specified size.</summary>
    /// <param name="width">The buffer width in columns.</param>
    /// <param name="height">The buffer height in rows.</param>
    member _.SetBufferSize(width: int, height: int) =
        bufferWidth <- width
        bufferHeight <- height

    interface IConsoleBackend with
        member _.ReadLine() =
            checkThrow ()
            if stdinQueue.Count > 0 then stdinQueue.Dequeue() else null

        member _.ReadKey _intercept =
            checkThrow ()

            if keyQueue.Count > 0 then
                keyQueue.Dequeue()
            else
                ConsoleKeyInfo('\000', ConsoleKey.Enter, false, false, false)

        member _.Read() =
            checkThrow ()

            if stdinQueue.Count > 0 then
                let line = stdinQueue.Peek()
                if line.Length > 0 then int line[0] else -1
            else
                -1

        member _.KeyAvailable =
            checkThrow ()
            keyQueue.Count > 0

        member _.Write message =
            checkThrow ()
            stdoutBuffer.Append message |> ignore

        member _.WriteLine message =
            checkThrow ()
            stdoutBuffer.AppendLine message |> ignore

        member _.WriteBlankLine() =
            checkThrow ()
            stdoutBuffer.AppendLine() |> ignore

        member _.Out = stdoutWriter :> TextWriter

        member _.ErrorWrite message =
            checkThrow ()
            stderrBuffer.Append message |> ignore

        member _.ErrorWriteLine message =
            checkThrow ()
            stderrBuffer.AppendLine message |> ignore

        member _.Error = stderrWriter :> TextWriter

        member _.CursorLeft
            with get () = cursorLeft
            and set v = cursorLeft <- v

        member _.CursorTop
            with get () = cursorTop
            and set v = cursorTop <- v

        member _.SetCursorPosition(left, top) =
            cursorLeft <- left
            cursorTop <- top

        member _.GetCursorPosition() = cursorLeft, cursorTop

        member _.CursorVisible
            with get () = cursorVisible
            and set v = cursorVisible <- v

        member _.ForegroundColor
            with get () = foregroundColor
            and set v = foregroundColor <- v

        member _.BackgroundColor
            with get () = backgroundColor
            and set v = backgroundColor <- v

        member _.ResetColor() =
            foregroundColor <- ConsoleColor.Gray
            backgroundColor <- ConsoleColor.Black

        member _.WindowWidth = windowWidth

        member _.WindowHeight = windowHeight

        member _.BufferWidth = bufferWidth

        member _.BufferHeight = bufferHeight

        member _.Title
            with get () = title
            and set v = title <- v

        member _.IsInputRedirected = isInputRedirected

        member _.IsOutputRedirected = isOutputRedirected

        member _.IsErrorRedirected = isErrorRedirected

        member _.Clear() = clearCount <- clearCount + 1

        member _.Beep() = beepCount <- beepCount + 1
