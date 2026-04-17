namespace FIO.Console

open System
open System.IO

/// Abstraction over System.Console operations for testability.
type internal IConsoleBackend =
    /// Reads a line of text from stdin.
    /// <returns>The line read from stdin.</returns>
    abstract ReadLine: unit -> string
    /// Reads a key press from the console.
    /// <param name="intercept">Whether to hide the key from display.</param>
    /// <returns>The key press information.</returns>
    abstract ReadKey: intercept: bool -> ConsoleKeyInfo
    /// Reads a single character from stdin (-1 if no more characters available).
    /// <returns>The character code, or -1 if no more characters are available.</returns>
    abstract Read: unit -> int
    /// Gets whether a key press is available in the input buffer.
    /// <returns>True if a key press is available; otherwise, false.</returns>
    abstract KeyAvailable: bool

    /// Writes text to stdout without a newline.
    /// <param name="message">The text to write.</param>
    /// <returns>Unit.</returns>
    abstract Write: message: string -> unit
    /// Writes text to stdout with a newline.
    /// <param name="message">The text to write.</param>
    /// <returns>Unit.</returns>
    abstract WriteLine: message: string -> unit
    /// Writes a blank line to stdout.
    /// <returns>Unit.</returns>
    abstract WriteBlankLine: unit -> unit
    /// Gets the stdout TextWriter for fprintf operations.
    /// <returns>The stdout TextWriter.</returns>
    abstract Out: TextWriter

    /// Writes text to stderr without a newline.
    /// <param name="message">The text to write.</param>
    /// <returns>Unit.</returns>
    abstract ErrorWrite: message: string -> unit
    /// Writes text to stderr with a newline.
    /// <param name="message">The text to write.</param>
    /// <returns>Unit.</returns>
    abstract ErrorWriteLine: message: string -> unit
    /// Gets the stderr TextWriter for fprintf operations.
    /// <returns>The stderr TextWriter.</returns>
    abstract Error: TextWriter

    /// Gets or sets the cursor column position.
    /// <returns>The current cursor column position.</returns>
    abstract CursorLeft: int with get, set
    /// Gets or sets the cursor row position.
    /// <returns>The current cursor row position.</returns>
    abstract CursorTop: int with get, set
    /// Sets the cursor position atomically.
    /// <param name="left">The column position.</param>
    /// <param name="top">The row position.</param>
    /// <returns>Unit.</returns>
    abstract SetCursorPosition: left: int * top: int -> unit
    /// Gets the cursor position as (column, row).
    /// <returns>The cursor position as (column, row).</returns>
    abstract GetCursorPosition: unit -> int * int
    /// Gets or sets whether the cursor is visible.
    /// <returns>True if the cursor is visible; otherwise, false.</returns>
    abstract CursorVisible: bool with get, set

    /// Gets or sets the foreground color.
    /// <returns>The current foreground color.</returns>
    abstract ForegroundColor: ConsoleColor with get, set
    /// Gets or sets the background color.
    /// <returns>The current background color.</returns>
    abstract BackgroundColor: ConsoleColor with get, set
    /// Resets foreground and background colors to defaults.
    /// <returns>Unit.</returns>
    abstract ResetColor: unit -> unit

    /// Gets the console window width in columns.
    /// <returns>The window width in columns.</returns>
    abstract WindowWidth: int
    /// Gets the console window height in rows.
    /// <returns>The window height in rows.</returns>
    abstract WindowHeight: int
    /// Gets the console buffer width in columns.
    /// <returns>The buffer width in columns.</returns>
    abstract BufferWidth: int
    /// Gets the console buffer height in rows.
    /// <returns>The buffer height in rows.</returns>
    abstract BufferHeight: int
    /// Gets or sets the console window title.
    /// <returns>The current console window title.</returns>
    abstract Title: string with get, set

    /// Gets whether stdin is redirected.
    /// <returns>True if stdin is redirected; otherwise, false.</returns>
    abstract IsInputRedirected: bool
    /// Gets whether stdout is redirected.
    /// <returns>True if stdout is redirected; otherwise, false.</returns>
    abstract IsOutputRedirected: bool
    /// Gets whether stderr is redirected.
    /// <returns>True if stderr is redirected; otherwise, false.</returns>
    abstract IsErrorRedirected: bool

    /// Clears the console screen.
    /// <returns>Unit.</returns>
    abstract Clear: unit -> unit
    /// Plays a beep sound.
    /// <returns>Unit.</returns>
    abstract Beep: unit -> unit

/// Real console backend that delegates to System.Console.
type internal SystemConsoleBackend() =
    interface IConsoleBackend with
        member _.ReadLine() = Console.ReadLine()

        member _.ReadKey intercept = Console.ReadKey intercept

        member _.Read() = Console.Read()

        member _.KeyAvailable = Console.KeyAvailable

        member _.Write message = Console.Write message

        member _.WriteLine message = Console.WriteLine message

        member _.WriteBlankLine() = Console.WriteLine()

        member _.Out = Console.Out

        member _.ErrorWrite message = Console.Error.Write message

        member _.ErrorWriteLine message = Console.Error.WriteLine message

        member _.Error = Console.Error

        member _.CursorLeft
            with get () = Console.CursorLeft
            and set v = Console.CursorLeft <- v

        member _.CursorTop
            with get () = Console.CursorTop
            and set v = Console.CursorTop <- v

        member _.SetCursorPosition(left, top) = Console.SetCursorPosition(left, top)

        member _.GetCursorPosition() =
            let struct (left, top) = Console.GetCursorPosition()
            left, top

        member _.CursorVisible
            with get () = Console.CursorVisible
            and set v = Console.CursorVisible <- v

        member _.ForegroundColor
            with get () = Console.ForegroundColor
            and set v = Console.ForegroundColor <- v

        member _.BackgroundColor
            with get () = Console.BackgroundColor
            and set v = Console.BackgroundColor <- v

        member _.ResetColor() = Console.ResetColor()

        member _.WindowWidth = Console.WindowWidth

        member _.WindowHeight = Console.WindowHeight

        member _.BufferWidth = Console.BufferWidth

        member _.BufferHeight = Console.BufferHeight

        member _.Title
            with get () = Console.Title
            and set v = Console.Title <- v

        member _.IsInputRedirected = Console.IsInputRedirected

        member _.IsOutputRedirected = Console.IsOutputRedirected

        member _.IsErrorRedirected = Console.IsErrorRedirected

        member _.Clear() = Console.Clear()

        member _.Beep() = Console.Beep()

/// Global console backend configuration for testing.
/// Thread safety: the backend is process-global. Tests using set/reset
/// MUST run under testSequenced to avoid non-deterministic failures.
[<RequireQualifiedAccess>]
module internal ConsoleBackend =

    /// The current console backend instance.
    let mutable private current: IConsoleBackend = SystemConsoleBackend()

    /// Gets the current console backend.
    /// <returns>The current console backend instance.</returns>
    let get () = current

    /// Sets the console backend (for testing).
    /// <param name="backend">The console backend to use.</param>
    /// <returns>Unit.</returns>
    let set (backend: IConsoleBackend) = current <- backend

    /// Resets to the real System.Console backend.
    /// <returns>Unit.</returns>
    let reset () = current <- SystemConsoleBackend()
