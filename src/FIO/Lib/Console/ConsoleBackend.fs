namespace FIO.Console

open System
open System.IO

/// Abstraction over System.Console operations for testability.
type internal IConsoleBackend =
    /// Reads a line of text from stdin.
    abstract ReadLine: unit -> string
    /// Reads a key press from the console.
    abstract ReadKey: intercept: bool -> ConsoleKeyInfo
    /// Reads a single character from stdin (-1 if no more characters available).
    abstract Read: unit -> int
    /// Gets whether a key press is available in the input buffer.
    abstract KeyAvailable: bool

    /// Writes text to stdout without a newline.
    abstract Write: message: string -> unit
    /// Writes text to stdout with a newline.
    abstract WriteLine: message: string -> unit
    /// Writes a blank line to stdout.
    abstract WriteBlankLine: unit -> unit
    /// Gets the stdout TextWriter for fprintf operations.
    abstract Out: TextWriter

    /// Writes text to stderr without a newline.
    abstract ErrorWrite: message: string -> unit
    /// Writes text to stderr with a newline.
    abstract ErrorWriteLine: message: string -> unit
    /// Gets the stderr TextWriter for fprintf operations.
    abstract Error: TextWriter

    /// Gets or sets the cursor column position.
    abstract CursorLeft: int with get, set
    /// Gets or sets the cursor row position.
    abstract CursorTop: int with get, set
    /// Sets the cursor position atomically.
    abstract SetCursorPosition: left: int * top: int -> unit
    /// Gets the cursor position as (column, row).
    abstract GetCursorPosition: unit -> int * int
    /// Gets or sets whether the cursor is visible.
    abstract CursorVisible: bool with get, set

    /// Gets or sets the foreground color.
    abstract ForegroundColor: ConsoleColor with get, set
    /// Gets or sets the background color.
    abstract BackgroundColor: ConsoleColor with get, set
    /// Resets foreground and background colors to defaults.
    abstract ResetColor: unit -> unit

    /// Gets the console window width in columns.
    abstract WindowWidth: int
    /// Gets the console window height in rows.
    abstract WindowHeight: int
    /// Gets the console buffer width in columns.
    abstract BufferWidth: int
    /// Gets the console buffer height in rows.
    abstract BufferHeight: int
    /// Gets or sets the console window title.
    abstract Title: string with get, set

    /// Gets whether stdin is redirected.
    abstract IsInputRedirected: bool
    /// Gets whether stdout is redirected.
    abstract IsOutputRedirected: bool
    /// Gets whether stderr is redirected.
    abstract IsErrorRedirected: bool

    /// Clears the console screen.
    abstract Clear: unit -> unit
    /// Plays a beep sound.
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
    let get () = current

    /// Sets the console backend (for testing).
    let set (backend: IConsoleBackend) = current <- backend

    /// Resets to the real System.Console backend.
    let reset () = current <- SystemConsoleBackend()
