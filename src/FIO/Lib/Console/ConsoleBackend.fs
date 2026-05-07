namespace FIO.Console

open System
open System.IO

/// <summary>Represents an abstraction over console I/O operations, enabling test substitution of the system console.</summary>
type internal IConsoleBackend =
    /// <summary>Returns a line of text read from standard input.</summary>
    /// <returns>The next line from standard input.</returns>
    abstract ReadLine: unit -> string
    /// <summary>Returns a key press read from the console.</summary>
    /// <param name="intercept">When <c>true</c>, the key is not echoed to the display.</param>
    /// <returns>The key press information.</returns>
    abstract ReadKey: intercept: bool -> ConsoleKeyInfo
    /// <summary>Returns the next character code read from standard input.</summary>
    /// <returns>The character code, or <c>-1</c> when no input is available.</returns>
    abstract Read: unit -> int
    /// <summary>Returns whether a key press is available in the input buffer.</summary>
    /// <returns>true if a key press is available; false otherwise.</returns>
    abstract KeyAvailable: bool

    /// <summary>Creates a write of a message to standard output without a trailing newline.</summary>
    /// <param name="message">The text to write.</param>
    abstract Write: message: string -> unit
    /// <summary>Creates a write of a message to standard output followed by a newline.</summary>
    /// <param name="message">The text to write.</param>
    abstract WriteLine: message: string -> unit
    /// <summary>Creates a write of a blank line to standard output.</summary>
    abstract WriteBlankLine: unit -> unit
    /// <summary>Returns the standard output text writer.</summary>
    /// <returns>The standard output text writer.</returns>
    abstract Out: TextWriter

    /// <summary>Creates a write of a message to standard error without a trailing newline.</summary>
    /// <param name="message">The text to write.</param>
    abstract ErrorWrite: message: string -> unit
    /// <summary>Creates a write of a message to standard error followed by a newline.</summary>
    /// <param name="message">The text to write.</param>
    abstract ErrorWriteLine: message: string -> unit
    /// <summary>Returns the standard error text writer.</summary>
    /// <returns>The standard error text writer.</returns>
    abstract Error: TextWriter

    /// <summary>Returns or sets the column position of the cursor within the console buffer.</summary>
    /// <returns>The zero-based column position.</returns>
    abstract CursorLeft: int with get, set
    /// <summary>Returns or sets the row position of the cursor within the console buffer.</summary>
    /// <returns>The zero-based row position.</returns>
    abstract CursorTop: int with get, set
    /// <summary>Creates a repositioning of the cursor to the specified coordinates.</summary>
    /// <param name="left">The column position.</param>
    /// <param name="top">The row position.</param>
    abstract SetCursorPosition: left: int * top: int -> unit
    /// <summary>Returns the current cursor position as a column-row tuple.</summary>
    /// <returns>A tuple of <c>(left, top)</c>.</returns>
    abstract GetCursorPosition: unit -> int * int
    /// <summary>Returns or sets whether the cursor is visible in the console window.</summary>
    /// <returns>true if the cursor is visible; false otherwise.</returns>
    abstract CursorVisible: bool with get, set

    /// <summary>Returns or sets the foreground color of the console.</summary>
    /// <returns>The current foreground color.</returns>
    abstract ForegroundColor: ConsoleColor with get, set
    /// <summary>Returns or sets the background color of the console.</summary>
    /// <returns>The current background color.</returns>
    abstract BackgroundColor: ConsoleColor with get, set
    /// <summary>Creates a reset of the console foreground and background colors to their defaults.</summary>
    abstract ResetColor: unit -> unit

    /// <summary>Returns the width of the console window in columns.</summary>
    /// <returns>The width in character columns.</returns>
    abstract WindowWidth: int
    /// <summary>Returns the height of the console window in rows.</summary>
    /// <returns>The height in character rows.</returns>
    abstract WindowHeight: int
    /// <summary>Returns the width of the console buffer in columns.</summary>
    /// <returns>The width in character columns.</returns>
    abstract BufferWidth: int
    /// <summary>Returns the height of the console buffer in rows.</summary>
    /// <returns>The height in character rows.</returns>
    abstract BufferHeight: int
    /// <summary>Returns or sets the title displayed in the console window title bar.</summary>
    /// <returns>The current title string.</returns>
    abstract Title: string with get, set

    /// <summary>Returns whether standard input has been redirected from a file or pipe.</summary>
    /// <returns>true if input is redirected; false otherwise.</returns>
    abstract IsInputRedirected: bool
    /// <summary>Returns whether standard output has been redirected to a file or pipe.</summary>
    /// <returns>true if output is redirected; false otherwise.</returns>
    abstract IsOutputRedirected: bool
    /// <summary>Returns whether standard error has been redirected to a file or pipe.</summary>
    /// <returns>true if error output is redirected; false otherwise.</returns>
    abstract IsErrorRedirected: bool

    /// <summary>Creates a clear of the console buffer and display.</summary>
    abstract Clear: unit -> unit
    /// <summary>Creates an audible beep through the console speaker.</summary>
    abstract Beep: unit -> unit

/// <summary>Represents the default console backend that delegates to <c>System.Console</c>.</summary>
type internal SystemConsoleBackend() =
    interface IConsoleBackend with
        /// <summary>Returns a line of text from standard input.</summary>
        /// <returns>The line read, or null if no input is available.</returns>
        member _.ReadLine() = Console.ReadLine()

        /// <summary>Returns the next key press from the console.</summary>
        /// <param name="intercept">Whether to suppress displaying the pressed key.</param>
        /// <returns>The key press information.</returns>
        member _.ReadKey intercept = Console.ReadKey intercept

        /// <summary>Returns the next character from standard input.</summary>
        /// <returns>The character code, or -1 if no input is available.</returns>
        member _.Read() = Console.Read()

        /// <summary>Returns whether a key press is available in the input buffer.</summary>
        /// <returns>true if a key press is available; false otherwise.</returns>
        member _.KeyAvailable = Console.KeyAvailable

        /// <summary>Transforms standard output by writing the string representation of a value.</summary>
        /// <param name="message">The value to write.</param>
        member _.Write message = Console.Write message

        /// <summary>Transforms standard output by writing the string representation of a value followed by a newline.</summary>
        /// <param name="message">The value to write.</param>
        member _.WriteLine message = Console.WriteLine message

        /// <summary>Transforms standard output by writing a blank line.</summary>
        member _.WriteBlankLine() = Console.WriteLine()

        /// <summary>Returns the standard output text writer.</summary>
        /// <returns>The standard output text writer.</returns>
        member _.Out = Console.Out

        /// <summary>Transforms standard error by writing the string representation of a value.</summary>
        /// <param name="message">The value to write.</param>
        member _.ErrorWrite message = Console.Error.Write message

        /// <summary>Transforms standard error by writing the string representation of a value followed by a newline.</summary>
        /// <param name="message">The value to write.</param>
        member _.ErrorWriteLine message = Console.Error.WriteLine message

        /// <summary>Returns the standard error text writer.</summary>
        /// <returns>The standard error text writer.</returns>
        member _.Error = Console.Error

        /// <summary>Returns the column position of the cursor.</summary>
        /// <returns>The zero-based column position.</returns>
        member _.CursorLeft
            with get () = Console.CursorLeft
            and set v = Console.CursorLeft <- v

        /// <summary>Returns the row position of the cursor.</summary>
        /// <returns>The zero-based row position.</returns>
        member _.CursorTop
            with get () = Console.CursorTop
            and set v = Console.CursorTop <- v

        /// <summary>Transforms the cursor by moving it to the specified position.</summary>
        /// <param name="left">The zero-based column position.</param>
        /// <param name="top">The zero-based row position.</param>
        member _.SetCursorPosition(left, top) = Console.SetCursorPosition(left, top)

        /// <summary>Returns the current cursor position as a column-row tuple.</summary>
        /// <returns>A struct tuple of the zero-based column and row positions.</returns>
        member _.GetCursorPosition() =
            let struct (left, top) = Console.GetCursorPosition()
            left, top

        /// <summary>Returns whether the cursor is visible.</summary>
        /// <returns>true if the cursor is visible; false otherwise.</returns>
        member _.CursorVisible
            with get () = Console.CursorVisible
            and set v = Console.CursorVisible <- v

        /// <summary>Returns the foreground color of the console.</summary>
        /// <returns>The current foreground color.</returns>
        member _.ForegroundColor
            with get () = Console.ForegroundColor
            and set v = Console.ForegroundColor <- v

        /// <summary>Returns the background color of the console.</summary>
        /// <returns>The current background color.</returns>
        member _.BackgroundColor
            with get () = Console.BackgroundColor
            and set v = Console.BackgroundColor <- v

        /// <summary>Transforms the console by resetting foreground and background colors to their defaults.</summary>
        member _.ResetColor() = Console.ResetColor()

        /// <summary>Returns the width of the console window.</summary>
        /// <returns>The width in character columns.</returns>
        member _.WindowWidth = Console.WindowWidth

        /// <summary>Returns the height of the console window.</summary>
        /// <returns>The height in character rows.</returns>
        member _.WindowHeight = Console.WindowHeight

        /// <summary>Returns the width of the console buffer.</summary>
        /// <returns>The width in character columns.</returns>
        member _.BufferWidth = Console.BufferWidth

        /// <summary>Returns the height of the console buffer.</summary>
        /// <returns>The height in character rows.</returns>
        member _.BufferHeight = Console.BufferHeight

        /// <summary>Returns the title of the console window.</summary>
        /// <returns>The current title string.</returns>
        member _.Title
            with get () = Console.Title
            and set v = Console.Title <- v

        /// <summary>Returns whether standard input is redirected from a file or pipe.</summary>
        /// <returns>true if input is redirected; false otherwise.</returns>
        member _.IsInputRedirected = Console.IsInputRedirected

        /// <summary>Returns whether standard output is redirected to a file or pipe.</summary>
        /// <returns>true if output is redirected; false otherwise.</returns>
        member _.IsOutputRedirected = Console.IsOutputRedirected

        /// <summary>Returns whether standard error is redirected to a file or pipe.</summary>
        /// <returns>true if error output is redirected; false otherwise.</returns>
        member _.IsErrorRedirected = Console.IsErrorRedirected

        /// <summary>Transforms the console by clearing the buffer and moving the cursor to the origin.</summary>
        member _.Clear() = Console.Clear()

        /// <summary>Transforms the console by playing a beep sound through the speaker.</summary>
        member _.Beep() = Console.Beep()

/// <summary>Provides access to the active console backend, allowing test code to substitute implementations.</summary>
[<RequireQualifiedAccess>]
module internal ConsoleBackend =

    /// <summary>Represents the currently active console backend instance.</summary>
    let mutable private current: IConsoleBackend = SystemConsoleBackend()

    /// <summary>Returns the currently active console backend.</summary>
    /// <returns>The <c>IConsoleBackend</c> instance in use.</returns>
    let get () = current

    /// <summary>Transforms the active console backend to use the specified implementation.</summary>
    /// <param name="backend">The new backend to install.</param>
    let set (backend: IConsoleBackend) = current <- backend

    /// <summary>Transforms the active console backend by restoring the default system implementation.</summary>
    let reset () = current <- SystemConsoleBackend()
