namespace FIO.Console

open System
open System.IO

/// <summary>
/// Abstraction over System.Console operations for testability.
/// </summary>
type internal IConsoleBackend =
    /// <summary>Reads a line of text from stdin.</summary>
    abstract ReadLine: unit -> string
    /// <summary>Reads a key press from the console.</summary>
    /// <param name="intercept">If true, the key is not displayed.</param>
    abstract ReadKey: intercept:bool -> ConsoleKeyInfo
    /// <summary>Reads a single character from stdin as an int.</summary>
    abstract Read: unit -> int
    /// <summary>Gets whether a key press is available in the input buffer.</summary>
    abstract KeyAvailable: bool

    /// <summary>Writes text to stdout without a newline.</summary>
    abstract Write: message:string -> unit
    /// <summary>Writes text to stdout with a newline.</summary>
    abstract WriteLine: message:string -> unit
    /// <summary>Writes a blank line to stdout.</summary>
    abstract WriteBlankLine: unit -> unit
    /// <summary>Gets the stdout TextWriter for fprintf operations.</summary>
    abstract Out: TextWriter
    
    /// <summary>Writes text to stderr without a newline.</summary>
    abstract ErrorWrite: message:string -> unit
    /// <summary>Writes text to stderr with a newline.</summary>
    abstract ErrorWriteLine: message:string -> unit
    /// <summary>Gets the stderr TextWriter for fprintf operations.</summary>
    abstract Error: TextWriter
    
    /// <summary>Gets or sets the cursor column position.</summary>
    abstract CursorLeft: int with get, set
    /// <summary>Gets or sets the cursor row position.</summary>
    abstract CursorTop: int with get, set
    /// <summary>Sets the cursor position atomically.</summary>
    abstract SetCursorPosition: left:int * top:int -> unit
    /// <summary>Gets the cursor position atomically.</summary>
    abstract GetCursorPosition: unit -> int * int
    /// <summary>Gets or sets whether the cursor is visible.</summary>
    abstract CursorVisible: bool with get, set
    
    /// <summary>Gets or sets the foreground color.</summary>
    abstract ForegroundColor: ConsoleColor with get, set
    /// <summary>Gets or sets the background color.</summary>
    abstract BackgroundColor: ConsoleColor with get, set
    /// <summary>Resets foreground and background colors to defaults.</summary>
    abstract ResetColor: unit -> unit
    
    /// <summary>Gets the console window width in columns.</summary>
    abstract WindowWidth: int
    /// <summary>Gets the console window height in rows.</summary>
    abstract WindowHeight: int
    /// <summary>Gets the console buffer width in columns.</summary>
    abstract BufferWidth: int
    /// <summary>Gets the console buffer height in rows.</summary>
    abstract BufferHeight: int
    /// <summary>Gets or sets the console window title.</summary>
    abstract Title: string with get, set
    
    /// <summary>Gets whether stdin is redirected.</summary>
    abstract IsInputRedirected: bool
    /// <summary>Gets whether stdout is redirected.</summary>
    abstract IsOutputRedirected: bool
    /// <summary>Gets whether stderr is redirected.</summary>
    abstract IsErrorRedirected: bool
    
    /// <summary>Clears the console screen.</summary>
    abstract Clear: unit -> unit
    /// <summary>Plays a beep sound.</summary>
    abstract Beep: unit -> unit

/// <summary>
/// Real console backend that delegates to System.Console.
/// </summary>
type internal SystemConsoleBackend() =
    interface IConsoleBackend with
        member _.ReadLine () =
            Console.ReadLine()

        member _.ReadKey intercept =
            Console.ReadKey intercept

        member _.Read () =
            Console.Read()

        member _.KeyAvailable =
            Console.KeyAvailable
        
        member _.Write message =
            Console.Write message

        member _.WriteLine message =
            Console.WriteLine message

        member _.WriteBlankLine () =
            Console.WriteLine()

        member _.Out =
            Console.Out

        member _.ErrorWrite message =
            Console.Error.Write message

        member _.ErrorWriteLine message =
            Console.Error.WriteLine message

        member _.Error =
            Console.Error
        
        member _.CursorLeft
            with get() = Console.CursorLeft
            and set v = Console.CursorLeft <- v

        member _.CursorTop
            with get() = Console.CursorTop
            and set v = Console.CursorTop <- v

        member _.SetCursorPosition (left, top) =
            Console.SetCursorPosition(left, top)

        member _.GetCursorPosition () =
            let struct (left, top) = Console.GetCursorPosition()
            left, top

        member _.CursorVisible
            with get() = Console.CursorVisible
            and set v = Console.CursorVisible <- v

        member _.ForegroundColor
            with get() = Console.ForegroundColor
            and set v = Console.ForegroundColor <- v

        member _.BackgroundColor
            with get() = Console.BackgroundColor
            and set v = Console.BackgroundColor <- v

        member _.ResetColor () =
            Console.ResetColor()

        member _.WindowWidth =
            Console.WindowWidth

        member _.WindowHeight =
            Console.WindowHeight

        member _.BufferWidth =
            Console.BufferWidth

        member _.BufferHeight =
            Console.BufferHeight

        member _.Title
            with get() = Console.Title
            and set v = Console.Title <- v

        member _.IsInputRedirected =
            Console.IsInputRedirected

        member _.IsOutputRedirected =
            Console.IsOutputRedirected

        member _.IsErrorRedirected =
            Console.IsErrorRedirected

        member _.Clear () =
            Console.Clear()

        member _.Beep () =
            Console.Beep()

/// <summary>
/// Global console backend configuration for testing.
/// </summary>
[<RequireQualifiedAccess>]
module internal ConsoleBackend =

    let mutable private current: IConsoleBackend =
        SystemConsoleBackend()
    
    /// <summary>Gets the current console backend.</summary>
    let get () =
        current
    
    /// <summary>Sets the console backend (for testing).</summary>
    /// <param name="backend">The IConsoleBackend implementation to use.</param>
    let set (backend: IConsoleBackend) =
        current <- backend
    
    /// <summary>Resets to the real System.Console backend.</summary>
    let reset () =
        current <- SystemConsoleBackend()
