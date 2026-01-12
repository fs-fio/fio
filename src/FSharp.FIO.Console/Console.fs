/// <summary>
/// Provides functional, effectful console I/O operations for FIO.
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.FIO.Console

open FSharp.FIO.DSL

open System

/// <summary>
/// Prints a formatted message to stdout (no newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline PrintMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> printf format), onError)

/// <summary>
/// Prints a formatted message to stdout (no newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline Print (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintMapError<exn>(format, id)

/// <summary>
/// Prints a formatted message to stderr (no newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline PrintErrorMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> eprintf format), onError)

/// <summary>
/// Prints a formatted message to stderr (no newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline PrintError (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintErrorMapError<exn>(format, id)

/// <summary>
/// Prints a formatted message to stdout (with newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline PrintLineMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> printfn format), onError)

/// <summary>
/// Prints a formatted message to stdout (with newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline PrintLine (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintLineMapError<exn>(format, id)

/// <summary>
/// Prints a formatted message to stderr (with newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline PrintErrorLineMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> eprintfn format), onError)

/// <summary>
/// Prints a formatted message to stderr (with newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline PrintErrorLine (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintErrorLineMapError<exn>(format, id)

/// <summary>
/// Reads a line from stdin.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline ReadLineMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.Attempt<string, 'E>((fun () -> Console.ReadLine()), onError)

/// <summary>
/// Reads a line from stdin.
/// </summary>
let ReadLine : FIO<string, exn> =
    ReadLineMapError<exn> id

/// <summary>
/// Reads a single key from stdin.
/// </summary>
/// <param name="intercept">Whether to hide the key from display.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline ReadKeyMapError<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    FIO.Attempt<ConsoleKeyInfo, 'E>((fun () -> Console.ReadKey intercept), onError)

/// <summary>
/// Reads a single key from stdin.
/// </summary>
/// <param name="intercept">Whether to hide the key from display.</param>
let inline ReadKey (intercept: bool) : FIO<ConsoleKeyInfo, exn> =
    ReadKeyMapError<exn>(intercept, id)

/// <summary>
/// Reads a single character from stdin as an int.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline ReadMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.Read()), onError)

/// <summary>
/// Reads a single character from stdin as an int.
/// </summary>
let Read : FIO<int, exn> =
    ReadMapError<exn> id

/// <summary>
/// Writes a message to stdout (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline WriteMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.Write message), onError)

/// <summary>
/// Writes a message to stdout (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline Write (message: string) : FIO<unit, exn> =
    WriteMapError<exn>(message, id)

/// <summary>
/// Writes a message to stdout (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline WriteLineMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.WriteLine message), onError)

/// <summary>
/// Writes a message to stdout (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline WriteLine (message: string) : FIO<unit, exn> =
    WriteLineMapError<exn>(message, id)

/// <summary>
/// Writes a message to stderr (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline WriteErrorMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.Error.Write message), onError)

/// <summary>
/// Writes a message to stderr (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline WriteError (message: string) : FIO<unit, exn> =
    WriteErrorMapError<exn>(message, id)

/// <summary>
/// Writes a message to stderr (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline WriteErrorLineMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.Error.WriteLine message), onError)

/// <summary>
/// Writes a message to stderr (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline WriteErrorLine (message: string) : FIO<unit, exn> =
    WriteErrorLineMapError<exn>(message, id)

/// <summary>
/// Clears the console.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline ClearMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.Clear()), onError)

/// <summary>
/// Clears the console.
/// </summary>
let Clear : FIO<unit, exn> =
    ClearMapError<exn> id

/// <summary>
/// Sets the cursor column position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline SetCursorLeftMapError<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.CursorLeft <- left), onError)

/// <summary>
/// Sets the cursor column position.
/// </summary>
/// <param name="left">The column position.</param>
let inline SetCursorLeft (left: int) : FIO<unit, exn> =
    SetCursorLeftMapError<exn>(left, id)

/// <summary>
/// Sets the cursor row position.
/// </summary>
/// <param name="top">The row position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline SetCursorTopMapError<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.CursorTop <- top), onError)

/// <summary>
/// Sets the cursor row position.
/// </summary>
/// <param name="top">The row position.</param>
let inline SetCursorTop (top: int) : FIO<unit, exn> =
    SetCursorTopMapError<exn>(top, id)

/// <summary>
/// Gets the cursor column position.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetCursorLeftMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.CursorLeft), onError)

/// <summary>
/// Gets the cursor column position.
/// </summary>
let GetCursorLeft : FIO<int, exn> =
    GetCursorLeftMapError<exn> id

/// <summary>
/// Gets the cursor row position.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetCursorTopMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.CursorTop), onError)

/// <summary>
/// Gets the cursor row position.
/// </summary>
let GetCursorTop : FIO<int, exn> =
    GetCursorTopMapError<exn> id

/// <summary>
/// Sounds the system bell.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline BeepMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.Beep()), onError)

/// <summary>
/// Sounds the system bell.
/// </summary>
let Beep : FIO<unit, exn> =
    BeepMapError<exn> id

/// <summary>
/// Gets the console foreground color.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetForegroundColorMapError<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.Attempt<ConsoleColor, 'E>((fun () -> Console.ForegroundColor), onError)

/// <summary>
/// Gets the console foreground color.
/// </summary>
let GetForegroundColor : FIO<ConsoleColor, exn> =
    GetForegroundColorMapError<exn> id

/// <summary>
/// Sets the console foreground color.
/// </summary>
/// <param name="color">The color to set.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline SetForegroundColorMapError<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.ForegroundColor <- color), onError)

/// <summary>
/// Sets the console foreground color.
/// </summary>
/// <param name="color">The color to set.</param>
let inline SetForegroundColor (color: ConsoleColor) : FIO<unit, exn> =
    SetForegroundColorMapError<exn>(color, id)

/// <summary>
/// Gets the console background color.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetBackgroundColorMapError<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.Attempt<ConsoleColor, 'E>((fun () -> Console.BackgroundColor), onError)

/// <summary>
/// Gets the console background color.
/// </summary>
let GetBackgroundColor : FIO<ConsoleColor, exn> =
    GetBackgroundColorMapError<exn> id

/// <summary>
/// Sets the console background color.
/// </summary>
/// <param name="color">The color to set.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline SetBackgroundColorMapError<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.BackgroundColor <- color), onError)

/// <summary>
/// Sets the console background color.
/// </summary>
/// <param name="color">The color to set.</param>
let inline SetBackgroundColor (color: ConsoleColor) : FIO<unit, exn> =
    SetBackgroundColorMapError<exn>(color, id)

/// <summary>
/// Gets the console window title.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetTitleMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.Attempt<string, 'E>((fun () -> Console.Title), onError)

/// <summary>
/// Gets the console window title.
/// </summary>
let GetTitle : FIO<string, exn> =
    GetTitleMapError<exn> id

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The title to set.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline SetTitleMapError<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.Title <- title), onError)

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The title to set.</param>
let inline SetTitle (title: string) : FIO<unit, exn> =
    SetTitleMapError<exn>(title, id)

/// <summary>
/// Checks whether stdin is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline IsInputRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.Attempt<bool, 'E>((fun () -> Console.IsInputRedirected), onError)

/// <summary>
/// Checks whether stdin is redirected.
/// </summary>
let IsInputRedirected : FIO<bool, exn> =
    IsInputRedirectedMapError<exn> id

/// <summary>
/// Checks whether stdout is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline IsOutputRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.Attempt<bool, 'E>((fun () -> Console.IsOutputRedirected), onError)

/// <summary>
/// Checks whether stdout is redirected.
/// </summary>
let IsOutputRedirected : FIO<bool, exn> =
    IsOutputRedirectedMapError<exn> id

/// <summary>
/// Checks whether stderr is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline IsErrorRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.Attempt<bool, 'E>((fun () -> Console.IsErrorRedirected), onError)

/// <summary>
/// Checks whether stderr is redirected.
/// </summary>
let IsErrorRedirected : FIO<bool, exn> =
    IsErrorRedirectedMapError<exn> id

/// <summary>
/// Resets the console colors to their defaults.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline ResetColorMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.ResetColor()), onError)

/// <summary>
/// Resets the console colors to their defaults.
/// </summary>
let ResetColor : FIO<unit, exn> =
    ResetColorMapError<exn> id

/// <summary>
/// Sets the cursor position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="top">The row position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline SetCursorPositionMapError<'E> (left: int, top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () -> Console.SetCursorPosition(left, top)), onError)

/// <summary>
/// Sets the cursor position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="top">The row position.</param>
let inline SetCursorPosition (left: int, top: int) : FIO<unit, exn> =
    SetCursorPositionMapError<exn>(left, top, id)

/// <summary>
/// Gets the cursor position as (column, row).
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetCursorPositionMapError<'E> (onError: exn -> 'E) : FIO<int * int, 'E> =
    FIO.Attempt<int * int, 'E>((fun () -> Console.CursorLeft, Console.CursorTop), onError)

/// <summary>
/// Gets the cursor position as (column, row).
/// </summary>
let GetCursorPosition : FIO<int * int, exn> =
    GetCursorPositionMapError<exn> id

/// <summary>
/// Gets the console window width.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetWindowWidthMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.WindowWidth), onError)

/// <summary>
/// Gets the console window width.
/// </summary>
let GetWindowWidth : FIO<int, exn> =
    GetWindowWidthMapError<exn> id

/// <summary>
/// Gets the console window height.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetWindowHeightMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.WindowHeight), onError)

/// <summary>
/// Gets the console window height.
/// </summary>
let GetWindowHeight : FIO<int, exn> =
    GetWindowHeightMapError<exn> id

/// <summary>
/// Gets the console buffer width.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetBufferWidthMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.BufferWidth), onError)

/// <summary>
/// Gets the console buffer width.
/// </summary>
let GetBufferWidth : FIO<int, exn> =
    GetBufferWidthMapError<exn> id

/// <summary>
/// Gets the console buffer height.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline GetBufferHeightMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.Attempt<int, 'E>((fun () -> Console.BufferHeight), onError)

/// <summary>
/// Gets the console buffer height.
/// </summary>
let GetBufferHeight : FIO<int, exn> =
    GetBufferHeightMapError<exn> id

/// <summary>
/// Writes multiple lines to stdout with buffering.
/// </summary>
/// <param name="lines">The lines to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline PrintLinesMapError<'E> (lines: string seq, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.Attempt<unit, 'E>((fun () ->
        use writer = Console.Out
        for line in lines do
            writer.WriteLine line
        writer.Flush()), onError)

/// <summary>
/// Writes multiple lines to stdout with buffering.
/// </summary>
/// <param name="lines">The lines to write.</param>
let inline PrintLines (lines: string seq) : FIO<unit, exn> =
    PrintLinesMapError<exn>(lines, id)

/// <summary>
/// Reads a password from stdin with masked input.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline ReadPasswordMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.Attempt<string, 'E>((fun () ->
        let mutable password = ""
        let mutable key = Console.ReadKey true
        while key.Key <> ConsoleKey.Enter do
            if key.Key = ConsoleKey.Backspace && password.Length > 0 then
                password <- password.Substring(0, password.Length - 1)
                Console.Write "\b \b"
            elif key.Key <> ConsoleKey.Backspace then
                password <- password + string key.KeyChar
                Console.Write "*"
            key <- Console.ReadKey true
        Console.WriteLine()
        password), onError)

/// <summary>
/// Reads a password from stdin with masked input.
/// </summary>
let ReadPassword : FIO<string, exn> =
    ReadPasswordMapError<exn> id

/// <summary>
/// Executes an action with a temporary foreground color.
/// </summary>
/// <param name="color">The foreground color to use.</param>
/// <param name="action">The action to execute.</param>
let WithForegroundColor (color: ConsoleColor, action: FIO<'R, exn>) : FIO<'R, exn> =
    FIO.AcquireRelease(
        GetForegroundColor,
        SetForegroundColor,
        fun _ -> fio {
            do! SetForegroundColor color
            return! action
        })

/// <summary>
/// Executes an action with a temporary foreground color.
/// </summary>
/// <param name="color">The foreground color to use.</param>
/// <param name="action">The action to execute.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let WithForegroundColorMapError<'R, 'E> (color: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.AcquireRelease(
        GetForegroundColorMapError onError,
        (fun c -> SetForegroundColorMapError(c, onError)),
        fun _ -> fio {
            do! SetForegroundColorMapError(color, onError)
            return! action
        })

/// <summary>
/// Executes an action with a temporary background color.
/// </summary>
/// <param name="color">The background color to use.</param>
/// <param name="action">The action to execute.</param>
let WithBackgroundColor (color: ConsoleColor, action: FIO<'R, exn>) : FIO<'R, exn> =
    FIO.AcquireRelease(
        GetBackgroundColor,
        SetBackgroundColor,
        fun _ -> fio {
            do! SetBackgroundColor color
            return! action
        })

/// <summary>
/// Executes an action with a temporary background color.
/// </summary>
/// <param name="color">The background color to use.</param>
/// <param name="action">The action to execute.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let WithBackgroundColorMapError<'R, 'E> (color: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.AcquireRelease(
        GetBackgroundColorMapError onError,
        (fun c -> SetBackgroundColorMapError(c, onError)),
        fun _ -> fio {
            do! SetBackgroundColorMapError(color, onError)
            return! action
        })

/// <summary>
/// Executes an action with temporary foreground and background colors.
/// </summary>
/// <param name="foreground">The foreground color to use.</param>
/// <param name="background">The background color to use.</param>
/// <param name="action">The action to execute.</param>
let WithColors (foreground: ConsoleColor, background: ConsoleColor, action: FIO<'R, exn>) : FIO<'R, exn> =
    fio {
        let! oldFg = GetForegroundColor
        let! oldBg = GetBackgroundColor
        do! SetForegroundColor foreground
        do! SetBackgroundColor background
        return! action.Ensuring(fio {
            do! SetForegroundColor oldFg
            do! SetBackgroundColor oldBg
        })
    }

/// <summary>
/// Executes an action with temporary foreground and background colors.
/// </summary>
/// <param name="foreground">The foreground color to use.</param>
/// <param name="background">The background color to use.</param>
/// <param name="action">The action to execute.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let WithColorsMapError<'R, 'E> (foreground: ConsoleColor, background: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    fio {
        let! oldFg = GetForegroundColorMapError onError
        let! oldBg = GetBackgroundColorMapError onError
        do! SetForegroundColorMapError(foreground, onError)
        do! SetBackgroundColorMapError(background, onError)
        return! action.Ensuring(fio {
            do! SetForegroundColorMapError(oldFg, onError)
            do! SetBackgroundColorMapError(oldBg, onError)
        })
    }
