/// <summary>
/// Provides functional, effectful console I/O operations for FIO.
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.FIO.Console.Console

open FSharp.FIO.DSL

open System

/// <summary>
/// Prints a formatted message to stdout (no newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline print<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> printf format), onError)

/// <summary>
/// Prints a formatted message to stdout (no newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline printExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    print<exn>(format, id)

/// <summary>
/// Prints a formatted message to stderr (no newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline printError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> eprintf format), onError)

/// <summary>
/// Prints a formatted message to stderr (no newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline printErrorExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    printError<exn>(format, id)

/// <summary>
/// Prints a formatted message to stdout (with newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline printLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> printfn format), onError)

/// <summary>
/// Prints a formatted message to stdout (with newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline printLineExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    printLine<exn>(format, id)

/// <summary>
/// Prints a formatted message to stderr (with newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline printErrorLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> eprintfn format), onError)

/// <summary>
/// Prints a formatted message to stderr (with newline).
/// </summary>
/// <param name="format">The format string.</param>
let inline printErrorLineExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    printErrorLine<exn>(format, id)

/// <summary>
/// Reads a line from stdin.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E>((fun () -> Console.ReadLine()), onError)

/// <summary>
/// Reads a line from stdin.
/// </summary>
let readLineExn : FIO<string, exn> =
    readLine<exn> id

/// <summary>
/// Reads a single key from stdin.
/// </summary>
/// <param name="intercept">Whether to hide the key from display.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline readKey<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    FIO.attempt<ConsoleKeyInfo, 'E>((fun () -> Console.ReadKey intercept), onError)

/// <summary>
/// Reads a single key from stdin.
/// </summary>
/// <param name="intercept">Whether to hide the key from display.</param>
let inline readKeyExn (intercept: bool) : FIO<ConsoleKeyInfo, exn> =
    readKey<exn>(intercept, id)

/// <summary>
/// Reads a single character from stdin as an int.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline read<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.Read()), onError)

/// <summary>
/// Reads a single character from stdin as an int.
/// </summary>
let readExn : FIO<int, exn> =
    read<exn> id

/// <summary>
/// Writes a message to stdout (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline write<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.Write message), onError)

/// <summary>
/// Writes a message to stdout (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline writeExn (message: string) : FIO<unit, exn> =
    write<exn>(message, id)

/// <summary>
/// Writes a message to stdout (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline writeLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.WriteLine message), onError)

/// <summary>
/// Writes a message to stdout (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline writeLineExn (message: string) : FIO<unit, exn> =
    writeLine<exn>(message, id)

/// <summary>
/// Writes a message to stderr (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline writeError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.Error.Write message), onError)

/// <summary>
/// Writes a message to stderr (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline writeErrorExn (message: string) : FIO<unit, exn> =
    writeError<exn>(message, id)

/// <summary>
/// Writes a message to stderr (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline writeErrorLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.Error.WriteLine message), onError)

/// <summary>
/// Writes a message to stderr (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline writeErrorLineExn (message: string) : FIO<unit, exn> =
    writeErrorLine<exn>(message, id)

/// <summary>
/// Clears the console.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.Clear()), onError)

/// <summary>
/// Clears the console.
/// </summary>
let clearExn : FIO<unit, exn> =
    clear<exn> id

/// <summary>
/// Sets the cursor column position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline setCursorLeft<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.CursorLeft <- left), onError)

/// <summary>
/// Sets the cursor column position.
/// </summary>
/// <param name="left">The column position.</param>
let inline setCursorLeftExn (left: int) : FIO<unit, exn> =
    setCursorLeft<exn>(left, id)

/// <summary>
/// Sets the cursor row position.
/// </summary>
/// <param name="top">The row position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline setCursorTop<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.CursorTop <- top), onError)

/// <summary>
/// Sets the cursor row position.
/// </summary>
/// <param name="top">The row position.</param>
let inline setCursorTopExn (top: int) : FIO<unit, exn> =
    setCursorTop<exn>(top, id)

/// <summary>
/// Gets the cursor column position.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getCursorLeft<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.CursorLeft), onError)

/// <summary>
/// Gets the cursor column position.
/// </summary>
let getCursorLeftExn : FIO<int, exn> =
    getCursorLeft<exn> id

/// <summary>
/// Gets the cursor row position.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getCursorTop<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.CursorTop), onError)

/// <summary>
/// Gets the cursor row position.
/// </summary>
let getCursorTopExn : FIO<int, exn> =
    getCursorTop<exn> id

/// <summary>
/// Sounds the system bell.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline beep<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.Beep()), onError)

/// <summary>
/// Sounds the system bell.
/// </summary>
let beepExn : FIO<unit, exn> =
    beep<exn> id

/// <summary>
/// Gets the console foreground color.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getForegroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.attempt<ConsoleColor, 'E>((fun () -> Console.ForegroundColor), onError)

/// <summary>
/// Gets the console foreground color.
/// </summary>
let getForegroundColorExn : FIO<ConsoleColor, exn> =
    getForegroundColor<exn> id

/// <summary>
/// Sets the console foreground color.
/// </summary>
/// <param name="color">The color to set.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline setForegroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.ForegroundColor <- color), onError)

/// <summary>
/// Sets the console foreground color.
/// </summary>
/// <param name="color">The color to set.</param>
let inline setForegroundColorExn (color: ConsoleColor) : FIO<unit, exn> =
    setForegroundColor<exn>(color, id)

/// <summary>
/// Gets the console background color.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getBackgroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.attempt<ConsoleColor, 'E>((fun () -> Console.BackgroundColor), onError)

/// <summary>
/// Gets the console background color.
/// </summary>
let getBackgroundColorExn : FIO<ConsoleColor, exn> =
    getBackgroundColor<exn> id

/// <summary>
/// Sets the console background color.
/// </summary>
/// <param name="color">The color to set.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline setBackgroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.BackgroundColor <- color), onError)

/// <summary>
/// Sets the console background color.
/// </summary>
/// <param name="color">The color to set.</param>
let inline setBackgroundColorExn (color: ConsoleColor) : FIO<unit, exn> =
    setBackgroundColor<exn>(color, id)

/// <summary>
/// Gets the console window title.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getTitle<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E>((fun () -> Console.Title), onError)

/// <summary>
/// Gets the console window title.
/// </summary>
let getTitleExn : FIO<string, exn> =
    getTitle<exn> id

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The title to set.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline setTitle<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.Title <- title), onError)

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The title to set.</param>
let inline setTitleExn (title: string) : FIO<unit, exn> =
    setTitle<exn>(title, id)

/// <summary>
/// Checks whether stdin is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline isInputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> Console.IsInputRedirected), onError)

/// <summary>
/// Checks whether stdin is redirected.
/// </summary>
let isInputRedirectedExn : FIO<bool, exn> =
    isInputRedirected<exn> id

/// <summary>
/// Checks whether stdout is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline isOutputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> Console.IsOutputRedirected), onError)

/// <summary>
/// Checks whether stdout is redirected.
/// </summary>
let isOutputRedirectedExn : FIO<bool, exn> =
    isOutputRedirected<exn> id

/// <summary>
/// Checks whether stderr is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline isErrorRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> Console.IsErrorRedirected), onError)

/// <summary>
/// Checks whether stderr is redirected.
/// </summary>
let isErrorRedirectedExn : FIO<bool, exn> =
    isErrorRedirected<exn> id

/// <summary>
/// Resets the console colors to their defaults.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline resetColor<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.ResetColor()), onError)

/// <summary>
/// Resets the console colors to their defaults.
/// </summary>
let resetColorExn : FIO<unit, exn> =
    resetColor<exn> id

/// <summary>
/// Sets the cursor position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="top">The row position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline setCursorPosition<'E> (left: int, top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> Console.SetCursorPosition(left, top)), onError)

/// <summary>
/// Sets the cursor position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="top">The row position.</param>
let inline setCursorPositionExn (left: int, top: int) : FIO<unit, exn> =
    setCursorPosition<exn>(left, top, id)

/// <summary>
/// Gets the cursor position as (column, row).
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getCursorPosition<'E> (onError: exn -> 'E) : FIO<int * int, 'E> =
    FIO.attempt<int * int, 'E>((fun () -> Console.CursorLeft, Console.CursorTop), onError)

/// <summary>
/// Gets the cursor position as (column, row).
/// </summary>
let getCursorPositionExn : FIO<int * int, exn> =
    getCursorPosition<exn> id

/// <summary>
/// Gets the console window width.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getWindowWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.WindowWidth), onError)

/// <summary>
/// Gets the console window width.
/// </summary>
let getWindowWidthExn : FIO<int, exn> =
    getWindowWidth<exn> id

/// <summary>
/// Gets the console window height.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getWindowHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.WindowHeight), onError)

/// <summary>
/// Gets the console window height.
/// </summary>
let getWindowHeightExn : FIO<int, exn> =
    getWindowHeight<exn> id

/// <summary>
/// Gets the console buffer width.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getBufferWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.BufferWidth), onError)

/// <summary>
/// Gets the console buffer width.
/// </summary>
let getBufferWidthExn : FIO<int, exn> =
    getBufferWidth<exn> id

/// <summary>
/// Gets the console buffer height.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline getBufferHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> Console.BufferHeight), onError)

/// <summary>
/// Gets the console buffer height.
/// </summary>
let getBufferHeightExn : FIO<int, exn> =
    getBufferHeight<exn> id

/// <summary>
/// Writes multiple lines to stdout with buffering.
/// </summary>
/// <param name="lines">The lines to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline printLines<'E> (lines: string seq, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () ->
        use writer = Console.Out
        for line in lines do
            writer.WriteLine line
        writer.Flush()), onError)

/// <summary>
/// Writes multiple lines to stdout with buffering.
/// </summary>
/// <param name="lines">The lines to write.</param>
let inline printLinesExn (lines: string seq) : FIO<unit, exn> =
    printLines<exn>(lines, id)

/// <summary>
/// Reads a password from stdin with masked input.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let inline readPassword<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E>((fun () ->
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
let readPasswordExn : FIO<string, exn> =
    readPassword<exn> id

/// <summary>
/// Executes an action with a temporary foreground color.
/// </summary>
/// <param name="color">The foreground color to use.</param>
/// <param name="action">The action to execute.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let withForegroundColor<'R, 'E> (color: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.acquireRelease(
        getForegroundColor onError,
        (fun c -> setForegroundColor(c, onError)),
        fun _ -> fio {
            do! setForegroundColor(color, onError)
            return! action
        })

/// <summary>
/// Executes an action with a temporary foreground color.
/// </summary>
/// <param name="color">The foreground color to use.</param>
/// <param name="action">The action to execute.</param>
let withForegroundColorExn<'R> (color: ConsoleColor, action: FIO<'R, exn>) : FIO<'R, exn> =
    withForegroundColor<'R, exn>(color, action, id)

/// <summary>
/// Executes an action with a temporary background color.
/// </summary>
/// <param name="color">The background color to use.</param>
/// <param name="action">The action to execute.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let withBackgroundColor<'R, 'E> (color: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.acquireRelease(
        getBackgroundColor onError,
        (fun c -> setBackgroundColor(c, onError)),
        fun _ -> fio {
            do! setBackgroundColor(color, onError)
            return! action
        })

/// <summary>
/// Executes an action with a temporary background color.
/// </summary>
/// <param name="color">The background color to use.</param>
/// <param name="action">The action to execute.</param>
let withBackgroundColorExn<'R> (color: ConsoleColor, action: FIO<'R, exn>) : FIO<'R, exn> =
    withBackgroundColor<'R, exn>(color, action, id)

/// <summary>
/// Executes an action with temporary foreground and background colors.
/// </summary>
/// <param name="foreground">The foreground color to use.</param>
/// <param name="background">The background color to use.</param>
/// <param name="action">The action to execute.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let withColors<'R, 'E> (foreground: ConsoleColor, background: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    fio {
        let! oldFg = getForegroundColor onError
        let! oldBg = getBackgroundColor onError
        do! setForegroundColor(foreground, onError)
        do! setBackgroundColor(background, onError)
        return! action.Ensuring(fio {
            do! setForegroundColor(oldFg, onError)
            do! setBackgroundColor(oldBg, onError)
        })
    }

/// <summary>
/// Executes an action with temporary foreground and background colors.
/// </summary>
/// <param name="foreground">The foreground color to use.</param>
/// <param name="background">The background color to use.</param>
/// <param name="action">The action to execute.</param>
let withColorsExn<'R> (foreground: ConsoleColor, background: ConsoleColor, action: FIO<'R, exn>) : FIO<'R, exn> =
    withColors<'R, exn>(foreground, background, action, id)
