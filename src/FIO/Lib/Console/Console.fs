/// <summary>
/// Provides functional, effectful console I/O operations for FIO.
/// </summary>
[<RequireQualifiedAccess>]
module FIO.Console.Console

open FIO.DSL

open System
open System.Text

let inline private backend() = ConsoleBackend.get()

/// <summary>
/// Prints a formatted message to stdout (no newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let print<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> fprintf (backend().Out) format), onError)

/// <summary>
/// Prints a formatted message to stdout (no newline).
/// </summary>
/// <param name="format">The format string.</param>
let printExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    print<exn>(format, id)

/// <summary>
/// Prints a formatted message to stderr (no newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let printError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> fprintf (backend().Error) format), onError)

/// <summary>
/// Prints a formatted message to stderr (no newline).
/// </summary>
/// <param name="format">The format string.</param>
let printErrorExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    printError<exn>(format, id)

/// <summary>
/// Prints a formatted message to stdout (with newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let printLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> fprintfn (backend().Out) format), onError)

/// <summary>
/// Prints a formatted message to stdout (with newline).
/// </summary>
/// <param name="format">The format string.</param>
let printLineExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    printLine<exn>(format, id)

/// <summary>
/// Prints a formatted message to stderr (with newline).
/// </summary>
/// <param name="format">The format string.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let printErrorLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> fprintfn (backend().Error) format), onError)

/// <summary>
/// Prints a formatted message to stderr (with newline).
/// </summary>
/// <param name="format">The format string.</param>
let printErrorLineExn (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    printErrorLine<exn>(format, id)

/// <summary>
/// Reads a line from stdin.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E>((fun () -> backend().ReadLine()), onError)

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
let readKey<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    FIO.attempt<ConsoleKeyInfo, 'E>((fun () -> backend().ReadKey intercept), onError)

/// <summary>
/// Reads a single key from stdin.
/// </summary>
/// <param name="intercept">Whether to hide the key from display.</param>
let readKeyExn (intercept: bool) : FIO<ConsoleKeyInfo, exn> =
    readKey<exn>(intercept, id)

/// <summary>
/// Gets a value indicating whether a key press is available in the input stream.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let keyAvailable<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> backend().KeyAvailable), onError)

/// <summary>
/// Gets a value indicating whether a key press is available in the input stream.
/// </summary>
let keyAvailableExn : FIO<bool, exn> =
    keyAvailable<exn> id

/// <summary>
/// Reads a single character from stdin as an int.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let read<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().Read()), onError)

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
let write<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().Write message), onError)

/// <summary>
/// Writes a message to stdout (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let writeExn (message: string) : FIO<unit, exn> =
    write<exn>(message, id)

/// <summary>
/// Writes a message to stdout (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let writeLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().WriteLine message), onError)

/// <summary>
/// Writes a message to stdout (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let writeLineExn (message: string) : FIO<unit, exn> =
    writeLine<exn>(message, id)

/// <summary>
/// Writes a blank line to stdout.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let newLine<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().WriteBlankLine()), onError)

/// <summary>
/// Writes a blank line to stdout.
/// </summary>
let newLineExn : FIO<unit, exn> =
    newLine<exn> id

/// <summary>
/// Writes a message to stderr (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let writeError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().ErrorWrite message), onError)

/// <summary>
/// Writes a message to stderr (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let writeErrorExn (message: string) : FIO<unit, exn> =
    writeError<exn>(message, id)

/// <summary>
/// Writes a message to stderr (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let writeErrorLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().ErrorWriteLine message), onError)

/// <summary>
/// Writes a message to stderr (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let writeErrorLineExn (message: string) : FIO<unit, exn> =
    writeErrorLine<exn>(message, id)

/// <summary>
/// Clears the console.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().Clear()), onError)

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
let setCursorLeft<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().CursorLeft <- left), onError)

/// <summary>
/// Sets the cursor column position.
/// </summary>
/// <param name="left">The column position.</param>
let setCursorLeftExn (left: int) : FIO<unit, exn> =
    setCursorLeft<exn>(left, id)

/// <summary>
/// Sets the cursor row position.
/// </summary>
/// <param name="top">The row position.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let setCursorTop<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().CursorTop <- top), onError)

/// <summary>
/// Sets the cursor row position.
/// </summary>
/// <param name="top">The row position.</param>
let setCursorTopExn (top: int) : FIO<unit, exn> =
    setCursorTop<exn>(top, id)

/// <summary>
/// Gets the cursor column position.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getCursorLeft<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().CursorLeft), onError)

/// <summary>
/// Gets the cursor column position.
/// </summary>
let getCursorLeftExn : FIO<int, exn> =
    getCursorLeft<exn> id

/// <summary>
/// Gets the cursor row position.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getCursorTop<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().CursorTop), onError)

/// <summary>
/// Gets the cursor row position.
/// </summary>
let getCursorTopExn : FIO<int, exn> =
    getCursorTop<exn> id

/// <summary>
/// Sounds the system bell.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let beep<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().Beep()), onError)

/// <summary>
/// Sounds the system bell.
/// </summary>
let beepExn : FIO<unit, exn> =
    beep<exn> id

/// <summary>
/// Gets the console foreground color.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getForegroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.attempt<ConsoleColor, 'E>((fun () -> backend().ForegroundColor), onError)

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
let setForegroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().ForegroundColor <- color), onError)

/// <summary>
/// Sets the console foreground color.
/// </summary>
/// <param name="color">The color to set.</param>
let setForegroundColorExn (color: ConsoleColor) : FIO<unit, exn> =
    setForegroundColor<exn>(color, id)

/// <summary>
/// Gets the console background color.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getBackgroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.attempt<ConsoleColor, 'E>((fun () -> backend().BackgroundColor), onError)

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
let setBackgroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().BackgroundColor <- color), onError)

/// <summary>
/// Sets the console background color.
/// </summary>
/// <param name="color">The color to set.</param>
let setBackgroundColorExn (color: ConsoleColor) : FIO<unit, exn> =
    setBackgroundColor<exn>(color, id)

/// <summary>
/// Gets the console window title.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getTitle<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E>((fun () -> backend().Title), onError)

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
let setTitle<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().Title <- title), onError)

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The title to set.</param>
let setTitleExn (title: string) : FIO<unit, exn> =
    setTitle<exn>(title, id)

/// <summary>
/// Checks whether stdin is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let isInputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> backend().IsInputRedirected), onError)

/// <summary>
/// Checks whether stdin is redirected.
/// </summary>
let isInputRedirectedExn : FIO<bool, exn> =
    isInputRedirected<exn> id

/// <summary>
/// Checks whether stdout is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let isOutputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> backend().IsOutputRedirected), onError)

/// <summary>
/// Checks whether stdout is redirected.
/// </summary>
let isOutputRedirectedExn : FIO<bool, exn> =
    isOutputRedirected<exn> id

/// <summary>
/// Checks whether stderr is redirected.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let isErrorRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> backend().IsErrorRedirected), onError)

/// <summary>
/// Checks whether stderr is redirected.
/// </summary>
let isErrorRedirectedExn : FIO<bool, exn> =
    isErrorRedirected<exn> id

/// <summary>
/// Resets the console colors to their defaults.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let resetColor<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().ResetColor()), onError)

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
let setCursorPosition<'E> (left: int, top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().SetCursorPosition(left, top)), onError)

/// <summary>
/// Sets the cursor position.
/// </summary>
/// <param name="left">The column position.</param>
/// <param name="top">The row position.</param>
let setCursorPositionExn (left: int, top: int) : FIO<unit, exn> =
    setCursorPosition<exn>(left, top, id)

/// <summary>
/// Gets the cursor position as (column, row).
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getCursorPosition<'E> (onError: exn -> 'E) : FIO<int * int, 'E> =
    FIO.attempt<int * int, 'E>((fun () -> backend().GetCursorPosition()), onError)

/// <summary>
/// Gets the cursor position as (column, row).
/// </summary>
let getCursorPositionExn : FIO<int * int, exn> =
    getCursorPosition<exn> id

/// <summary>
/// Gets whether the cursor is visible.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getCursorVisible<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.attempt<bool, 'E>((fun () -> backend().CursorVisible), onError)

/// <summary>
/// Gets whether the cursor is visible.
/// </summary>
let getCursorVisibleExn : FIO<bool, exn> =
    getCursorVisible<exn> id

/// <summary>
/// Sets the cursor visibility.
/// </summary>
/// <param name="visible">True to show the cursor, false to hide it.</param>
/// <param name="onError">Maps exceptions to the error type.</param>
let setCursorVisible<'E> (visible: bool, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () -> backend().CursorVisible <- visible), onError)

/// <summary>
/// Sets the cursor visibility.
/// </summary>
/// <param name="visible">True to show the cursor, false to hide it.</param>
let setCursorVisibleExn (visible: bool) : FIO<unit, exn> =
    setCursorVisible<exn>(visible, id)

/// <summary>
/// Gets the console window width.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getWindowWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().WindowWidth), onError)

/// <summary>
/// Gets the console window width.
/// </summary>
let getWindowWidthExn : FIO<int, exn> =
    getWindowWidth<exn> id

/// <summary>
/// Gets the console window height.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getWindowHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().WindowHeight), onError)

/// <summary>
/// Gets the console window height.
/// </summary>
let getWindowHeightExn : FIO<int, exn> =
    getWindowHeight<exn> id

/// <summary>
/// Gets the console buffer width.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getBufferWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().BufferWidth), onError)

/// <summary>
/// Gets the console buffer width.
/// </summary>
let getBufferWidthExn : FIO<int, exn> =
    getBufferWidth<exn> id

/// <summary>
/// Gets the console buffer height.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let getBufferHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.attempt<int, 'E>((fun () -> backend().BufferHeight), onError)

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
let printLines<'E> (lines: string seq, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E>((fun () ->
        let writer = backend().Out
        for line in lines do
            writer.WriteLine line
        writer.Flush()), onError)

/// <summary>
/// Writes multiple lines to stdout with buffering.
/// </summary>
/// <param name="lines">The lines to write.</param>
let printLinesExn (lines: string seq) : FIO<unit, exn> =
    printLines<exn>(lines, id)

/// <summary>
/// Reads a password from stdin with masked input.
/// </summary>
/// <param name="onError">Maps exceptions to the error type.</param>
let readPassword<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E>((fun () ->
        let b = backend()
        let password = StringBuilder()
        let mutable key = b.ReadKey true
        while key.Key <> ConsoleKey.Enter do
            if key.Key = ConsoleKey.Backspace && password.Length > 0 then
                password.Length <- password.Length - 1
                b.Write "\b \b"
            elif not (Char.IsControl key.KeyChar) then
                password.Append key.KeyChar |> ignore
                b.Write "*"
            key <- b.ReadKey true
        b.WriteBlankLine()
        password.ToString()), onError)

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
