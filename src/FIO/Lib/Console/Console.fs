/// <summary>Provides functional, effectful console I/O operations; every function takes an <c>onError</c> mapping for exception-to-typed-error conversion.</summary>
[<RequireQualifiedAccess>]
module FIO.Console.Console

open FIO.DSL

open System
open System.Text

/// <summary>Returns the currently active console backend instance.</summary>
/// <returns>The <c>IConsoleBackend</c> in use by this module.</returns>
let inline private backend () = ConsoleBackend.get ()

/// <summary>Builds an effect that applies a function to the active console backend.</summary>
/// <param name="f">A function that receives the backend and produces a result.</param>
/// <param name="onError">A function that maps an exception thrown during execution to the typed error.</param>
/// <returns>An effect that completes with the value returned by <paramref name="f"/>.</returns>
let inline private eff (f: IConsoleBackend -> 'R) (onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.attempt ((fun () -> f (backend ())), onError)

/// <summary>Creates an effect that prints a formatted message to standard output without a trailing newline.</summary>
/// <param name="format">The format string to render.</param>
/// <param name="onError">A function that maps an exception thrown during printing to the typed error.</param>
/// <returns>An effect that completes with unit once the message has been written.</returns>
let print<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintf b.Out format) onError

/// <summary>Creates an effect that prints a formatted message to standard error without a trailing newline.</summary>
/// <param name="format">The format string to render.</param>
/// <param name="onError">A function that maps an exception thrown during printing to the typed error.</param>
/// <returns>An effect that completes with unit once the message has been written.</returns>
let printError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintf b.Error format) onError

/// <summary>Creates an effect that prints a formatted message to standard output followed by a newline.</summary>
/// <param name="format">The format string to render.</param>
/// <param name="onError">A function that maps an exception thrown during printing to the typed error.</param>
/// <returns>An effect that completes with unit once the line has been written.</returns>
let printLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintfn b.Out format) onError

/// <summary>Creates an effect that prints a formatted message to standard error followed by a newline.</summary>
/// <param name="format">The format string to render.</param>
/// <param name="onError">A function that maps an exception thrown during printing to the typed error.</param>
/// <returns>An effect that completes with unit once the line has been written.</returns>
let printErrorLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintfn b.Error format) onError

/// <summary>Creates an effect that reads a single line from standard input.</summary>
/// <param name="onError">A function that maps an exception thrown during reading to the typed error.</param>
/// <returns>An effect that completes with the line read from standard input.</returns>
let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> = eff (fun b -> b.ReadLine()) onError

/// <summary>Creates an effect that reads a single key press from the console.</summary>
/// <param name="intercept">When <c>true</c>, the key is not echoed to the display.</param>
/// <param name="onError">A function that maps an exception thrown during reading to the typed error.</param>
/// <returns>An effect that completes with the key press information.</returns>
let readKey<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    eff (fun b -> b.ReadKey intercept) onError

/// <summary>Returns whether a key press is currently available in the console input buffer.</summary>
/// <param name="onError">A function that maps an exception thrown during the check to the typed error.</param>
/// <returns>An effect that completes with <c>true</c> when a key press is buffered; <c>false</c> otherwise.</returns>
let keyAvailable<'E> (onError: exn -> 'E) : FIO<bool, 'E> = eff (fun b -> b.KeyAvailable) onError

/// <summary>Creates an effect that reads a single character from standard input as an integer code.</summary>
/// <param name="onError">A function that maps an exception thrown during reading to the typed error.</param>
/// <returns>An effect that completes with the character code, or <c>-1</c> when no more characters are available.</returns>
let read<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.Read()) onError

/// <summary>Creates an effect that writes a string to standard output without a trailing newline.</summary>
/// <param name="message">The text to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the message has been written.</returns>
let write<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Write message) onError

/// <summary>Creates an effect that writes a string to standard output followed by a newline.</summary>
/// <param name="message">The text to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the line has been written.</returns>
let writeLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.WriteLine message) onError

/// <summary>Creates an effect that writes a blank line to standard output.</summary>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the blank line has been written.</returns>
let newLine<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.WriteBlankLine()) onError

/// <summary>Creates an effect that writes a string to standard error without a trailing newline.</summary>
/// <param name="message">The text to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the message has been written.</returns>
let writeError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.ErrorWrite message) onError

/// <summary>Creates an effect that writes a string to standard error followed by a newline.</summary>
/// <param name="message">The text to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the line has been written.</returns>
let writeErrorLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.ErrorWriteLine message) onError

/// <summary>Creates an effect that clears the console screen.</summary>
/// <param name="onError">A function that maps an exception thrown during the clear to the typed error.</param>
/// <returns>An effect that completes with unit once the screen has been cleared.</returns>
let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Clear()) onError

/// <summary>Creates an effect that sets the cursor's column position.</summary>
/// <param name="left">The column index to move the cursor to.</param>
/// <param name="onError">A function that maps an exception thrown during the move to the typed error.</param>
/// <returns>An effect that completes with unit once the cursor has been moved.</returns>
let setCursorLeft<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.CursorLeft <- left) onError

/// <summary>Creates an effect that sets the cursor's row position.</summary>
/// <param name="top">The row index to move the cursor to.</param>
/// <param name="onError">A function that maps an exception thrown during the move to the typed error.</param>
/// <returns>An effect that completes with unit once the cursor has been moved.</returns>
let setCursorTop<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.CursorTop <- top) onError

/// <summary>Returns the current cursor column position.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the cursor column index.</returns>
let getCursorLeft<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.CursorLeft) onError

/// <summary>Returns the current cursor row position.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the cursor row index.</returns>
let getCursorTop<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.CursorTop) onError

/// <summary>Creates an effect that emits a system bell tone.</summary>
/// <param name="onError">A function that maps an exception thrown during the beep to the typed error.</param>
/// <returns>An effect that completes with unit after the beep has been emitted.</returns>
let beep<'E> (onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Beep()) onError

/// <summary>Returns the current console foreground color.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the active foreground color.</returns>
let getForegroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    eff (fun b -> b.ForegroundColor) onError

/// <summary>Creates an effect that sets the console foreground color.</summary>
/// <param name="color">The color to apply to subsequent output.</param>
/// <param name="onError">A function that maps an exception thrown during the set to the typed error.</param>
/// <returns>An effect that completes with unit once the color has been applied.</returns>
let setForegroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.ForegroundColor <- color) onError

/// <summary>Returns the current console background color.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the active background color.</returns>
let getBackgroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    eff (fun b -> b.BackgroundColor) onError

/// <summary>Creates an effect that sets the console background color.</summary>
/// <param name="color">The color to apply to subsequent output.</param>
/// <param name="onError">A function that maps an exception thrown during the set to the typed error.</param>
/// <returns>An effect that completes with unit once the color has been applied.</returns>
let setBackgroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.BackgroundColor <- color) onError

/// <summary>Returns the current console window title.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the active window title.</returns>
let getTitle<'E> (onError: exn -> 'E) : FIO<string, 'E> = eff (fun b -> b.Title) onError

/// <summary>Creates an effect that sets the console window title.</summary>
/// <param name="title">The new title text.</param>
/// <param name="onError">A function that maps an exception thrown during the set to the typed error.</param>
/// <returns>An effect that completes with unit once the title has been applied.</returns>
let setTitle<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Title <- title) onError

/// <summary>Returns whether standard input has been redirected from a file or pipe.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with <c>true</c> when standard input is redirected; <c>false</c> otherwise.</returns>
let isInputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    eff (fun b -> b.IsInputRedirected) onError

/// <summary>Returns whether standard output has been redirected from a file or pipe.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with <c>true</c> when standard output is redirected; <c>false</c> otherwise.</returns>
let isOutputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    eff (fun b -> b.IsOutputRedirected) onError

/// <summary>Returns whether standard error has been redirected from a file or pipe.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with <c>true</c> when standard error is redirected; <c>false</c> otherwise.</returns>
let isErrorRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    eff (fun b -> b.IsErrorRedirected) onError

/// <summary>Creates an effect that resets the console foreground and background colors to their defaults.</summary>
/// <param name="onError">A function that maps an exception thrown during the reset to the typed error.</param>
/// <returns>An effect that completes with unit once the colors have been reset.</returns>
let resetColor<'E> (onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.ResetColor()) onError

/// <summary>Creates an effect that sets the cursor to the specified column and row.</summary>
/// <param name="left">The column index.</param>
/// <param name="top">The row index.</param>
/// <param name="onError">A function that maps an exception thrown during the move to the typed error.</param>
/// <returns>An effect that completes with unit once the cursor has been positioned.</returns>
let setCursorPosition<'E> (left: int, top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.SetCursorPosition(left, top)) onError

/// <summary>Returns the current cursor position as a column-row pair.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the cursor position as <c>(left, top)</c>.</returns>
let getCursorPosition<'E> (onError: exn -> 'E) : FIO<int * int, 'E> =
    eff (fun b -> b.GetCursorPosition()) onError

/// <summary>Returns whether the console cursor is currently visible.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with <c>true</c> when the cursor is visible; <c>false</c> otherwise.</returns>
let getCursorVisible<'E> (onError: exn -> 'E) : FIO<bool, 'E> = eff (fun b -> b.CursorVisible) onError

/// <summary>Creates an effect that sets the console cursor's visibility.</summary>
/// <param name="visible">Whether the cursor should be visible.</param>
/// <param name="onError">A function that maps an exception thrown during the set to the typed error.</param>
/// <returns>An effect that completes with unit once visibility has been applied.</returns>
let setCursorVisible<'E> (visible: bool, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.CursorVisible <- visible) onError

/// <summary>Returns the console window width in columns.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the window width in columns.</returns>
let getWindowWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.WindowWidth) onError

/// <summary>Returns the console window height in rows.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the window height in rows.</returns>
let getWindowHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.WindowHeight) onError

/// <summary>Returns the console buffer width in columns.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the buffer width in columns.</returns>
let getBufferWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.BufferWidth) onError

/// <summary>Returns the console buffer height in rows.</summary>
/// <param name="onError">A function that maps an exception thrown during the read to the typed error.</param>
/// <returns>An effect that completes with the buffer height in rows.</returns>
let getBufferHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.BufferHeight) onError

/// <summary>Creates an effect that writes multiple lines to standard output and flushes once at the end.</summary>
/// <param name="lines">The sequence of lines to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once every line has been written and the writer is flushed.</returns>
let printLines<'E> (lines: string seq, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E> (
        (fun () ->
            let writer = backend().Out

            for line in lines do
                writer.WriteLine line

            writer.Flush()),
        onError
    )

/// <summary>Creates an effect that reads a password from standard input with masked input.</summary>
/// <param name="onError">A function that maps an exception thrown during reading to the typed error.</param>
/// <returns>An effect that completes with the entered password as a string.</returns>
/// <remarks>Each typed character is echoed as <c>*</c>; backspace deletes the most recent character.</remarks>
let readPassword<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt<string, 'E> (
        (fun () ->
            let b = backend ()
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
            password.ToString()),
        onError
    )

/// <summary>Builds a resource-managed effect that runs an action under a temporary foreground color and restores the original afterwards.</summary>
/// <typeparam name="'R">The success result type produced by <paramref name="action"/>.</typeparam>
/// <param name="color">The temporary foreground color.</param>
/// <param name="action">The effect to run while the temporary color is active.</param>
/// <param name="onError">A function that maps an exception thrown during get, set, or restore to the typed error.</param>
/// <returns>An effect that completes with <paramref name="action"/>'s result and always restores the original foreground color afterwards.</returns>
let withForegroundColor<'R, 'E> (color: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.acquireRelease (
        getForegroundColor onError,
        (fun c -> setForegroundColor (c, onError)),
        fun _ ->
            fio {
                do! setForegroundColor (color, onError)
                return! action
            }
    )

/// <summary>Builds a resource-managed effect that runs an action under a temporary background color and restores the original afterwards.</summary>
/// <typeparam name="'R">The success result type produced by <paramref name="action"/>.</typeparam>
/// <param name="color">The temporary background color.</param>
/// <param name="action">The effect to run while the temporary color is active.</param>
/// <param name="onError">A function that maps an exception thrown during get, set, or restore to the typed error.</param>
/// <returns>An effect that completes with <paramref name="action"/>'s result and always restores the original background color afterwards.</returns>
let withBackgroundColor<'R, 'E> (color: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.acquireRelease (
        getBackgroundColor onError,
        (fun c -> setBackgroundColor (c, onError)),
        fun _ ->
            fio {
                do! setBackgroundColor (color, onError)
                return! action
            }
    )

/// <summary>Builds a resource-managed effect that runs an action under temporary foreground and background colors and restores both afterwards.</summary>
/// <typeparam name="'R">The success result type produced by <paramref name="action"/>.</typeparam>
/// <param name="foreground">The temporary foreground color.</param>
/// <param name="background">The temporary background color.</param>
/// <param name="action">The effect to run while the temporary colors are active.</param>
/// <param name="onError">A function that maps an exception thrown during get, set, or restore to the typed error.</param>
/// <returns>An effect that completes with <paramref name="action"/>'s result and always restores both colors afterwards.</returns>
let withColors<'R, 'E>
    (foreground: ConsoleColor, background: ConsoleColor, action: FIO<'R, 'E>, onError: exn -> 'E)
    : FIO<'R, 'E> =
    fio {
        let! oldFg = getForegroundColor onError
        let! oldBg = getBackgroundColor onError
        do! setForegroundColor (foreground, onError)
        do! setBackgroundColor (background, onError)

        return!
            action.Ensuring(
                fio {
                    do! setForegroundColor (oldFg, onError)
                    do! setBackgroundColor (oldBg, onError)
                }
            )
    }
