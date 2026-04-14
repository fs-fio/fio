/// Functional, effectful console I/O operations. All functions take an onError parameter to map exceptions to the error type.
[<RequireQualifiedAccess>]
module FIO.Console.Console

open FIO.DSL

open System
open System.Text

/// Gets the current console backend instance.
let inline private backend () = ConsoleBackend.get ()

/// Creates an FIO effect from a console backend operation.
let inline private eff (f: IConsoleBackend -> 'R) (onError: exn -> 'E) : FIO<'R, 'E> =
    FIO.attempt ((fun () -> f (backend ())), onError)

/// Prints a formatted message to stdout without a newline.
let print<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintf b.Out format) onError

/// Prints a formatted message to stderr without a newline.
let printError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintf b.Error format) onError

/// Prints a formatted message to stdout with a newline.
let printLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintfn b.Out format) onError

/// Prints a formatted message to stderr with a newline.
let printErrorLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> fprintfn b.Error format) onError

/// Reads a line from stdin.
let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> = eff (fun b -> b.ReadLine()) onError

/// Reads a single key from stdin.
/// <param name="intercept">Whether to hide the key from display.</param>
let readKey<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    eff (fun b -> b.ReadKey intercept) onError

/// Checks whether a key press is available in the input buffer.
let keyAvailable<'E> (onError: exn -> 'E) : FIO<bool, 'E> = eff (fun b -> b.KeyAvailable) onError

/// Reads a single character from stdin as an int.
let read<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.Read()) onError

/// Writes a message to stdout without a newline.
let write<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Write message) onError

/// Writes a message to stdout with a newline.
let writeLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.WriteLine message) onError

/// Writes a blank line to stdout.
let newLine<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.WriteBlankLine()) onError

/// Writes a message to stderr without a newline.
let writeError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.ErrorWrite message) onError

/// Writes a message to stderr with a newline.
let writeErrorLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.ErrorWriteLine message) onError

/// Clears the console screen.
let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Clear()) onError

/// Sets the cursor column position.
let setCursorLeft<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.CursorLeft <- left) onError

/// Sets the cursor row position.
let setCursorTop<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.CursorTop <- top) onError

/// Gets the cursor column position.
let getCursorLeft<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.CursorLeft) onError

/// Gets the cursor row position.
let getCursorTop<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.CursorTop) onError

/// Sounds the system bell.
let beep<'E> (onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Beep()) onError

/// Gets the console foreground color.
let getForegroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    eff (fun b -> b.ForegroundColor) onError

/// Sets the console foreground color.
let setForegroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.ForegroundColor <- color) onError

/// Gets the console background color.
let getBackgroundColor<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    eff (fun b -> b.BackgroundColor) onError

/// Sets the console background color.
let setBackgroundColor<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.BackgroundColor <- color) onError

/// Gets the console window title.
let getTitle<'E> (onError: exn -> 'E) : FIO<string, 'E> = eff (fun b -> b.Title) onError

/// Sets the console window title.
let setTitle<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.Title <- title) onError

/// Checks whether stdin is redirected.
let isInputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    eff (fun b -> b.IsInputRedirected) onError

/// Checks whether stdout is redirected.
let isOutputRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    eff (fun b -> b.IsOutputRedirected) onError

/// Checks whether stderr is redirected.
let isErrorRedirected<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    eff (fun b -> b.IsErrorRedirected) onError

/// Resets the console colors to their defaults.
let resetColor<'E> (onError: exn -> 'E) : FIO<unit, 'E> = eff (fun b -> b.ResetColor()) onError

/// Sets the cursor position.
let setCursorPosition<'E> (left: int, top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.SetCursorPosition(left, top)) onError

/// Gets the cursor position as (column, row).
let getCursorPosition<'E> (onError: exn -> 'E) : FIO<int * int, 'E> =
    eff (fun b -> b.GetCursorPosition()) onError

/// Gets whether the cursor is visible.
let getCursorVisible<'E> (onError: exn -> 'E) : FIO<bool, 'E> = eff (fun b -> b.CursorVisible) onError

/// Sets the cursor visibility.
/// <param name="visible">Whether the cursor should be visible.</param>
let setCursorVisible<'E> (visible: bool, onError: exn -> 'E) : FIO<unit, 'E> =
    eff (fun b -> b.CursorVisible <- visible) onError

/// Gets the console window width in columns.
let getWindowWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.WindowWidth) onError

/// Gets the console window height in rows.
let getWindowHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.WindowHeight) onError

/// Gets the console buffer width in columns.
let getBufferWidth<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.BufferWidth) onError

/// Gets the console buffer height in rows.
let getBufferHeight<'E> (onError: exn -> 'E) : FIO<int, 'E> = eff (fun b -> b.BufferHeight) onError

/// Writes multiple lines to stdout with buffering.
let printLines<'E> (lines: string seq, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt<unit, 'E> (
        (fun () ->
            let writer = backend().Out

            for line in lines do
                writer.WriteLine line

            writer.Flush()),
        onError
    )

/// Reads a password from stdin with masked input.
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

/// Runs an effect with a temporary foreground color, restoring the original afterwards.
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

/// Runs an effect with a temporary background color, restoring the original afterwards.
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

/// Runs an effect with temporary foreground and background colors, restoring the originals afterwards.
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
