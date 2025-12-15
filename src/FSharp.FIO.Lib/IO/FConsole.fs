(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional, effectful I/O operations for FIO, including console input/output and related utilities.
/// Includes the FConsole type for type-safe, composable, and effectful console operations.
/// </summary>

[<RequireQualifiedAccess>]
module FSharp.FIO.Lib.IO.FConsole

open FSharp.FIO.DSL

open System

/// <summary>
/// Prints a formatted message to the console output (no newline).
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="format">The format string to print.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that prints the formatted message or returns an error of type 'E.</returns>
let inline PrintMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> printf format) onError

/// <summary>
/// Prints a formatted message to the console output (no newline).
/// </summary>
/// <param name="format">The format string to print.</param>
/// <returns>An FIO effect that prints the formatted message or returns an error of type exn.</returns>
let inline Print (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintMapError<exn> (format, id)

/// <summary>
/// Prints a formatted message to the console output (with newline).
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="format">The format string to print.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that prints the formatted message with a newline or returns an error of type 'E.</returns>
let inline PrintLineMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> printfn format) onError

/// <summary>
/// Prints a formatted message to the console output (with newline).
/// </summary>
/// <param name="format">The format string to print.</param>
/// <returns>An FIO effect that prints the formatted message with a newline or returns an error of type 'E.</returns>
let inline PrintLine (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintLineMapError<exn> (format, id)

/// <summary>
/// Writes a line to the console output with a custom error handler.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="format">The format string to write.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that writes the formatted line or returns an error of type 'E.</returns>
let inline WriteLineMapError<'E> (format, onError: exn -> 'E) : FIO<string, 'E> =
    !<<< (fun () -> sprintf format) onError

/// <summary>
/// Writes a line to the console output.
/// </summary>
/// <param name="format">The format string to write.</param>
/// <returns>An FIO effect that writes the formatted line or returns an error of type exn.</returns>
let inline WriteLine format : FIO<string, exn> =
    WriteLineMapError<exn> (format, id)

/// <summary>
/// Reads a line from the console input with a custom error handler.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that reads a line or returns an error of type 'E.</returns>
let inline ReadLineMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    !<<< Console.ReadLine onError

/// <summary>
/// Reads a line from the console input.
/// </summary>
/// <returns>An FIO effect that reads a line or returns an error of type exn.</returns>
let inline ReadLine () : FIO<string, exn> =
    ReadLineMapError<exn> id

/// <summary>
/// Reads a single key from the console input.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="intercept">Whether to intercept the key so it is not displayed.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that reads a key or returns an error of type 'E.</returns>
let inline ReadKeyMapError<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    !<<< (fun () -> Console.ReadKey intercept) onError

/// <summary>
/// Reads a single key from the console input (no intercept).
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="intercept">Whether to intercept the key so it is not displayed.</param>
/// <returns>An FIO effect that reads a key or returns an error of type exn.</returns>
let inline ReadKey (intercept: bool) : FIO<ConsoleKeyInfo, exn> =
    ReadKeyMapError<exn> (intercept, id)

/// <summary>
/// Reads a single character from the console input as an int.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that reads a character or returns an error of type 'E.</returns>
let inline ReadMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    !<<< (fun () -> Console.Read()) onError

/// <summary>
/// Reads a single character from the console input as an int.
/// </summary>
/// <returns>An FIO effect that reads a character or returns an error of type exn.</returns>
let inline Read () : FIO<int, exn> =
    ReadMapError<exn> id

/// <summary>
/// Writes a message to the console output (no newline).
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="message">The message to write.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that writes the message or returns an error of type 'E.</returns>
let inline WriteMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.Write message) onError

/// <summary>
/// Writes a message to the console output (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <returns>An FIO effect that writes the message or returns an error of type exn.</returns>
let inline Write (message: string) : FIO<unit, exn> =
    WriteMapError<exn> (message, id)

/// <summary>
/// Writes a message to the error output stream (no newline).
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="message">The message to write.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that writes the message to the error output stream or returns an error of type 'E.</returns>
let inline WriteErrorMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.Error.Write message) onError

/// <summary>
/// Writes a message to the error output stream (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <returns>An FIO effect that writes the message to the error output stream or returns an error of type exn.</returns>
let inline WriteError (message: string) : FIO<unit, exn> =
    WriteErrorMapError<exn> (message, id)

/// <summary>
/// Writes a message to the error output stream (with newline).
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="message">The message to write.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that writes the message with a newline to the error output stream or returns an error of type 'E.</returns>
let inline WriteErrorLineMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.Error.WriteLine message) onError

/// <summary>
/// Writes a message to the error output stream (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <returns>An FIO effect that writes the message with a newline to the error output stream or returns an error of type exn.</returns>
let inline WriteErrorLine (message: string) : FIO<unit, exn> =
    WriteErrorLineMapError<exn> (message, id)

/// <summary>
/// Clears the console.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that clears the console or returns an error of type 'E.</returns>
let inline ClearMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.Clear()) onError

/// <summary>
/// Clears the console.
/// </summary>
/// <returns>An FIO effect that clears the console or returns an error of type exn.</returns>
let inline Clear () : FIO<unit, exn> =
    ClearMapError<exn> id

/// <summary>
/// Sets the cursor left position in the console window.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="left">The new cursor left position.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that sets the cursor left position or returns an error of type 'E.</returns>
let inline SetCursorLeftMapError<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.CursorLeft <- left) onError

/// <summary>
/// Sets the cursor left position in the console window.
/// </summary>
/// <param name="left">The new cursor left position.</param>
/// <returns>An FIO effect that sets the cursor left position or returns an error of type exn.</returns>
let inline SetCursorLeft (left: int) : FIO<unit, exn> =
    SetCursorLeftMapError<exn> (left, id)

/// <summary>
/// Sets the cursor top position in the console window.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="top">The new cursor top position.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that sets the cursor top position or returns an error of type 'E.</returns>
let inline SetCursorTopMapError<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.CursorTop <- top) onError

/// <summary>
/// Sets the cursor top position in the console window.
/// </summary>
/// <param name="top">The new cursor top position.</param>
/// <returns>An FIO effect that sets the cursor top position or returns an error of type exn.</returns>
let inline SetCursorTop (top: int) : FIO<unit, exn> =
    SetCursorTopMapError<exn> (top, id)

/// <summary>
/// Gets the cursor left position in the console window.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that gets the cursor left position or returns an error of type 'E.</returns>
let inline GetCursorLeftMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    !<<< (fun () -> Console.CursorLeft) onError

/// <summary>
/// Gets the cursor left position in the console window.
/// </summary>
/// <returns>An FIO effect that gets the cursor left position or returns an error of type exn.</returns>
let inline GetCursorLeft () : FIO<int, exn> =
    GetCursorLeftMapError<exn> id

/// <summary>
/// Gets the cursor top position in the console window.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that gets the cursor top position or returns an error of type 'E.</returns>
let inline GetCursorTopMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    !<<< (fun () -> Console.CursorTop) onError

/// <summary>
/// Gets the cursor top position in the console window.
/// </summary>
/// <returns>An FIO effect that gets the cursor top position or returns an error of type exn.</returns>
let inline GetCursorTop () : FIO<int, exn> =
    GetCursorTopMapError<exn> id

/// <summary>
/// Sounds the system bell.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that sounds the system bell or returns an error of type 'E.</returns>
let inline BeepMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.Beep()) onError

/// <summary>
/// Sounds the system bell.
/// </summary>
/// <returns>An FIO effect that sounds the system bell or returns an error of type exn.</returns>
let inline Beep () : FIO<unit, exn> =
    BeepMapError<exn> id

/// <summary>
/// Gets the foreground color of the console.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that gets the foreground color or returns an error of type 'E.</returns>
let inline GetForegroundColorMapError<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    !<<< (fun () -> Console.ForegroundColor) onError

/// <summary>
/// Gets the foreground color of the console.
/// </summary>
/// <returns>An FIO effect that gets the foreground color or returns an error of type exn.</returns>
let inline GetForegroundColor () : FIO<ConsoleColor, exn> =
    GetForegroundColorMapError<exn> id

/// <summary>
/// Sets the foreground color of the console.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="color">The new foreground color.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that sets the foreground color or returns an error of type 'E.</returns>
let inline SetForegroundColorMapError<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.ForegroundColor <- color) onError

/// <summary>
/// Sets the foreground color of the console.
/// </summary>
/// <param name="color">The new foreground color.</param>
/// <returns>An FIO effect that sets the foreground color or returns an error of type exn.</returns>
let inline SetForegroundColor (color: ConsoleColor) : FIO<unit, exn> =
    SetForegroundColorMapError<exn> (color, id)

/// <summary>
/// Gets the background color of the console.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that gets the background color or returns an error of type 'E.</returns>
let inline GetBackgroundColorMapError<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    !<<< (fun () -> Console.BackgroundColor) onError

/// <summary>
/// Gets the background color of the console.
/// </summary>
/// <returns>An FIO effect that gets the background color or returns an error of type exn.</returns>
let inline GetBackgroundColor () : FIO<ConsoleColor, exn> =
    GetBackgroundColorMapError<exn> id

/// <summary>
/// Sets the background color of the console.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="color">The new background color.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that sets the background color or returns an error of type 'E.</returns>
let inline SetBackgroundColorMapError<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.BackgroundColor <- color) onError

/// <summary>
/// Sets the background color of the console.
/// </summary>
/// <param name="color">The new background color.</param>
/// <returns>An FIO effect that sets the background color or returns an error of type exn.</returns>
let inline SetBackgroundColor (color: ConsoleColor) : FIO<unit, exn> =
    SetBackgroundColorMapError<exn> (color, id)

/// <summary>
/// Gets the console window title.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that gets the console window title or returns an error of type 'E.</returns>
let inline GetTitleMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    !<<< (fun () -> Console.Title) onError

/// <summary>
/// Gets the console window title.
/// </summary>
/// <returns>An FIO effect that gets the console window title or returns an error of type exn.</returns>
let inline GetTitle () : FIO<string, exn> =
    GetTitleMapError<exn> id

/// <summary>
/// Sets the console window title.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="title">The new title.</param>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that sets the console window title or returns an error of type 'E.</returns>
let inline SetTitleMapError<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> =
    !<<< (fun () -> Console.Title <- title) onError

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The new title.</param>
/// <returns>An FIO effect that sets the console window title or returns an error of type exn.</returns>
let inline SetTitle (title: string) : FIO<unit, exn> =
    SetTitleMapError<exn> (title, id)

/// <summary>
/// Returns whether input is redirected from the standard input stream.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that checks if input is redirected or returns an error of type 'E.</returns>
let inline IsInputRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    !<<< (fun () -> Console.IsInputRedirected) onError

/// <summary>
/// Returns whether input is redirected from the standard input stream.
/// </summary>
/// <returns>An FIO effect that checks if input is redirected or returns an error of type exn.</returns>
let inline IsInputRedirected () : FIO<bool, exn> =
    IsInputRedirectedMapError<exn> id

/// <summary>
/// Returns whether output is redirected from the standard output stream.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that checks if output is redirected or returns an error of type 'E.</returns>
let inline IsOutputRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    !<<< (fun () -> Console.IsOutputRedirected) onError

/// <summary>
/// Returns whether output is redirected from the standard output stream.
/// </summary>
/// <returns>An FIO effect that checks if output is redirected or returns an error of type exn.</returns>
let inline IsOutputRedirected () : FIO<bool, exn> =
    IsOutputRedirectedMapError<exn> id

/// <summary>
/// Returns whether error output is redirected from the standard error stream.
/// </summary>
/// <typeparam name="E">The error type.</typeparam>
/// <param name="onError">A function to map exceptions to the error type 'E.</param>
/// <returns>An FIO effect that checks if error output is redirected or returns an error of type 'E.</returns>
let inline IsErrorRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    !<<< (fun () -> Console.IsErrorRedirected) onError

/// <summary>
/// Returns whether error output is redirected from the standard error stream.
/// </summary>
/// <returns>An FIO effect that checks if error output is redirected or returns an error of type exn.</returns>
let inline IsErrorRedirected () : FIO<bool, exn> =
    IsErrorRedirectedMapError<exn> id
