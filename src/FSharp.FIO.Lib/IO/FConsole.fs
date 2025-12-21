(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional, effectful I/O operations for FIO, including console input/output and related utilities.
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.FIO.Lib.IO.FConsole

open FSharp.FIO.DSL

open System

/// <summary>
/// Prints a formatted message to the console output (no newline).
/// </summary>
/// <param name="format">The format string to print.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline PrintMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> printf format), onError)

/// <summary>
/// Prints a formatted message to the console output (no newline).
/// </summary>
/// <param name="format">The format string to print.</param>
let inline Print (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintMapError<exn> (format, id)

/// <summary>
/// Prints a formatted message to the console output (with newline).
/// </summary>
/// <param name="format">The format string to print.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline PrintLineMapError<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> printfn format), onError)

/// <summary>
/// Prints a formatted message to the console output (with newline).
/// </summary>
/// <param name="format">The format string to print.</param>
let inline PrintLine (format: Printf.TextWriterFormat<unit>) : FIO<unit, exn> =
    PrintLineMapError<exn> (format, id)

/// <summary>
/// Writes a line to the console output with a custom error handler.
/// </summary>
/// <param name="format">The format string to write.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline WriteLineMapError<'E> (format, onError: exn -> 'E) : FIO<string, 'E> =
    FIO.FromFunc<string, 'E> ((fun () -> sprintf format), onError)

/// <summary>
/// Writes a line to the console output.
/// </summary>
/// <param name="format">The format string to write.</param>
let inline WriteLine format : FIO<string, exn> =
    WriteLineMapError<exn> (format, id)

/// <summary>
/// Reads a line from the console input with a custom error handler.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline ReadLineMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.FromFunc<string, 'E> ((fun () -> Console.ReadLine ()), onError)

/// <summary>
/// Reads a line from the console input.
/// </summary>
let inline ReadLine () : FIO<string, exn> =
    ReadLineMapError<exn> id

/// <summary>
/// Reads a single key from the console input.
/// </summary>
/// <param name="intercept">Whether to intercept the key so it is not displayed.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline ReadKeyMapError<'E> (intercept: bool, onError: exn -> 'E) : FIO<ConsoleKeyInfo, 'E> =
    FIO.FromFunc<ConsoleKeyInfo, 'E> ((fun () -> Console.ReadKey intercept), onError)

/// <summary>
/// Reads a single key from the console input.
/// </summary>
/// <param name="intercept">Whether to intercept the key so it is not displayed.</param>
let inline ReadKey (intercept: bool) : FIO<ConsoleKeyInfo, exn> =
    ReadKeyMapError<exn> (intercept, id)

/// <summary>
/// Reads a single character from the console input as an int.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline ReadMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.FromFunc<int, 'E> ((fun () -> Console.Read ()), onError)

/// <summary>
/// Reads a single character from the console input as an int.
/// </summary>
let inline Read () : FIO<int, exn> =
    ReadMapError<exn> id

/// <summary>
/// Writes a message to the console output (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline WriteMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.Write message), onError)

/// <summary>
/// Writes a message to the console output (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline Write (message: string) : FIO<unit, exn> =
    WriteMapError<exn> (message, id)

/// <summary>
/// Writes a message to the error output stream (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline WriteErrorMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.Error.Write message), onError)

/// <summary>
/// Writes a message to the error output stream (no newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline WriteError (message: string) : FIO<unit, exn> =
    WriteErrorMapError<exn> (message, id)

/// <summary>
/// Writes a message to the error output stream (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline WriteErrorLineMapError<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.Error.WriteLine message), onError)

/// <summary>
/// Writes a message to the error output stream (with newline).
/// </summary>
/// <param name="message">The message to write.</param>
let inline WriteErrorLine (message: string) : FIO<unit, exn> =
    WriteErrorLineMapError<exn> (message, id)

/// <summary>
/// Clears the console.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline ClearMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.Clear ()), onError)

/// <summary>
/// Clears the console.
/// </summary>
let inline Clear () : FIO<unit, exn> =
    ClearMapError<exn> id

/// <summary>
/// Sets the cursor left position in the console window.
/// </summary>
/// <param name="left">The new cursor left position.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline SetCursorLeftMapError<'E> (left: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.CursorLeft <- left), onError)

/// <summary>
/// Sets the cursor left position in the console window.
/// </summary>
/// <param name="left">The new cursor left position.</param>
let inline SetCursorLeft (left: int) : FIO<unit, exn> =
    SetCursorLeftMapError<exn> (left, id)

/// <summary>
/// Sets the cursor top position in the console window.
/// </summary>
/// <param name="top">The new cursor top position.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline SetCursorTopMapError<'E> (top: int, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.CursorTop <- top), onError)

/// <summary>
/// Sets the cursor top position in the console window.
/// </summary>
/// <param name="top">The new cursor top position.</param>
let inline SetCursorTop (top: int) : FIO<unit, exn> =
    SetCursorTopMapError<exn> (top, id)

/// <summary>
/// Gets the cursor left position in the console window.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline GetCursorLeftMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.FromFunc<int, 'E> ((fun () -> Console.CursorLeft), onError)

/// <summary>
/// Gets the cursor left position in the console window.
/// </summary>
let inline GetCursorLeft () : FIO<int, exn> =
    GetCursorLeftMapError<exn> id

/// <summary>
/// Gets the cursor top position in the console window.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline GetCursorTopMapError<'E> (onError: exn -> 'E) : FIO<int, 'E> =
    FIO.FromFunc<int, 'E> ((fun () -> Console.CursorTop), onError)

/// <summary>
/// Gets the cursor top position in the console window.
/// </summary>
let inline GetCursorTop () : FIO<int, exn> =
    GetCursorTopMapError<exn> id

/// <summary>
/// Sounds the system bell.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline BeepMapError<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.Beep ()), onError)

/// <summary>
/// Sounds the system bell.
/// </summary>
let inline Beep () : FIO<unit, exn> =
    BeepMapError<exn> id

/// <summary>
/// Gets the foreground color of the console.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline GetForegroundColorMapError<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.FromFunc<ConsoleColor, 'E> ((fun () -> Console.ForegroundColor), onError)

/// <summary>
/// Gets the foreground color of the console.
/// </summary>
let inline GetForegroundColor () : FIO<ConsoleColor, exn> =
    GetForegroundColorMapError<exn> id

/// <summary>
/// Sets the foreground color of the console.
/// </summary>
/// <param name="color">The new foreground color.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline SetForegroundColorMapError<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.ForegroundColor <- color), onError)

/// <summary>
/// Sets the foreground color of the console.
/// </summary>
/// <param name="color">The new foreground color.</param>
let inline SetForegroundColor (color: ConsoleColor) : FIO<unit, exn> =
    SetForegroundColorMapError<exn> (color, id)

/// <summary>
/// Gets the background color of the console.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline GetBackgroundColorMapError<'E> (onError: exn -> 'E) : FIO<ConsoleColor, 'E> =
    FIO.FromFunc<ConsoleColor, 'E> ((fun () -> Console.BackgroundColor), onError)

/// <summary>
/// Gets the background color of the console.
/// </summary>
let inline GetBackgroundColor () : FIO<ConsoleColor, exn> =
    GetBackgroundColorMapError<exn> id

/// <summary>
/// Sets the background color of the console.
/// </summary>
/// <param name="color">The new background color.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline SetBackgroundColorMapError<'E> (color: ConsoleColor, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.BackgroundColor <- color), onError)

/// <summary>
/// Sets the background color of the console.
/// </summary>
/// <param name="color">The new background color.</param>
let inline SetBackgroundColor (color: ConsoleColor) : FIO<unit, exn> =
    SetBackgroundColorMapError<exn> (color, id)

/// <summary>
/// Gets the console window title.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline GetTitleMapError<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.FromFunc<string, 'E> ((fun () -> Console.Title), onError)

/// <summary>
/// Gets the console window title.
/// </summary>
let inline GetTitle () : FIO<string, exn> =
    GetTitleMapError<exn> id

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The new title.</param>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline SetTitleMapError<'E> (title: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.FromFunc<unit, 'E> ((fun () -> Console.Title <- title), onError)

/// <summary>
/// Sets the console window title.
/// </summary>
/// <param name="title">The new title.</param>
let inline SetTitle (title: string) : FIO<unit, exn> =
    SetTitleMapError<exn> (title, id)

/// <summary>
/// Checks whether input is redirected from the standard input stream.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline IsInputRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.FromFunc<bool, 'E> ((fun () -> Console.IsInputRedirected), onError)

/// <summary>
/// Checks whether input is redirected from the standard input stream.
/// </summary>
let inline IsInputRedirected () : FIO<bool, exn> =
    IsInputRedirectedMapError<exn> id

/// <summary>
/// Checks whether output is redirected from the standard output stream.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline IsOutputRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.FromFunc<bool, 'E> ((fun () -> Console.IsOutputRedirected), onError)

/// <summary>
/// Checks whether output is redirected from the standard output stream.
/// </summary>
let inline IsOutputRedirected () : FIO<bool, exn> =
    IsOutputRedirectedMapError<exn> id

/// <summary>
/// Checks whether error output is redirected from the standard error stream.
/// </summary>
/// <param name="onError">A function to map exceptions to the error type.</param>
let inline IsErrorRedirectedMapError<'E> (onError: exn -> 'E) : FIO<bool, 'E> =
    FIO.FromFunc<bool, 'E> ((fun () -> Console.IsErrorRedirected), onError)

/// <summary>
/// Checks whether error output is redirected from the standard error stream.
/// </summary>
let inline IsErrorRedirected () : FIO<bool, exn> =
    IsErrorRedirectedMapError<exn> id
