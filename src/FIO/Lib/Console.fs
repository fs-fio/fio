/// <summary>Provides functional, effectful console I/O operations; every function takes an <c>onError</c> mapping for exception-to-typed-error conversion.</summary>
[<RequireQualifiedAccess>]
module FIO.Console.Console

open FIO.DSL

open System

/// <summary>Creates an effect that prints a formatted message to standard output without a trailing newline.</summary>
/// <param name="format">The format string to render.</param>
/// <param name="onError">A function that maps an exception thrown during printing to the typed error.</param>
/// <returns>An effect that completes with unit once the message has been written.</returns>
let print<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt ((fun () -> fprintf Console.Out format), onError)

/// <summary>Creates an effect that prints a formatted message to standard output followed by a newline.</summary>
/// <param name="format">The format string to render.</param>
/// <param name="onError">A function that maps an exception thrown during printing to the typed error.</param>
/// <returns>An effect that completes with unit once the line has been written.</returns>
let printLine<'E> (format: Printf.TextWriterFormat<unit>, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt ((fun () -> fprintfn Console.Out format), onError)

/// <summary>Creates an effect that reads a single line from standard input.</summary>
/// <param name="onError">A function that maps an exception thrown during reading to the typed error.</param>
/// <returns>An effect that completes with the line read from standard input.</returns>
let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
    FIO.attempt ((fun () -> Console.ReadLine()), onError)

/// <summary>Creates an effect that writes a string to standard output without a trailing newline.</summary>
/// <param name="message">The text to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the message has been written.</returns>
let write<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt ((fun () -> Console.Write message), onError)

/// <summary>Creates an effect that writes a string to standard output followed by a newline.</summary>
/// <param name="message">The text to write.</param>
/// <param name="onError">A function that maps an exception thrown during writing to the typed error.</param>
/// <returns>An effect that completes with unit once the line has been written.</returns>
let writeLine<'E> (message: string, onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt ((fun () -> Console.WriteLine message), onError)

/// <summary>Creates an effect that clears the console screen.</summary>
/// <param name="onError">A function that maps an exception thrown during the clear to the typed error.</param>
/// <returns>An effect that completes with unit once the screen has been cleared.</returns>
let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
    FIO.attempt ((fun () -> Console.Clear()), onError)
