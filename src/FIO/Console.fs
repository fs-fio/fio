namespace FIO.Console

open FIO.DSL

open System

[<RequireQualifiedAccess>]
module Console =

    /// Returns an effect that writes formatted text to standard output.
    let print<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintf Console.Out format) onError

    /// Returns an effect that writes formatted text followed by a newline to standard output.
    let printLine<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintfn Console.Out format) onError

    /// Returns an effect that reads a line from standard input.
    let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
        FIO.attempt (fun () -> Console.ReadLine()) onError

    /// Returns an effect that writes text to standard output.
    let write<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Write text) onError

    /// Returns an effect that writes text followed by a newline to standard output.
    let writeLine<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.WriteLine text) onError

    /// Returns an effect that clears the console.
    let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Clear()) onError
