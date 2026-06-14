namespace FIO.Console

open FIO.DSL

open System

[<RequireQualifiedAccess>]
module Console =

    let print<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintf Console.Out format) onError

    let printLine<'E> (format: Printf.TextWriterFormat<unit>) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> fprintfn Console.Out format) onError

    let readLine<'E> (onError: exn -> 'E) : FIO<string, 'E> =
        FIO.attempt (fun () -> Console.ReadLine()) onError

    let write<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Write text) onError

    let writeLine<'E> (text: string) (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.WriteLine text) onError

    let clear<'E> (onError: exn -> 'E) : FIO<unit, 'E> =
        FIO.attempt (fun () -> Console.Clear()) onError
