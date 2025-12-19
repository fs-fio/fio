(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Internal utilities for type casting and conversion used across the FIO DSL.
/// </summary>
[<AutoOpen>]
module internal FSharp.FIO.DSL.Utilities

[<AutoOpen>]
module internal Casting =

    open System.Threading.Tasks

    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) =
        fun (exn: exn) -> onError exn :> obj

    let inline upcastFunc (func: unit -> 'R) : unit -> obj =
        fun () -> func () :> obj

    let inline upcastTask (genericTask: Task<'R>) : Task<obj> =
        task {
            let! res = genericTask
            return box res
        }
