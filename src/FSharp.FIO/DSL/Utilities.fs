/// <summary>
/// Internal utilities for type casting and conversion used across the FIO DSL.
/// </summary>
[<AutoOpen>]
module private FSharp.FIO.DSL.Utilities

[<AutoOpen>]
module internal Casting =

    open System.Threading.Tasks

    /// <summary>
    /// Upcasts an error mapping function to work with obj types.
    /// </summary>
    /// <param name="onError">The error mapping function.</param>
    let inline upcastOnError (onError: exn -> 'E) : (exn -> obj) =
        fun (exn: exn) -> onError exn :> obj

    /// <summary>
    /// Upcasts a function's return type to obj.
    /// </summary>
    /// <param name="func">The function to upcast.</param>
    let inline upcastFunc (func: unit -> 'R) : unit -> obj =
        fun () -> func () :> obj

    /// <summary>
    /// Upcasts a generic Task's result type to obj.
    /// </summary>
    /// <param name="genericTask">The task to upcast.</param>
    let inline upcastTask (genericTask: Task<'R>) : Task<obj> =
        task {
            let! res = genericTask
            return box res
        }
