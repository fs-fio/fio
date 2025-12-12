(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2025 - Daniel "iyyel" Larsen and Technical University of Denmark (DTU) *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

namespace FSharp.FIO.Runtime

open FSharp.FIO.DSL

open System
open System.Globalization
open System.Collections.Generic

[<AutoOpen>]
module private Utils =
    
    let inline pop (contStack: ResizeArray<ContStackFrame>) =
        let lastIndex = contStack.Count - 1
        let stackFrame = contStack[lastIndex]
        contStack.RemoveAt lastIndex
        stackFrame

/// <summary>
/// Object pool for continuation stacks to reduce GC pressure.
/// Thread-local pooling avoids synchronization overhead.
/// </summary>
type internal ContStackPool private () =
    [<ThreadStatic; DefaultValue>]
    static val mutable private pool: Stack<ResizeArray<ContStackFrame>>
    
    /// <summary>
    /// Rents a continuation stack from the pool or creates a new one.
    /// </summary>
    /// <returns>A cleared ResizeArray ready for use.</returns>
    static member inline Rent () =
        if isNull ContStackPool.pool then 
            ContStackPool.pool <- Stack<_>()
        
        if ContStackPool.pool.Count > 0 then
            let stack = ContStackPool.pool.Pop()
            stack.Clear()
            stack
        else
            ResizeArray<ContStackFrame> 32  // Pre-sized to reduce resizing
    
    /// <summary>
    /// Returns a continuation stack to the pool for reuse.
    /// Large stacks are not pooled to avoid holding excessive memory.
    /// </summary>
    /// <param name="stack">The stack to return to the pool.</param>
    static member inline Return (stack: ResizeArray<ContStackFrame>) =
        if stack.Capacity <= 1024 then  // Don't pool huge stacks
            stack.Clear()
            ContStackPool.pool.Push stack

/// <summary>
/// Represents a functional runtime for interpreting FIO effects.
/// </summary>
[<AbstractClass>]
type FRuntime internal () =
    /// <summary>
    /// Gets the name of the runtime.
    /// </summary>
    abstract member Name : string

    /// <summary>
    /// Gets the configuration string for the runtime.
    /// </summary>
    abstract member ConfigString : string

    override this.ConfigString =
        this.Name

    /// <summary>
    /// Runs an FIO effect and returns a fiber representing its execution.
    /// </summary>
    /// <typeparam name="R">The result type.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="eff">The FIO effect to run.</param>
    /// <returns>A fiber representing the running effect.</returns>
    abstract member Run<'R, 'E> : FIO<'R, 'E> -> Fiber<'R, 'E>

    /// <summary>
    /// Gets a file-friendly string representation of the runtime.
    /// </summary>
    /// <returns>A string suitable for file names.</returns>
    member this.ToFileString () =
        this.ToString()
            .ToLowerInvariant()
            .Replace("(", "")
            .Replace(")", "")
            .Replace(":", "")
            .Replace(' ', '-')
    
    override this.ToString () =
        this.ConfigString

/// <summary>
/// Represents the configuration for a worker runtime.
/// </summary>
type WorkerConfig =
    { EWC: int
      EWS: int
      BWC: int }

/// <summary>
/// Represents a functional worker runtime for interpreting FIO effects.
/// </summary>
[<AbstractClass>]
type FWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FRuntime ()

    let validateWorkerConfiguration () =
        if config.EWC <= 0 ||
           config.EWS <= 0 ||
           config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString ()}"

    do validateWorkerConfiguration ()

    /// <summary>
    /// Gets the worker configuration.
    /// </summary>
    /// <returns>The worker configuration.</returns>
    member _.GetWorkerConfiguration () =
        config

    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    override this.ToString () =
        $"{this.Name} ({this.ConfigString})"
