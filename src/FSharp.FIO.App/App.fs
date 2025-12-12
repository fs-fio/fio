(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2025 - Daniel "iyyel" Larsen and Technical University of Denmark (DTU) *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides application-level helpers and the <c>FIOApp</c> base class for running FIO effects as applications.
/// Includes default runtime, fiber handling, and customizable success/error handlers for effectful entry points.
/// </summary>
module FSharp.FIO.App

open FSharp.FIO.DSL
open FSharp.FIO.Runtime
open FSharp.FIO.Runtime.Concurrent

open System
open System.Threading
open System.Threading.Tasks

module private ThreadPoolConfig =
    let configure () =
        let cores = Environment.ProcessorCount
        let minWorkerThreads = cores * 2
        let maxWorkerThreads = cores * 50
        let minIOThreads = cores
        let maxIOThreads = cores * 10
        
        ThreadPool.SetMinThreads(minWorkerThreads, minIOThreads) |> ignore
        ThreadPool.SetMaxThreads(maxWorkerThreads, maxIOThreads) |> ignore

do ThreadPoolConfig.configure ()

let private defaultRuntime = Runtime()

let private mergeResult onSuccess onError = function
    | Ok res -> onSuccess res
    | Error err -> onError err

let private mergeFiber onSuccess onError (fiber: Fiber<'R, 'E>) = task {
    let! res = fiber.Task()
    return! mergeResult onSuccess onError res
}

let private defaultOnSuccess res = task {
    Console.ForegroundColor <- ConsoleColor.DarkGreen
    Console.WriteLine $"%A{Ok res}"
    Console.ResetColor()
}

let private defaultOnError err = task {
    Console.ForegroundColor <- ConsoleColor.DarkRed
    Console.WriteLine $"%A{Error err}"
    Console.ResetColor()
}

let private defaultFiberHandler fiber = mergeFiber defaultOnSuccess defaultOnError fiber

/// <summary>
/// Abstract base class for FIO applications. Provides effectful entry points and customizable handlers for success and error cases.
/// </summary>
/// <typeparam name="R">The result type of the application effect.</typeparam>
/// <typeparam name="E">The error type of the application effect.</typeparam>
/// <param name="onSuccess">Handler to invoke on success.</param>
/// <param name="onError">Handler to invoke on error.</param>
/// <param name="runtime">The runtime to use for interpretation.</param>
[<AbstractClass>]
type FIOApp<'R, 'E> (onSuccess: 'R -> Task<unit>, onError: 'E -> Task<unit>, runtime: FRuntime) =
    let fiberHandler = mergeFiber onSuccess onError

    /// <summary>
    /// Initializes a new <c>FIOApp</c> with default handlers and runtime.
    /// </summary>
    new() = FIOApp(defaultOnSuccess, defaultOnError, defaultRuntime)

    /// <summary>
    /// The main effect to be interpreted by the application.
    /// </summary>
    /// <returns>The main FIO effect of the application.</returns>
    abstract member effect: FIO<'R, 'E>

    /// <summary>
    /// Runs the given <c>FIOApp</c> instance using its configured runtime and handlers.
    /// </summary>
    /// <typeparam name="R">The result type of the application effect.</typeparam>
    /// <typeparam name="E">The error type of the application effect.</typeparam>
    /// <param name="app">The FIOApp instance to run.</param>
    static member Run<'R, 'E> (app: FIOApp<'R, 'E>) =
        app.Run()

    /// <summary>
    /// Runs the given FIO effect using the default runtime and handlers.
    /// </summary>
    /// <typeparam name="R">The result type of the effect.</typeparam>
    /// <typeparam name="E">The error type of the effect.</typeparam>
    /// <param name="eff">The FIO effect to run.</param>
    static member Run<'R, 'E> (eff: FIO<'R, 'E>) =
        let fiber = defaultRuntime.Run eff
        let task = defaultFiberHandler fiber
        task.Wait()

    /// <summary>
    /// Runs the application effect using the configured runtime and handlers.
    /// </summary>
    /// <typeparam name="R">The result type of the application effect.</typeparam>
    /// <typeparam name="E">The error type of the application effect.</typeparam>
    member this.Run<'R, 'E> () =
        this.Run runtime

    /// <summary>
    /// Runs the application effect using the specified runtime and the configured handlers.
    /// </summary>
    /// <typeparam name="R">The result type of the application effect.</typeparam>
    /// <typeparam name="E">The error type of the application effect.</typeparam>
    /// <param name="runtime">The runtime to use for interpretation.</param>
    member this.Run<'R, 'E> runtime =
        let fiber = runtime.Run this.effect
        let task = fiberHandler fiber
        task.Wait()

    /// <summary>
    /// Runs the application effect using the specified success and error handlers.
    /// </summary>
    /// <typeparam name="R">The result type of the application effect.</typeparam>
    /// <typeparam name="E">The error type of the application effect.</typeparam>
    /// <typeparam name="F">The result type of the handlers.</typeparam>
    /// <param name="onSuccess">Handler to invoke on success.</param>
    /// <param name="onError">Handler to invoke on error.</param>
    member this.Run<'R, 'E, 'F> (onSuccess: 'R -> Task<'F>, onError: 'E -> Task<'F>) =
        let fiber = runtime.Run this.effect
        let task = mergeFiber onSuccess onError fiber
        task.Wait()
