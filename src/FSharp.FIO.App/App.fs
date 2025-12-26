/// <summary>
/// Application-level helpers and the FIOApp base class for running FIO effects.
/// </summary>
module FSharp.FIO.App

open FSharp.FIO.DSL
open FSharp.FIO.Runtime
open FSharp.FIO.Runtime.Default

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

let private defaultRuntime = new DefaultRuntime()

/// <summary>
/// Applies the appropriate handler based on success or error.
/// </summary>
/// <param name="onSuccess">Handler to invoke on success.</param>
/// <param name="onError">Handler to invoke on error.</param>
let private mergeResult onSuccess onError = function
    | Ok res -> onSuccess res
    | Error err -> onError err

/// <summary>
/// Awaits a fiber and applies the appropriate handler.
/// </summary>
/// <param name="onSuccess">Handler to invoke on success.</param>
/// <param name="onError">Handler to invoke on error.</param>
/// <param name="fiber">The fiber to await.</param>
let private mergeFiber onSuccess onError (fiber: Fiber<'R, 'E>) = task {
    let! res = fiber.Task()
    return! mergeResult onSuccess onError res
}

/// <summary>
/// Default success handler that prints the result in green.
/// </summary>
/// <param name="res">The result to print.</param>
let private defaultOnSuccess res = task {
    Console.ForegroundColor <- ConsoleColor.DarkGreen
    Console.WriteLine $"%A{Ok res}"
    Console.ResetColor()
}

/// <summary>
/// Default error handler that prints the error in red.
/// </summary>
/// <param name="err">The error to print.</param>
let private defaultOnError err = task {
    Console.ForegroundColor <- ConsoleColor.DarkRed
    Console.WriteLine $"%A{Error err}"
    Console.ResetColor()
}

/// <summary>
/// Default fiber handler using default success and error handlers.
/// </summary>
/// <param name="fiber">The fiber to handle.</param>
let private defaultFiberHandler fiber = mergeFiber defaultOnSuccess defaultOnError fiber

/// <summary>
/// Abstract base class for FIO applications.
/// </summary>
/// <param name="onSuccess">Handler to invoke on success.</param>
/// <param name="onError">Handler to invoke on error.</param>
/// <param name="runtime">The runtime to use for interpretation.</param>
[<AbstractClass>]
type FIOApp<'R, 'E> (onSuccess: 'R -> Task<unit>, onError: 'E -> Task<unit>, runtime: FRuntime) =
    let fiberHandler = mergeFiber onSuccess onError

    /// <summary>
    /// Initializes a new FIOApp with default handlers and runtime.
    /// </summary>
    new() = FIOApp(defaultOnSuccess, defaultOnError, defaultRuntime)

    /// <summary>
    /// The main effect to interpret.
    /// </summary>
    abstract member effect: FIO<'R, 'E>

    /// <summary>
    /// Runs the given FIOApp instance.
    /// </summary>
    /// <param name="app">The FIOApp instance to run.</param>
    static member Run<'R, 'E> (app: FIOApp<'R, 'E>) =
        app.Run()

    /// <summary>
    /// Runs the given FIO effect using default runtime and handlers.
    /// </summary>
    /// <param name="eff">The FIO effect to run.</param>
    static member Run<'R, 'E> (eff: FIO<'R, 'E>) =
        let fiber = defaultRuntime.Run eff
        let task = defaultFiberHandler fiber
        task.Wait()

    /// <summary>
    /// Runs the application effect.
    /// </summary>
    member this.Run<'R, 'E> () =
        this.Run runtime

    /// <summary>
    /// Runs the application effect with the specified runtime.
    /// </summary>
    /// <param name="runtime">The runtime to use.</param>
    member this.Run<'R, 'E> runtime =
        let fiber = runtime.Run this.effect
        let task = fiberHandler fiber
        task.Wait()

    /// <summary>
    /// Runs the application effect with the specified handlers.
    /// </summary>
    /// <param name="onSuccess">Handler to invoke on success.</param>
    /// <param name="onError">Handler to invoke on error.</param>
    member this.Run<'R, 'E, 'F> (onSuccess: 'R -> Task<'F>, onError: 'E -> Task<'F>) =
        let fiber = runtime.Run this.effect
        let task = mergeFiber onSuccess onError fiber
        task.Wait()
