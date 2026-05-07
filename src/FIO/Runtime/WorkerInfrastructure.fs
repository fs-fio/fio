namespace FIO.Runtime

open FIO.DSL

open System
open System.Threading
open System.Globalization
open System.Threading.Tasks
open System.Collections.Generic

/// <summary>Represents the kind of a worker for fault severity assessment.</summary>
[<Struct>]
type WorkerKind =
    /// <summary>Represents a worker that interprets FIO effects.</summary>
    | Evaluation
    /// <summary>Represents a worker that handles fibers blocked on channels or other fibers.</summary>
    | Blocking

/// <summary>Represents a worker fault event reported by the runtime's health monitor.</summary>
[<Struct; NoComparison; NoEquality>]
type WorkerFaultEvent =
    {
        /// <summary>Represents the human-readable worker identifier.</summary>
        WorkerName: string
        /// <summary>Represents the worker's role at the time of the fault.</summary>
        WorkerKind: WorkerKind
        /// <summary>Represents the cumulative number of faults observed for this worker.</summary>
        FaultCount: int
        /// <summary>Represents the exception that caused the fault.</summary>
        Exception: exn
        /// <summary>Represents the time at which the fault was observed.</summary>
        Timestamp: DateTimeOffset
        /// <summary>Represents whether the worker will attempt to restart after this fault.</summary>
        WillRestart: bool
    }

/// <summary>Represents the overall health state of a worker-based runtime.</summary>
[<Struct>]
type RuntimeHealthState =
    /// <summary>Represents a runtime in which all workers are running normally.</summary>
    | Healthy
    /// <summary>Represents a runtime with one or more faulted evaluation workers but still capable of progress, carrying the count of faulted workers.</summary>
    | Degraded of faultedWorkers: int
    /// <summary>Represents a runtime that cannot make further progress, carrying a human-readable reason.</summary>
    | Faulted of reason: string

/// <summary>Represents the restart policy applied to faulted workers.</summary>
type WorkerRestartPolicy =
    {
        /// <summary>Represents the maximum total number of faults a single worker may incur before permanent shutdown.</summary>
        MaxFaultsPerWorker: int
        /// <summary>Represents the sliding-window duration used to detect rapid faults.</summary>
        FaultWindow: TimeSpan
        /// <summary>Represents the maximum number of faults permitted within <c>FaultWindow</c> before permanent shutdown.</summary>
        MaxFaultsInWindow: int
        /// <summary>Represents the initial backoff delay applied after the first fault.</summary>
        InitialBackoff: TimeSpan
        /// <summary>Represents the maximum backoff delay between restart attempts.</summary>
        MaxBackoff: TimeSpan
        /// <summary>Represents the multiplier applied to the backoff delay on each consecutive fault.</summary>
        BackoffMultiplier: float
    }

    /// <summary>Returns the default restart policy.</summary>
    /// <returns>A policy permitting up to 5 faults per worker, 3 within a 60-second window, with backoff growing from 100&#160;ms to 5&#160;s.</returns>
    static member Default =
        {
            MaxFaultsPerWorker = 5
            FaultWindow = TimeSpan.FromSeconds 60.0
            MaxFaultsInWindow = 3
            InitialBackoff = TimeSpan.FromMilliseconds 100.0
            MaxBackoff = TimeSpan.FromSeconds 5.0
            BackoffMultiplier = 2.0
        }

/// <summary>Represents the exception raised when <c>Run</c> is invoked on a permanently faulted runtime.</summary>
/// <param name="message">The diagnostic message describing the fault.</param>
type RuntimeFaultedException(message: string) =
    inherit InvalidOperationException(message)

/// <summary>Represents a health monitor that tracks worker fault counts and overall runtime health state.</summary>
/// <param name="ewCount">The total number of evaluation workers being monitored.</param>
type internal WorkerHealthMonitor(ewCount: int) =
    /// <summary>Represents the encoded health state: 0 = healthy, 1 = degraded, 2 = faulted.</summary>
    let mutable state = 0
    /// <summary>Represents the number of evaluation workers that have permanently faulted.</summary>
    let mutable faultedEWCount = 0
    /// <summary>Represents the human-readable reason when the runtime enters the faulted state.</summary>
    let mutable faultReason = ""
    /// <summary>Represents the registered fault-notification callbacks.</summary>
    let onFaultCallbacks = ResizeArray<Action<WorkerFaultEvent>>()
    /// <summary>Represents the synchronization guard for the callback list.</summary>
    let callbackLock = obj ()

    /// <summary>Returns the current health state of the runtime derived from tracked faults.</summary>
    /// <returns>A <c>RuntimeHealthState</c> value indicating healthy, degraded, or faulted.</returns>
    member _.State =
        match Volatile.Read &state with
        | 0 -> Healthy
        | 1 -> Degraded(Volatile.Read &faultedEWCount)
        | _ -> Faulted(Volatile.Read &faultReason)

    /// <summary>Transitions the health state to degraded or faulted after a worker is permanently shut down.</summary>
    /// <param name="kind">The kind of the permanently faulted worker.</param>
    /// <param name="reason">A human-readable description of the fault.</param>
    member _.ReportPermanentFault(kind: WorkerKind, reason: string) =
        match kind with
        | Evaluation ->
            let newCount = Interlocked.Increment &faultedEWCount

            if newCount >= ewCount then
                Volatile.Write(&faultReason, "All evaluation workers permanently faulted.")
                Volatile.Write(&state, 2)
            else
                Interlocked.CompareExchange(&state, 1, 0) |> ignore
        | Blocking ->
            Volatile.Write(&faultReason, $"BlockingWorker permanently faulted: {reason}")
            Volatile.Write(&state, 2)

    /// <summary>Builds a diagnostic log entry and invokes all registered fault-notification callbacks for the given event.</summary>
    /// <param name="event'">The fault event to report.</param>
    member _.NotifyFault(event': WorkerFaultEvent) =
        Console.Error.WriteLine
            $"[FIO] {event'.WorkerName} fault #{event'.FaultCount}: {event'.Exception.Message} (willRestart={event'.WillRestart})"

        let callbacks = lock callbackLock (fun () -> onFaultCallbacks.ToArray())

        for cb in callbacks do
            try
                cb.Invoke event'
            with _ ->
                ()

    /// <summary>Creates a fault-notification subscription that invokes the given callback on each worker fault.</summary>
    /// <param name="callback">The callback to invoke when a worker fault is observed.</param>
    member _.OnFault(callback: Action<WorkerFaultEvent>) =
        lock callbackLock (fun () -> onFaultCallbacks.Add callback)

/// <summary>Represents the abstract base for worker-based FIO runtimes that share evaluation, scheduling, and blocking-worker infrastructure.</summary>
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime()

    /// <summary>Builds a validated worker-based runtime, raising an exception if the configuration is invalid.</summary>
    let validateWorkerConfiguration () =
        if config.EWC <= 0 || config.EWS <= 0 || config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString()}"

    do validateWorkerConfiguration ()

    /// <summary>Represents the health monitor for this runtime's workers.</summary>
    let monitor = WorkerHealthMonitor(config.EWC)

    /// <summary>Returns the worker configuration this runtime was constructed with.</summary>
    /// <returns>The configuration carrying evaluation/blocking worker counts and per-work-item step budget.</returns>
    member _.WorkerConfig = config

    /// <summary>Returns the health monitor used by this runtime's workers.</summary>
    /// <returns>The <c>WorkerHealthMonitor</c> instance tracking worker faults.</returns>
    member internal _.Monitor = monitor

    /// <summary>Returns the current health state of this runtime.</summary>
    /// <returns>A <c>RuntimeHealthState</c> indicating whether the runtime is healthy, degraded, or faulted.</returns>
    member _.HealthState = monitor.State

    /// <summary>Creates a fault-notification subscription that invokes the given callback on each worker fault.</summary>
    /// <param name="callback">A function from the fault event to its handling side effect; called once per fault.</param>
    member _.OnWorkerFault(callback: Action<WorkerFaultEvent>) = monitor.OnFault callback

    /// <summary>Returns a string describing this runtime's worker configuration.</summary>
    /// <returns>A formatted string with the evaluation worker count, step budget, and blocking worker count.</returns>
    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    /// <summary>Returns a string combining the runtime name with its worker configuration.</summary>
    /// <returns>A formatted string of the form "Name (ConfigString)".</returns>
    override this.ToString() = $"{this.Name} ({this.ConfigString})"

/// <summary>Represents lifecycle functions for starting and restarting workers with fault tolerance and exponential backoff.</summary>
module internal WorkerLifecycle =

    /// <summary>Creates a long-running worker task that restarts on faults according to the given policy.</summary>
    /// <param name="monitor">The health monitor that tracks worker faults.</param>
    /// <param name="policy">The restart policy controlling fault limits and backoff.</param>
    /// <param name="workerName">The human-readable name for diagnostic logging.</param>
    /// <param name="workerKind">The kind of worker being started.</param>
    /// <param name="onRestart">A callback invoked before each restart attempt.</param>
    /// <param name="innerLoop">The worker's main loop, which receives a cancellation token and runs until cancelled or faulted.</param>
    /// <returns>A struct tuple of the cancellation token source controlling the worker and the worker's background task.</returns>
    let startWorker
        (monitor: WorkerHealthMonitor)
        (policy: WorkerRestartPolicy)
        (workerName: string)
        (workerKind: WorkerKind)
        (onRestart: unit -> unit)
        (innerLoop: CancellationToken -> Task<unit>)
        : struct (CancellationTokenSource * Task) =
        let cts = new CancellationTokenSource()

        let workerTask =
            Task.Factory
                .StartNew(
                    Func<Task>(fun () ->
                        task {
                            let token = cts.Token
                            let mutable faultCount = 0
                            let recentFaults = ResizeArray<DateTimeOffset>(policy.MaxFaultsInWindow)
                            let mutable permanentlyFaulted = false

                            while not permanentlyFaulted && not token.IsCancellationRequested do
                                try
                                    do! innerLoop token
                                    permanentlyFaulted <- true
                                with
                                | :? OperationCanceledException -> permanentlyFaulted <- true
                                | exn when not token.IsCancellationRequested ->
                                    faultCount <- faultCount + 1
                                    let now = DateTimeOffset.UtcNow

                                    let windowStart = now - policy.FaultWindow
                                    let mutable i = 0

                                    while i < recentFaults.Count do
                                        if recentFaults.[i] < windowStart then
                                            recentFaults.RemoveAt i
                                        else
                                            i <- i + 1

                                    recentFaults.Add now

                                    let willRestart =
                                        faultCount < policy.MaxFaultsPerWorker
                                        && recentFaults.Count < policy.MaxFaultsInWindow

                                    monitor.NotifyFault
                                        {
                                            WorkerName = workerName
                                            WorkerKind = workerKind
                                            FaultCount = faultCount
                                            Exception = exn
                                            Timestamp = now
                                            WillRestart = willRestart
                                        }

                                    if willRestart then
                                        onRestart ()

                                        let backoffMs =
                                            policy.InitialBackoff.TotalMilliseconds
                                            * pown policy.BackoffMultiplier (faultCount - 1)
                                            |> min policy.MaxBackoff.TotalMilliseconds
                                            |> int

                                        do! Task.Delay(backoffMs, token)
                                    else
                                        monitor.ReportPermanentFault(
                                            workerKind,
                                            $"{workerName} permanently faulted: {exn.Message}"
                                        )

                                        permanentlyFaulted <- true
                                | _ -> permanentlyFaulted <- true
                        }
                        :> Task),
                    CancellationToken.None,
                    TaskCreationOptions.LongRunning,
                    TaskScheduler.Default
                )
                .Unwrap()

        struct (cts, workerTask)
