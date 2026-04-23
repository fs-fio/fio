namespace FIO.Runtime

open FIO.DSL

open System
open System.Threading
open System.Globalization
open System.Threading.Tasks
open System.Collections.Generic

/// Classifies a worker for fault severity assessment.
[<Struct>]
type WorkerKind =
    /// Worker that evaluates FIO effects.
    | Evaluation
    /// Worker that handles blocked fibers.
    | Blocking

/// Describes a worker fault event.
[<Struct; NoComparison; NoEquality>]
type WorkerFaultEvent =
    {
        /// Human-readable worker identifier.
        WorkerName: string
        /// Whether this is an evaluation or blocking worker.
        WorkerKind: WorkerKind
        /// Cumulative fault count for this worker.
        FaultCount: int
        /// The exception that caused the fault.
        Exception: exn
        /// When the fault occurred.
        Timestamp: DateTimeOffset
        /// Whether the worker will attempt to restart.
        WillRestart: bool
    }

/// Runtime health state.
[<Struct>]
type RuntimeHealthState =
    /// All workers running normally.
    | Healthy
    /// Some evaluation workers faulted; throughput reduced but no deadlocks.
    | Degraded of faultedWorkers: int
    /// Critical worker failure; runtime cannot make progress.
    | Faulted of reason: string

/// Configures the worker restart policy.
type WorkerRestartPolicy =
    {
        /// Maximum total faults per worker before permanent shutdown.
        MaxFaultsPerWorker: int
        /// Sliding window duration for rapid-fault detection.
        FaultWindow: TimeSpan
        /// Maximum faults within the window before permanent shutdown.
        MaxFaultsInWindow: int
        /// Initial backoff delay after first fault.
        InitialBackoff: TimeSpan
        /// Maximum backoff delay.
        MaxBackoff: TimeSpan
        /// Backoff multiplier per consecutive fault.
        BackoffMultiplier: float
    }

    /// Default restart policy.
    /// <returns>Policy with 5 max faults, 60s window, 3 max in window, 100ms initial backoff, 5s max backoff.</returns>
    static member Default =
        {
            MaxFaultsPerWorker = 5
            FaultWindow = TimeSpan.FromSeconds 60.0
            MaxFaultsInWindow = 3
            InitialBackoff = TimeSpan.FromMilliseconds 100.0
            MaxBackoff = TimeSpan.FromSeconds 5.0
            BackoffMultiplier = 2.0
        }

/// Thrown when Run() is called on a permanently faulted runtime.
type RuntimeFaultedException(message: string) =
    inherit InvalidOperationException(message)

/// Tracks worker health across a runtime instance.
/// <remarks>Thread-safe: multiple workers may report faults concurrently.</remarks>
type internal WorkerHealthMonitor(ewCount: int) =
    let mutable state = 0
    let mutable faultedEWCount = 0
    let mutable faultReason = ""
    let onFaultCallbacks = ResizeArray<Action<WorkerFaultEvent>>()
    let callbackLock = obj ()

    /// Gets the current runtime health state.
    /// <returns>The current health state of the runtime.</returns>
    member _.State =
        match Volatile.Read &state with
        | 0 -> Healthy
        | 1 -> Degraded(Volatile.Read &faultedEWCount)
        | _ -> Faulted(Volatile.Read &faultReason)

    /// Reports a permanently faulted worker.
    /// <param name="kind">The kind of worker that faulted.</param>
    /// <param name="reason">Description of the fault.</param>
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

    /// Reports a worker fault event to registered callbacks.
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

    /// Registers a callback to be invoked when any worker faults.
    /// <param name="callback">The callback to register.</param>
    member _.OnFault(callback: Action<WorkerFaultEvent>) =
        lock callbackLock (fun () -> onFaultCallbacks.Add callback)

/// Base class for worker-based FIO runtimes.
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime()

    let validateWorkerConfiguration () =
        if config.EWC <= 0 || config.EWS <= 0 || config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString()}"

    do validateWorkerConfiguration ()

    let monitor = WorkerHealthMonitor(config.EWC)

    member _.WorkerConfig = config

    /// Gets the health monitor for this runtime.
    member internal _.Monitor = monitor

    /// Gets the current health state of the runtime.
    /// <returns>The current health state.</returns>
    member _.HealthState = monitor.State

    /// Registers a callback to be invoked when any worker faults.
    /// <param name="callback">The callback to register.</param>
    member _.OnWorkerFault(callback: Action<WorkerFaultEvent>) = monitor.OnFault callback

    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    override this.ToString() = $"{this.Name} ({this.ConfigString})"

/// Shared worker lifecycle management for worker-based runtimes.
module internal WorkerLifecycle =

    /// Starts a worker with automatic restart-on-fault, exponential backoff,
    /// and health monitoring.
    /// <param name="monitor">Health monitor for fault reporting.</param>
    /// <param name="policy">Restart policy governing backoff and fault limits.</param>
    /// <param name="workerName">Human-readable worker name for diagnostics.</param>
    /// <param name="workerKind">Whether this is an evaluation or blocking worker.</param>
    /// <param name="onRestart">Called before backoff on restart; use to clear internal state.</param>
    /// <param name="innerLoop">The worker's main loop; should run until cancellation or channel closure.</param>
    /// <returns>A struct tuple of the CancellationTokenSource (for disposal) and the worker Task.</returns>
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
