namespace FIO.Runtime

open System
open System.Threading
open System.Globalization
open System.Threading.Tasks

/// <summary>Represents the abstract base for worker-based FIO runtimes that share evaluation, scheduling, and blocking-worker infrastructure.</summary>
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime()

    let validateWorkerConfiguration () =
        if config.EWC <= 0 || config.EWS <= 0 || config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString()}"

    do validateWorkerConfiguration ()

    /// <summary>Returns the worker configuration this runtime was constructed with.</summary>
    /// <returns>The configuration carrying evaluation/blocking worker counts and per-work-item step budget.</returns>
    member _.WorkerConfig = config

    /// <summary>Returns a string describing this runtime's worker configuration.</summary>
    /// <returns>A formatted string with the evaluation worker count, step budget, and blocking worker count.</returns>
    override _.ConfigString =
        let ci = CultureInfo "en-US"
        $"""EWC: %s{config.EWC.ToString("N0", ci)} EWS: %s{config.EWS.ToString("N0", ci)} BWC: %s{config.BWC.ToString("N0", ci)}"""

    /// <summary>Returns a string combining the runtime name with its worker configuration.</summary>
    /// <returns>A formatted string of the form "Name (ConfigString)".</returns>
    override this.ToString() = $"{this.Name} ({this.ConfigString})"

/// <summary>Represents lifecycle helpers for starting long-running worker tasks.</summary>
module internal WorkerLifecycle =

    /// <summary>Creates a long-running worker task. On unhandled exception the task faults and the worker stops permanently; the exception is logged to stderr so worker death is not silent.</summary>
    /// <param name="workerName">The human-readable name used in diagnostic logging.</param>
    /// <param name="innerLoop">The worker's main loop, which receives a cancellation token and runs until cancelled or it throws.</param>
    /// <returns>A struct tuple of the cancellation token source controlling the worker and the worker's background task.</returns>
    let startWorker
        (workerName: string)
        (innerLoop: CancellationToken -> Task<unit>)
        : struct (CancellationTokenSource * Task) =
        let cts = new CancellationTokenSource()

        let workerTask =
            Task.Factory
                .StartNew(
                    Func<Task>(fun () ->
                        task {
                            try
                                do! innerLoop cts.Token
                            with
                            | :? OperationCanceledException -> ()
                            | exn ->
                                Console.Error.WriteLine $"[FIO] {workerName} faulted: {exn.Message}"
                                raise exn
                        }
                        :> Task),
                    CancellationToken.None,
                    TaskCreationOptions.LongRunning,
                    TaskScheduler.Default
                )
                .Unwrap()

        struct (cts, workerTask)

/// <summary>Provides shared worker-construction helpers used by all <c>FIOWorkerRuntime</c> subclasses.</summary>
module internal WorkerBuilders =

    /// <summary>Creates the paired lists of blocking and evaluation workers, distributing each evaluation worker across blocking workers via round-robin indexing.</summary>
    /// <typeparam name="'B">The concrete blocking-worker type used by this runtime.</typeparam>
    /// <typeparam name="'E">The concrete evaluation-worker type used by this runtime.</typeparam>
    /// <param name="blockingCount">The number of blocking workers to build (BWC).</param>
    /// <param name="evaluationCount">The number of evaluation workers to build (EWC).</param>
    /// <param name="blockingFactory">A factory that builds the blocking worker at the given index.</param>
    /// <param name="evaluationFactory">A factory that builds the evaluation worker at the given index, paired with its round-robin-assigned blocking worker.</param>
    /// <returns>A struct tuple of the blocking-worker list and the evaluation-worker list.</returns>
    let inline buildPairedWorkers
        (blockingCount: int)
        (evaluationCount: int)
        ([<InlineIfLambda>] blockingFactory: int -> 'B)
        ([<InlineIfLambda>] evaluationFactory: int -> 'B -> 'E)
        : struct ('B list * 'E list) =
        let blockingWorkers = List.init blockingCount blockingFactory
        let blockingWorkerCount = blockingWorkers.Length

        let evaluationWorkers =
            List.init evaluationCount (fun i -> evaluationFactory i blockingWorkers.[i % blockingWorkerCount])

        struct (blockingWorkers, evaluationWorkers)
