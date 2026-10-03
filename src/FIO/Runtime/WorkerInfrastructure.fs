namespace FIO.Runtime

open System
open System.Threading
open System.Globalization
open System.Threading.Tasks

/// Base class for worker-based FIO runtimes, configured with a worker configuration.
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) =
    inherit FIORuntime()

    static let cultureEnUs = CultureInfo "en-US"

    static let describe (config: WorkerConfig) =
        $"""EWC: %s{config.EvaluationWorkers.ToString("N0", cultureEnUs)} EWS: %s{config.EvaluationSteps.ToString("N0", cultureEnUs)} BWC: %s{config.BlockingWorkers.ToString("N0", cultureEnUs)}"""

    let validateWorkerConfiguration () =
        if config.EvaluationWorkers <= 0 || config.EvaluationSteps <= 0 || config.BlockingWorkers <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{describe config}"

    do validateWorkerConfiguration ()

    /// The worker configuration this runtime was created with.
    member _.WorkerConfig : WorkerConfig =
        config

    override _.ConfigString : string =
        describe config

    override this.ToString () : string =
        $"{this.Name} ({this.ConfigString})"

module internal WorkerLifecycle =

    let startWorker (workerName: string) (innerLoop: CancellationToken -> Task<unit>) =
        let cancelSource = new CancellationTokenSource()
        let cancellationToken = cancelSource.Token

        let workerTask =
            Task.Factory.StartNew(Func<Task>(fun () ->
                task {
                    try
                        do! innerLoop cancellationToken
                    with
                    | :? OperationCanceledException -> ()
                    | :? ObjectDisposedException -> ()
                    | ex ->
                        Console.Error.WriteLine $"FIO Worker '{workerName}' encountered an unhandled exception: {ex}"
                        raise ex
                } :> Task),
            CancellationToken.None,
            TaskCreationOptions.LongRunning,
            TaskScheduler.Default
            ).Unwrap()

        struct (cancelSource, workerTask)

module internal WorkerBuilders =

    let inline buildPairedWorkers
        (blockingCount: int)
        (evaluationCount: int)
        ([<InlineIfLambda>] blockingFactory: int -> 'A)
        ([<InlineIfLambda>] evaluationFactory: int -> 'A -> 'A1) =
        let blockingWorkers = List.init blockingCount blockingFactory
        let blockingWorkerCount = blockingWorkers.Length

        let evaluationWorkers =
            List.init evaluationCount (fun i -> evaluationFactory i blockingWorkers[i % blockingWorkerCount])

        struct (blockingWorkers, evaluationWorkers)
