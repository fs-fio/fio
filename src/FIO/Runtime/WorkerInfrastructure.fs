namespace FIO.Runtime

open System
open System.Threading
open System.Globalization
open System.Threading.Tasks

/// Base class for worker-based FIO runtimes, configured with a worker configuration.
[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime()

    static let cultureEnUs = CultureInfo "en-US"

    let validateWorkerConfiguration () =
        if config.EvaluationWorkers <= 0 || config.EvaluationSteps <= 0 || config.BlockingWorkers <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString()}"

    do validateWorkerConfiguration ()

    /// The worker configuration this runtime was created with.
    member _.WorkerConfig =
        config

    override _.ConfigString =
        $"""EWC: %s{config.EvaluationWorkers.ToString("N0", cultureEnUs)} EWS: %s{config.EvaluationSteps.ToString("N0", cultureEnUs)} BWC: %s{config.BlockingWorkers.ToString("N0", cultureEnUs)}"""

    override this.ToString () =
        $"{this.Name} ({this.ConfigString})"

module internal WorkerLifecycle =

    let startWorker (workerName: string) (innerLoop: CancellationToken -> Task<unit>)
        : struct (CancellationTokenSource * Task) =
        let cancelSource = new CancellationTokenSource()
        let workerTask =
            Task.Factory.StartNew(Func<Task>(fun () ->
                task {
                    try
                        do! innerLoop cancelSource.Token
                    with
                    | :? OperationCanceledException -> ()
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
        ([<InlineIfLambda>] evaluationFactory: int -> 'A -> 'A1)
        : struct ('A list * 'A1 list) =
        let blockingWorkers = List.init blockingCount blockingFactory
        let blockingWorkerCount = blockingWorkers.Length

        let evaluationWorkers =
            List.init evaluationCount (fun i -> evaluationFactory i blockingWorkers[i % blockingWorkerCount])

        struct (blockingWorkers, evaluationWorkers)
