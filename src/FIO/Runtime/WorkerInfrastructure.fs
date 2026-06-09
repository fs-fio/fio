namespace FIO.Runtime

open System
open System.Threading
open System.Globalization
open System.Threading.Tasks

[<AbstractClass>]
type FIOWorkerRuntime internal (config: WorkerConfig) as this =
    inherit FIORuntime()

    static let cultureEnUs = CultureInfo "en-US"

    let validateWorkerConfiguration () =
        if config.EWC <= 0 || config.EWS <= 0 || config.BWC <= 0 then
            invalidArg "config" $"Invalid worker configuration! %s{this.ToString()}"

    do validateWorkerConfiguration ()

    member _.WorkerConfig =
        config

    override _.ConfigString =
        $"""EWC: %s{config.EWC.ToString("N0", cultureEnUs)} EWS: %s{config.EWS.ToString("N0", cultureEnUs)} BWC: %s{config.BWC.ToString("N0", cultureEnUs)}"""

    override this.ToString () =
        $"{this.Name} ({this.ConfigString})"

module internal WorkerLifecycle =

    let startWorker (workerName: string) (innerLoop: CancellationToken -> Task<unit>)
        : struct (CancellationTokenSource * Task) =
        let cts = new CancellationTokenSource()
        let workerTask =
            Task.Factory.StartNew(Func<Task>(fun () ->
                task {
                    try
                        do! innerLoop cts.Token
                    with
                    | :? OperationCanceledException -> ()
                    | exn ->
                        Console.Error.WriteLine $"[FIO] {workerName} faulted: {exn.Message}"
                        raise exn
                } :> Task),
            CancellationToken.None,
            TaskCreationOptions.LongRunning,
            TaskScheduler.Default
            ).Unwrap()

        struct (cts, workerTask)

module internal WorkerBuilders =

    let inline buildPairedWorkers
        (blockingCount: int)
        (evaluationCount: int)
        ([<InlineIfLambda>] blockingFactory: int -> 'T1)
        ([<InlineIfLambda>] evaluationFactory: int -> 'T1 -> 'T2)
        : struct ('T1 list * 'T2 list) =
        let blockingWorkers = List.init blockingCount blockingFactory
        let blockingWorkerCount = blockingWorkers.Length

        let evaluationWorkers =
            List.init evaluationCount (fun i -> evaluationFactory i blockingWorkers.[i % blockingWorkerCount])

        struct (blockingWorkers, evaluationWorkers)
