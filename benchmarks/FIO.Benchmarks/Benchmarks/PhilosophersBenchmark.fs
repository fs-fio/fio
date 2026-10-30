namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

open System.Collections.Generic

[<RequireQualifiedAccess>]
module internal Philosophers =

    type private ArbMessage =
        | Hungry of id: int * grantedChannel: Channel<unit>
        | Done of id: int

    // The arbitrator actor: grants forks to hungry philosophers and releases them when done.
    let private arbitratorEffect (arbChannel: Channel<ArbMessage>) philosopherCount totalRounds =
        let forks = Array.create philosopherCount true
        let pending = Queue<int * Channel<unit>>()
        let mutable doneCount = 0

        let leftFork id =
            id
        let rightFork id =
            (id + 1) % philosopherCount
        let canEat id =
            forks[leftFork id] && forks[rightFork id]
        let takeForks id =
            forks[leftFork id] <- false
            forks[rightFork id] <- false
        let releaseForks id =
            forks[leftFork id] <- true
            forks[rightFork id] <- true

        let tryGrantPendingEffect () =
            fio {
                let scanCount = pending.Count
                for _ in 1..scanCount do
                    let id, grantedChannel = pending.Dequeue()

                    if canEat id then
                        takeForks id
                        do! grantedChannel.Write(()).Unit()
                    else
                        pending.Enqueue(id, grantedChannel)
            }

        let rec loop () =
            fio {
                if doneCount < totalRounds then
                    match! arbChannel.Read() with
                    | Hungry(id, grantedChannel) ->
                        if canEat id then
                            takeForks id
                            do! grantedChannel.Write(()).Unit()
                        else
                            pending.Enqueue(id, grantedChannel)
                    | Done id ->
                        doneCount <- doneCount + 1
                        releaseForks id
                        do! tryGrantPendingEffect ()
                    return! loop ()
            }

        loop ()

    // A philosopher that requests forks, eats, and releases them each round.
    let private philosopherEffect (arbChannel: Channel<ArbMessage>) id roundCount =
        fio {
            let grantedChannel = Channel<unit>()
            for _ in 1..roundCount do
                do! arbChannel.Write(Hungry(id, grantedChannel)).Unit()
                do! grantedChannel.Read().Unit()
                do! arbChannel.Write(Done id).Unit()
        }

    // Builds the dining-philosophers workload coordinated through one arbitrator.
    let effect philosopherCount roundCount =
        fio {
            let arbChannel = Channel<ArbMessage>()
            let totalRounds = philosopherCount * roundCount

            let philosophers =
                [ for id in 0 .. philosopherCount - 1 ->
                    philosopherEffect arbChannel id roundCount ]

            do! FIO.collectAllParDiscard (arbitratorEffect arbChannel philosopherCount totalRounds :: philosophers)
        }

// Measures the dining-philosophers problem coordinated through an arbitrator actor.
[<MemoryDiagnoser>]
[<RankColumn>]
type PhilosophersBenchmark() =
    inherit FIOBenchmark()

    member _.PhilosopherCounts =
        RuntimeParam.intParams "FIO_BENCH_PHILOSOPHERS_COUNT" [| 20 |]

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_PHILOSOPHERS_ROUNDS" [| 10_000 |]

    [<ParamsSource("PhilosopherCounts")>]
    member val PhilosopherCount = 0 with get, set

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    override this.CreateEffect () =
        Philosophers.effect this.PhilosopherCount this.RoundCount
