namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

[<RequireQualifiedAccess>]
module internal Bang =

    type private Actor =
        {
            Channel: Channel<int>
        }

    // Repeatedly sends a fixed message to the shared receiver's channel.
    let private sendingActorEffect actor roundCount message =
        fio {
            for _ in 1..roundCount do
                do! actor.Channel.Write(message).Unit()
        }

    // Reads the expected total number of messages from the channel.
    let private receivingActorEffect actor totalMessages =
        fio {
            for _ in 1..totalMessages do
                do! actor.Channel.Read().Unit()
        }

    // Builds the requested number of sending actors, all targeting one receiver.
    let private createSendingActors receivingActorChannel actorCount =
        [ for _ in 1..actorCount -> { Channel = receivingActorChannel } ]

    // Builds the Bang workload: many senders flooding a single receiver in parallel.
    let effect actorCount roundCount : FIO<unit, exn> =
        fio {
            let receivingActor = { Channel = Channel<int>() }
            let sendingActors = createSendingActors receivingActor.Channel actorCount
            let receiverEffect = receivingActorEffect receivingActor (actorCount * roundCount)

            let sendingEffects =
                sendingActors
                |> List.mapi (fun index actor ->
                    sendingActorEffect actor roundCount (index + 1))

            do! FIO.collectAllParDiscard (receiverEffect :: sendingEffects)
        }

// Measures many sending actors flooding a single receiver over one channel.
[<MemoryDiagnoser>]
[<RankColumn>]
type BangBenchmark() =
    inherit FIOBenchmark()

    member _.ActorCounts =
        RuntimeParam.intParams "FIO_BENCH_BANG_ACTORS" [| 50; 200 |]

    member _.RoundCounts =
        RuntimeParam.intParams "FIO_BENCH_BANG_ROUNDS" [| 1_000; 5_000 |]

    [<ParamsSource("ActorCounts")>]
    member val ActorCount = 0 with get, set

    [<ParamsSource("RoundCounts")>]
    member val RoundCount = 0 with get, set

    override this.CreateEffect () =
        Bang.effect this.ActorCount this.RoundCount
