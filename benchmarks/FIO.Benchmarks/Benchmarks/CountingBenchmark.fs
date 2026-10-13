namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

open System

[<RequireQualifiedAccess>]
module internal Counting =

    type private Message =
        | Increment
        | RetrieveChannel of Channel<int>

    // The counter actor: tallies increments and reports the total when asked.
    let private counterEffect (mailbox: Channel<Message>) =
        let rec loop count =
            fio {
                match! mailbox.Read() with
                | Increment ->
                    return! loop (count + 1)
                | RetrieveChannel replyChannel ->
                    do! replyChannel.Write(count).Unit()
            }
        loop 0

    // Sends a stream of increments, then verifies the counter's reported total.
    let private producerEffect (mailbox: Channel<Message>) replyChannel messageCount =
        fio {
            for _ in 1..messageCount do
                do! mailbox.Write(Increment).Unit()

            do! mailbox.Write(RetrieveChannel replyChannel).Unit()
            let! final = replyChannel.Read()

            if final <> messageCount then
                return! FIO.fail (InvalidOperationException $"Counting: expected {messageCount}, got {final}" :> exn)
        }

    // Builds the Counting workload: one producer feeding a counter actor.
    let effect messageCount =
        fio {
            let mailbox = Channel<Message>()
            let replyChannel = Channel<int>()

            do! FIO.collectAllParDiscard
                    [ counterEffect mailbox; producerEffect mailbox replyChannel messageCount ]
        }

// Measures throughput of a counter actor tallying a stream of increment messages.
[<MemoryDiagnoser>]
[<RankColumn>]
type CountingBenchmark() =
    inherit FIOBenchmark()

    member _.MessageCounts =
        RuntimeParam.intParams "FIO_BENCH_COUNTING_MESSAGES" [| 1_000_000 |]

    [<ParamsSource("MessageCounts")>]
    member val MessageCount = 0 with get, set

    override this.CreateEffect () =
        Counting.effect this.MessageCount
