[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Counting

open FIO.DSL

open System

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
let effect messageCount : FIO<unit, exn> =
    fio {
        let mailbox = Channel<Message>()
        let replyChannel = Channel<int>()

        do! FIO.collectAllParDiscard
                [ counterEffect mailbox; producerEffect mailbox replyChannel messageCount ]
    }
