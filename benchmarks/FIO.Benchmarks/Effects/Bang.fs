/// Bang benchmark — measures many-to-one message passing scalability.
[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Bang

open FIO.DSL

type private Actor = { Chan: Channel<int> }

let private sendingActorEff (actor, roundCount, msg, startChan: Channel<int>) =
    fio {
        do! startChan.Receive().Unit()

        for _ in 1..roundCount do
            do! actor.Chan.Send(msg).Unit()
    }

let private receivingActorEff (actor, totalMessages, startChan: Channel<int>) =
    fio {
        do! startChan.Receive().Unit()

        for _ in 1..totalMessages do
            let! _ = actor.Chan.Receive()
            ()
    }

let private bangEff (receivingActor, sendingActors: Actor list, actorCount, roundCount, msg, startChan) =
    let receiverEff =
        receivingActorEff (receivingActor, actorCount * roundCount, startChan)

    sendingActors
    |> List.indexed
    |> List.fold (fun acc (i, actor) -> sendingActorEff (actor, roundCount, msg + i, startChan) <&&> acc) receiverEff

let private createSendingActors (receiveActorChan, actorCount) =
    [ for _ in 1..actorCount -> { Chan = receiveActorChan } ]

let effect (actorCount: int, roundCount: int) : FIO<unit, exn> =
    fio {
        let startChan = Channel<int>()
        let receivingActor = { Chan = Channel<int>() }
        let sendingActors = createSendingActors (receivingActor.Chan, actorCount)

        // Send start signal to receiver + all senders
        for _ in 1 .. (actorCount + 1) do
            do! startChan.Send(0).Unit()

        do! bangEff (receivingActor, sendingActors, actorCount, roundCount, 1, startChan)
    }
