[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Bang

open FIO.DSL

type private Actor = { Chan: Channel<int> }

let private sendingActorEff (actor, roundCount, msg, startChan: Channel<int>) =
    fio {
        do! startChan.Read().Unit()

        for _ in 1..roundCount do
            do! actor.Chan.Write(msg).Unit()
    }

let private receivingActorEff (actor, totalMessages, startChan: Channel<int>) =
    fio {
        do! startChan.Read().Unit()

        for _ in 1..totalMessages do
            let! _ = actor.Chan.Read()
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

let effect (actorCount: int) (roundCount: int) : FIO<unit, exn> =
    fio {
        let startChan = Channel<int>()
        let receivingActor = { Chan = Channel<int>() }
        let sendingActors = createSendingActors (receivingActor.Chan, actorCount)

        for _ in 1 .. (actorCount + 1) do
            do! startChan.Write(0).Unit()

        do! bangEff (receivingActor, sendingActors, actorCount, roundCount, 1, startChan)
    }
