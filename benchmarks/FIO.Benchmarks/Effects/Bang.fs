[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Bang

open FIO.DSL

type private Actor =
    {
        Channel: Channel<int> 
    }

let private sendingActorEffect actor roundCount message =
    fio {
        for _ in 1..roundCount do
            do! actor.Channel.Write(message).Unit()
    }

let private receivingActorEffect actor totalMessages =
    fio {
        for _ in 1..totalMessages do
            do! actor.Channel.Read().Unit()
    }

let private createSendingActors receivingActorChannel actorCount =
    [ for _ in 1..actorCount -> { Channel = receivingActorChannel } ]

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
