[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Threadring

open FIO.DSL

type private Actor =
    {
        SendChannel: Channel<int>
        ReceiveChannel: Channel<int>
    }

let private actorEffect actor isLastActor roundCount =
    fio {
        for round in 1..roundCount do
            let! receivedMessage = actor.ReceiveChannel.Read()
            if not (isLastActor && round = roundCount) then
                do! actor.SendChannel.Write(receivedMessage + 1).Unit()
    }

let private threadringEffect actors roundCount =
    let lastIndex = List.length actors - 1
    FIO.forEachParDiscard (List.indexed actors) (fun (index, actor) ->
        actorEffect actor (index = lastIndex) roundCount)

let private createActors channels =
    let allChannels = List.toArray channels
    let count = allChannels.Length
    channels |> List.mapi (fun index channel ->
        let receiveIndex =
            if index = 0 then count - 1
            else index - 1
        {
            SendChannel = channel
            ReceiveChannel = allChannels[receiveIndex]
        })

let effect actorCount roundCount : FIO<unit, exn> =
    fio {
        let channels = [ for _ in 1..actorCount -> Channel<int>() ]
        let actors = createActors channels
        do! actors.Head.ReceiveChannel.Write(0).Unit()
        do! threadringEffect actors roundCount
    }
