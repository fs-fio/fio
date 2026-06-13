[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Threadring

open FIO.DSL

type private Actor = { SendChan: Channel<int>; ReceiveChan: Channel<int> }

let private actorEff (actor, isLastActor, roundCount) =
    fio {
        for round in 1..roundCount do
            let! receivedMsg = actor.ReceiveChan.Read()

            if not (isLastActor && round = roundCount) then
                do! actor.SendChan.Write(receivedMsg + 1).Unit()
    }

let private threadringEff (actors: Actor list, roundCount: int) =
    let headEff = actorEff (actors.Head, false, roundCount)

    actors.Tail
    |> List.indexed
    |> List.fold
        (fun acc (index, actor) ->
            let isLastActor = index + 2 = actors.Length
            actorEff (actor, isLastActor, roundCount) <&&> acc)
        headEff

let private createActors (chans: Channel<int> list) =
    let allChans = List.toArray chans
    let count = allChans.Length

    chans
    |> List.mapi (fun index chan ->
        let receiveIndex = if index = 0 then count - 1 else index - 1
        { SendChan = chan; ReceiveChan = allChans[receiveIndex] })

let effect (actorCount: int) (roundCount: int) : FIO<unit, exn> =
    fio {
        let chans = [ for _ in 1..actorCount -> Channel<int>() ]
        let actors = createActors chans

        do! actors.Head.ReceiveChan.Write(0).Unit()
        do! threadringEff (actors, roundCount)
    }
