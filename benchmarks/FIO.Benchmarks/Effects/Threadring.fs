/// Threadring benchmark — measures message passing and context switching in a ring topology.
[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Threadring

open FIO.DSL

type private Actor = { SendChan: Channel<int>; ReceiveChan: Channel<int> }

let private actorEff (actor, isLastActor, roundCount) =
    fio {
        for round in 1..roundCount do
            let! receivedMsg = actor.ReceiveChan.Receive()

            if not (isLastActor && round = roundCount) then
                do! actor.SendChan.Send(receivedMsg + 1).Unit()
    }

let private threadringEff (actors: Actor list, roundCount: int) =
    let headEff = actorEff (actors.Head, false, roundCount)

    actors.Tail
    |> List.indexed
    |> List.fold
        (fun acc (i, actor) ->
            let isLastActor = i + 2 = actors.Length
            actorEff (actor, isLastActor, roundCount) <&&> acc)
        headEff

[<TailCall>]
let rec private createActors (chans, allChans: Channel<int> list, index, acc) =
    match chans with
    | [] -> acc
    | chan' :: chans' ->
        let actor =
            {
                SendChan = chan'
                ReceiveChan =
                    if index - 1 < 0 then
                        allChans[allChans.Length - 1]
                    else
                        allChans[index - 1]
            }

        createActors (chans', allChans, index + 1, acc @ [ actor ])

let effect (actorCount: int, roundCount: int) : FIO<unit, exn> =
    fio {
        let chans = [ for _ in 1..actorCount -> Channel<int>() ]
        let actors = createActors (chans, chans, 0, [])

        // Send initial message to the first actor's receive channel
        do! actors.Head.ReceiveChan.Send(0).Unit()
        do! threadringEff (actors, roundCount)
    }
