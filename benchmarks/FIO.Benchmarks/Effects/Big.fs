/// Big benchmark — measures mailbox contention with many-to-many message passing.
[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Big

open FIO.DSL

open System

type private Message =
    | Ping of int * Channel<Message>
    | Pong of int

type private Actor =
    {
        PingReceiveChan: Channel<Message>
        PongReceiveChan: Channel<Message>
        SendingChans: Channel<Message> list
    }

[<TailCall>]
let rec private sendPingsEff (actor, roundCount, ping) : FIO<unit, exn> =
    fio {
        for chan in actor.SendingChans do
            do! chan.Send(Ping(ping, actor.PongReceiveChan)).Unit()

        return! receivePingsEff (actor, roundCount, actor.SendingChans.Length, ping)
    }

and private receivePingsEff (actor, rounds, receiveCount, msg) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PingReceiveChan.Receive() with
            | Ping(ping, replyChan) ->
                match! replyChan.Send(Pong(ping + 1)) with
                | Pong _ -> ()
                | Ping _ ->
                    return!
                        FIO.fail (InvalidOperationException "receivePingsEff: Received ping when pong was expected!")
            | _ -> return! FIO.fail (InvalidOperationException "receivePingsEff: Received pong when ping was expected!")

        return! receivePongsEff (actor, rounds, actor.SendingChans.Length, msg)
    }

and private receivePongsEff (actor, roundCount, receiveCount, msg) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PongReceiveChan.Receive() with
            | Pong _ -> ()
            | _ -> return! FIO.fail (InvalidOperationException "receivePongsEff: Received ping when pong was expected!")

        if roundCount > 0 then
            return! sendPingsEff (actor, roundCount - 1, msg)
    }

let private actorEff (actor, msg, roundCount, startChan: Channel<int>) : FIO<unit, exn> =
    fio {
        do! startChan.Receive().Unit()
        return! sendPingsEff (actor, roundCount - 1, msg)
    }

[<TailCall>]
let rec private receivingActors (actorCount, acc) =
    match actorCount with
    | 0 -> acc
    | count ->
        let actor =
            {
                PingReceiveChan = Channel<Message>()
                PongReceiveChan = Channel<Message>()
                SendingChans = []
            }

        receivingActors (count - 1, acc @ [ actor ])

[<TailCall>]
let rec private createActorsHelper (receivingActors, prevReceivingActors, acc) =
    match receivingActors with
    | [] -> acc
    | ac :: acs ->
        let otherActors = prevReceivingActors @ acs
        let chansSend = List.map _.PingReceiveChan otherActors

        let actor =
            {
                PingReceiveChan = ac.PingReceiveChan
                PongReceiveChan = ac.PongReceiveChan
                SendingChans = chansSend
            }

        createActorsHelper (acs, prevReceivingActors @ [ ac ], actor :: acc)

let private createActors actorCount =
    let receiving = receivingActors (actorCount, [])
    createActorsHelper (receiving, [], [])

let private bigEff (actors: Actor list, roundCount, msg, startChan) : FIO<unit, exn> =
    let headEff = actorEff (actors.Head, msg, roundCount, startChan)

    actors.Tail
    |> List.indexed
    |> List.fold (fun acc (i, actor) -> actorEff (actor, msg + (i + 1) * 10, roundCount, startChan) <&&> acc) headEff

let effect (actorCount: int, roundCount: int) : FIO<unit, exn> =
    fio {
        let startChan = Channel<int>()
        let actors = createActors actorCount

        for _ in 1..actorCount do
            do! startChan.Send(0).Unit()

        do! bigEff (actors, roundCount, 0, startChan)
    }
