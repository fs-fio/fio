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
let rec private sendPingsEff (actor, roundCount, msg) : FIO<unit, exn> =
    fio {
        for chan in actor.SendingChans do
            do! chan.Write(Ping(msg, actor.PongReceiveChan)).Unit()

        return! receivePingsEff (actor, roundCount, actor.SendingChans.Length, msg)
    }

and private receivePingsEff (actor, roundCount, receiveCount, msg) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PingReceiveChan.Read() with
            | Ping(ping, replyChan) -> do! replyChan.Write(Pong(ping + 1)).Unit()
            | _ -> return! FIO.fail (InvalidOperationException "receivePingsEff: Received pong when ping was expected!")

        return! receivePongsEff (actor, roundCount, actor.SendingChans.Length, msg)
    }

and private receivePongsEff (actor, roundCount, receiveCount, msg) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PongReceiveChan.Read() with
            | Pong _ -> ()
            | _ -> return! FIO.fail (InvalidOperationException "receivePongsEff: Received ping when pong was expected!")

        if roundCount > 0 then
            return! sendPingsEff (actor, roundCount - 1, msg)
    }

let private actorEff (actor, msg, roundCount, startChan: Channel<int>) : FIO<unit, exn> =
    fio {
        do! startChan.Read().Unit()
        return! sendPingsEff (actor, roundCount - 1, msg)
    }

let private createActors actorCount =
    let baseActors =
        [ for _ in 1..actorCount ->
              {
                  PingReceiveChan = Channel<Message>()
                  PongReceiveChan = Channel<Message>()
                  SendingChans = []
              } ]

    let pingChans = baseActors |> List.map _.PingReceiveChan |> List.toArray

    baseActors
    |> List.mapi (fun i actor ->
        let sendingChans = [ for j in 0 .. actorCount - 1 do if j <> i then yield pingChans[j] ]
        { actor with SendingChans = sendingChans })

let private bigEff (actors: Actor list, roundCount, msg, startChan) : FIO<unit, exn> =
    let headEff = actorEff (actors.Head, msg, roundCount, startChan)

    actors.Tail
    |> List.indexed
    |> List.fold (fun acc (i, actor) -> actorEff (actor, msg + (i + 1) * 10, roundCount, startChan) <&&> acc) headEff

let effect (actorCount: int) (roundCount: int) : FIO<unit, exn> =
    fio {
        let startChan = Channel<int>()
        let actors = createActors actorCount

        for _ in 1..actorCount do
            do! startChan.Write(0).Unit()

        do! bigEff (actors, roundCount, 0, startChan)
    }
