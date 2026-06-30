[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Big

open FIO.DSL

open System

type private Message =
    | Ping of int * Channel<Message>
    | Pong of int

type private Actor =
    {
        PingReceiveChannel: Channel<Message>
        PongReceiveChannel: Channel<Message>
        SendingChannels: Channel<Message> list
        SendingCount: int
    }

// Sends a ping to every peer, then awaits their pongs.
let rec private sendPingsEffect actor roundCount message =
    fio {
        for channel in actor.SendingChannels do
            do! channel.Write(Ping(message, actor.PongReceiveChannel)).Unit()
        return! receivePingsEffect actor roundCount actor.SendingCount message
    }

// Receives pings from peers and replies to each with a pong.
and private receivePingsEffect actor roundCount receiveCount message =
    fio {
        for _ in 1..receiveCount do
            match! actor.PingReceiveChannel.Read() with
            | Ping(ping, replyChan) ->
                do! replyChan.Write(Pong(ping + 1)).Unit()
            | _ ->
                return! FIO.fail (InvalidOperationException "Big: received pong when ping was expected" :> exn)

        return! receivePongsEffect actor roundCount actor.SendingCount message
    }

// Receives the expected pongs, then starts the next round if any remain.
and private receivePongsEffect actor roundCount receiveCount message =
    fio {
        for _ in 1..receiveCount do
            match! actor.PongReceiveChannel.Read() with
            | Pong _ ->
                ()
            | _ ->
                return! FIO.fail (InvalidOperationException "Big: received ping when pong was expected" :> exn)

        if roundCount > 0 then
            return! sendPingsEffect actor (roundCount - 1) message
    }

// Runs one actor through all of its ping/pong rounds.
let private actorEffect actor message roundCount =
    sendPingsEffect actor (roundCount - 1) message

// Builds a fully-connected mesh of actors wired to each other's ping channels.
let private createActors actorCount =
    let baseActors =
        [ for _ in 1..actorCount ->
              {
                  PingReceiveChannel = Channel<Message>()
                  PongReceiveChannel = Channel<Message>()
                  SendingChannels = []
                  SendingCount = 0
              } ]

    let pingChannels =
        baseActors
        |> List.map _.PingReceiveChannel
        |> List.toArray

    baseActors
    |> List.mapi (fun index actor ->
        let sendingChannels = [ for targetIndex in 0 .. actorCount - 1 do if targetIndex <> index then yield pingChannels[targetIndex] ]
        { actor with SendingChannels = sendingChannels; SendingCount = List.length sendingChannels })

// Builds the Big workload: all-to-all ping/pong across a mesh of actors.
let effect actorCount roundCount : FIO<unit, exn> =
    fio {
        let actors = createActors actorCount
        do! FIO.forEachParDiscard (List.indexed actors) <| fun (index, actor) ->
                actorEffect actor (index * 10) roundCount
    }
