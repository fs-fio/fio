(****************************************************************************)
(* Big benchmark                                                            *)
(* Measures: Contention on mailbox; Many-to-Many message passing            *)
(* Savina benchmark #7                                                      *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf) *)
(****************************************************************************)

/// <summary>
/// Big benchmark measuring mailbox contention with many-to-many message passing.
/// </summary>
[<RequireQualifiedAccess>]
module private FSharp.FIO.Benchmarks.Suite.Big

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO.Console
#endif
open FSharp.FIO.Benchmarks.Tools.Timer

open System

/// <summary>
/// Message type for ping-pong communication between actors.
/// </summary>
type private Message =
    /// <summary>Ping message with value and reply channel.</summary>
    | Ping of int * Channel<Message>
    /// <summary>Pong response message with value.</summary>
    | Pong of int

/// <summary>
/// Actor with separate ping/pong receive channels and a list of channels to send to.
/// </summary>
type private Actor =
    { /// <summary>Channel for receiving ping messages.</summary>
      PingReceiveChan: Channel<Message>
      /// <summary>Channel for receiving pong messages.</summary>
      PongReceiveChan: Channel<Message>
      /// <summary>List of channels to send pings to.</summary>
      SendingChans: Channel<Message> list
#if DEBUG
      /// <summary>Actor name for debugging.</summary>
      Name: string
#endif
    }

/// <summary>
/// Sends pings to all other actors in the network.
/// </summary>
/// <param name="actor">Actor sending the pings.</param>
/// <param name="roundCount">Remaining rounds.</param>
/// <param name="ping">Ping value to send.</param>
/// <param name="chans">Channels to send pings to.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
[<TailCall>]
let rec private sendPingsEff (actor, roundCount, ping, chans: Channel<Message> list, timerChan) : FIO<unit, exn> =
    fio {
        for chan in chans do
            do! chan.Send(Ping(ping, actor.PongReceiveChan)).Unit()
            #if DEBUG
            do! Console.printLineExn $"DEBUG: %s{actor.Name} sent ping: %i{ping}"
            #endif
        
        return! receivePingsEff(actor, roundCount, actor.SendingChans.Length, ping, timerChan)
    }

/// <summary>
/// Receives pings from all other actors and responds with pongs.
/// </summary>
/// <param name="actor">Actor receiving the pings.</param>
/// <param name="rounds">Remaining rounds.</param>
/// <param name="receiveCount">Number of pings to receive.</param>
/// <param name="msg">Current message value.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
and private receivePingsEff (actor, rounds, receiveCount, msg, timerChan) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PingReceiveChan.Receive() with
            | Ping (ping, replyChan) ->
                #if DEBUG
                do! Console.printLineExn $"DEBUG: %s{actor.Name} received ping: %i{ping}"
                #endif
                match! replyChan.Send(Pong(ping + 1)) with
                | Pong _pong ->
                    #if DEBUG
                    do! Console.printLineExn $"DEBUG: %s{actor.Name} sent pong: %i{_pong}"
                    #endif
                    ()
                | Ping _ ->
                    return! FIO.fail(InvalidOperationException "receivePingsEff: Received ping when pong was expected!")
            | _ ->
                return! FIO.fail(InvalidOperationException "receivePingsEff: Received pong when ping was expected!")
                
        return! receivePongsEff(actor, rounds, actor.SendingChans.Length, msg, timerChan)
    }

/// <summary>
/// Receives pongs from all other actors, then starts next round or stops.
/// </summary>
/// <param name="actor">Actor receiving the pongs.</param>
/// <param name="roundCount">Remaining rounds.</param>
/// <param name="receiveCount">Number of pongs to receive.</param>
/// <param name="msg">Current message value.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
and private receivePongsEff (actor, roundCount, receiveCount, msg, timerChan: Channel<TimerMessage<int>>) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PongReceiveChan.Receive() with
            | Pong _pong ->
                #if DEBUG
                do! Console.printLineExn $"DEBUG: %s{actor.Name} received pong: %i{_pong}"
                #endif
                ()
            | _ ->
                return! FIO.fail(InvalidOperationException "receivePongsEff: Received ping when pong was expected!")
        
        if roundCount <= 0 then
            do! timerChan.Send(Stop).Unit()
        else
            return! sendPingsEff(actor, roundCount - 1, msg, actor.SendingChans, timerChan)
    }

/// <summary>
/// Actor effect that waits for start signal, then begins the ping-pong cycle.
/// </summary>
/// <param name="actor">Actor instance.</param>
/// <param name="msg">Initial message value.</param>
/// <param name="roundCount">Number of rounds to execute.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
let private actorEff (actor, msg, roundCount, timerChan: Channel<TimerMessage<int>>, startChan: Channel<int>) : FIO<unit, exn> =
    fio {
        do! timerChan.Send(Start).Unit()
        do! startChan.Receive().Unit()
        return! sendPingsEff(actor, roundCount - 1, msg, actor.SendingChans, timerChan)
    }

/// <summary>
/// Creates actors with only receive channels (no send channels yet).
/// </summary>
/// <param name="actorCount">Number of actors to create.</param>
/// <param name="acc">Accumulated actors.</param>
/// <returns>List of actors with receive channels only.</returns>
[<TailCall>]
let rec private receivingActors (actorCount, acc) =
    match actorCount with
    | 0 -> acc
    | count ->
        let actor =
            { PingReceiveChan = Channel<Message>()
              PongReceiveChan = Channel<Message>()
              SendingChans = []
#if DEBUG
              Name = $"Actor-{count - 1}"
#endif
            }
        receivingActors(count - 1, acc @ [actor])

/// <summary>
/// Assigns send channels to each actor pointing to all other actors.
/// </summary>
/// <param name="receivingActors">Remaining actors to process.</param>
/// <param name="prevReceivingActors">Previously processed actors.</param>
/// <param name="acc">Accumulated actors with send channels.</param>
/// <returns>List of fully connected actors.</returns>
[<TailCall>]
let rec private createActorsHelper (receivingActors, prevReceivingActors, acc) =
    match receivingActors with
    | [] -> acc
    | ac :: acs ->
        let otherActors = prevReceivingActors @ acs
        let chansSend = List.map _.PingReceiveChan otherActors

        let actor =
            { PingReceiveChan = ac.PingReceiveChan
              PongReceiveChan = ac.PongReceiveChan
              SendingChans = chansSend
#if DEBUG
              Name = ac.Name
#endif
            }

        createActorsHelper(acs, prevReceivingActors @ [ac], actor :: acc)

/// <summary>
/// Creates a fully connected network of actors.
/// </summary>
/// <param name="actorCount">Number of actors to create.</param>
/// <returns>List of fully connected actors.</returns>
let private createActors actorCount =
    let receivingActors = receivingActors(actorCount, [])
    createActorsHelper(receivingActors, [], [])

/// <summary>
/// Composes all actor effects to run concurrently.
/// </summary>
/// <param name="actors">List of actors in the network.</param>
/// <param name="roundCount">Number of rounds to execute.</param>
/// <param name="msg">Initial message value.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
let private bigEff (actors: Actor list, roundCount, msg, timerChan, startChan) : FIO<unit, exn> =
    let headEff = actorEff(actors.Head, msg, roundCount, timerChan, startChan)
    actors.Tail
    |> List.indexed
    |> List.fold (fun acc (i, actor) ->
        actorEff(actor, msg + (i + 1) * 10, roundCount, timerChan, startChan) <&&> acc)
        headEff

/// <summary>
/// Creates and runs the big benchmark, returning execution time in milliseconds.
/// </summary>
/// <param name="config">Big benchmark configuration.</param>
/// <returns>Execution time in milliseconds.</returns>
let benchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | BigConfig(ac, rc) -> FIO.succeed(ac, rc)
            | _ -> FIO.fail(ArgumentException("Big benchmark initialization failed: Requires a BigConfig", nameof config))
        
        if actorCount < 2 then
            return! FIO.fail(ArgumentException($"Big benchmark initialization failed: At least 2 actors should be specified. actorCount = %i{actorCount}", nameof actorCount))
        
        if roundCount < 1 then
            return! FIO.fail(ArgumentException($"Big benchmark initialization failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount))
        
        let timerChan = Channel<TimerMessage<int>>()
        let startChan = Channel<int>()
        let actors = createActors actorCount

        let! timerFiber = timerEff(actorCount, actorCount, actorCount, timerChan).Fork()
        do! timerChan.Send(MsgChannel startChan).Unit()
        do! bigEff(actors, roundCount, 0, timerChan, startChan)
        return! timerFiber.Join()
    }
