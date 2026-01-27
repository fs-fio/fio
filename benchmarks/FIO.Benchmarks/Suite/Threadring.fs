(****************************************************************************)
(* Threadring benchmark                                                     *)
(* Measures: Message sending; Context switching between actors              *)
(* Savina benchmark #5                                                      *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf) *)
(****************************************************************************)

/// <summary>
/// Threadring benchmark measuring message passing and context switching in a ring topology.
/// </summary>
[<RequireQualifiedAccess>]
module private FIO.Benchmarks.Suite.Threadring

open FIO.DSL
#if DEBUG
open FIO.Console
#endif
open FIO.Benchmarks.Tools.Timer

open System

/// <summary>
/// Actor in the ring with send and receive channels to adjacent actors.
/// </summary>
type private Actor =
    { /// <summary>Channel for sending messages to the next actor.</summary>
      SendChan: Channel<int>
      /// <summary>Channel for receiving messages from the previous actor.</summary>
      ReceiveChan: Channel<int>
#if DEBUG
      /// <summary>Actor name for debugging.</summary>
      Name: string
#endif
    }

/// <summary>
/// Actor effect that receives a message, increments it, and forwards to the next actor.
/// </summary>
/// <param name="actor">Actor instance with channels.</param>
/// <param name="isLastActor">Whether this is the last actor in the ring.</param>
/// <param name="roundCount">Number of rounds to execute.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
let private actorEff (actor, isLastActor, roundCount, timerChan: Channel<TimerMessage<int>>) =
    fio {
        do! timerChan.Send(Start).Unit()
        
        for round in 1..roundCount do
            let! receivedMsg = actor.ReceiveChan.Receive()
            #if DEBUG
            do! Console.printLineExn $"[DEBUG]: %s{actor.Name} received: %i{receivedMsg}"
            #endif
            if not (isLastActor && round = roundCount) then
                // The last actor of the last round should not send a message
                let! _sentMsg = actor.SendChan.Send(receivedMsg + 1)
                #if DEBUG
                do! Console.printLineExn $"[DEBUG]: %s{actor.Name} sent: %i{_sentMsg}"
                #endif
                ()

        do! timerChan.Send(Stop).Unit()
    }

/// <summary>
/// Composes all actor effects in the ring to run concurrently.
/// </summary>
/// <param name="actors">List of actors in the ring.</param>
/// <param name="roundCount">Number of rounds to execute.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
let private threadringEff (actors: Actor list, roundCount: int, timerChan: Channel<TimerMessage<int>>) =
    fio {
        do! timerChan.Send(MsgChannel actors.Head.ReceiveChan).Unit()
        let headEff = actorEff(actors.Head, false, roundCount, timerChan)
        return!
            actors.Tail
            |> List.indexed
            |> List.fold (fun acc (i, actor) ->
                // +2 to compensate for the first actor and 0-indexed
                let isLastActor = i + 2 = actors.Length
                actorEff(actor, isLastActor, roundCount, timerChan) <&&> acc)
                headEff
    }

/// <summary>
/// Recursively creates actors with circular channel references forming a ring.
/// </summary>
/// <param name="chans">Remaining channels to assign.</param>
/// <param name="allChans">Complete list of channels for circular reference.</param>
/// <param name="index">Current actor index.</param>
/// <param name="acc">Accumulated actors.</param>
/// <returns>List of actors with circular channel connections.</returns>
[<TailCall>]
let rec private createActors (chans, allChans: Channel<int> list, index, acc) =
    match chans with
    | [] -> acc
    | chan'::chans' ->
        let actor =
            { SendChan = chan'
              ReceiveChan =
                match index with
                | index when index - 1 < 0 -> allChans.Item(List.length allChans - 1)
                | index -> allChans.Item(index - 1)
#if DEBUG
              Name = $"Actor-{index}"
#endif
            }
        createActors (chans', allChans, index + 1, acc @ [actor])

/// <summary>
/// Creates and runs the threadring benchmark, returning execution time in milliseconds.
/// </summary>
/// <param name="config">Threadring benchmark configuration.</param>
/// <returns>Execution time in milliseconds.</returns>
let benchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | ThreadringConfig(ac, rc) -> FIO.succeed(ac, rc)
            | _ -> FIO.fail(ArgumentException("Threadring benchmark initialization failed: Requires a ThreadringConfig", nameof config))
        
        if actorCount < 2 then
            return! FIO.fail(ArgumentException($"Threadring benchmark initialization failed: At least 2 actors should be specified. actorCount = %i{actorCount}", nameof actorCount))
        
        if roundCount < 1 then
            return! FIO.fail(ArgumentException($"Threadring benchmark initialization failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount))
        
        let chans = [for _ in 1..actorCount -> Channel<int>()]
        let timerChan = Channel<TimerMessage<int>>()
        let actors = createActors(chans, chans, 0, [])
        let! fiber = timerEff(actorCount, 1, actorCount, timerChan).Fork()
        do! threadringEff(actors, roundCount, timerChan)
        return! fiber.Join()
    }
