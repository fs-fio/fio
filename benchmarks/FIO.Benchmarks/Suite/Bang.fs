(*****************************************************)
(* Bang benchmark                                    *)
(* Measures: Many-to-One message passing             *)
(* A Scalability Benchmark Suite for Erlang/OTP      *)
(* (https://dl.acm.org/doi/10.1145/2364489.2364495I) *)
(*****************************************************)

/// <summary>
/// Bang benchmark measuring many-to-one message passing scalability.
/// </summary>
[<RequireQualifiedAccess>]
module private FIO.Benchmarks.Suite.Bang

open FIO.DSL
#if DEBUG
open FIO.Console
#endif
open FIO.Benchmarks.Tools.Timer

open System

/// <summary>
/// Actor with a single communication channel.
/// </summary>
type private Actor =
    {
        /// <summary>Channel for sending/receiving messages.</summary>
        Chan: Channel<int>
#if DEBUG
        /// <summary>Actor name for debugging.</summary>
        Name: string
#endif
    }

/// <summary>
/// Sending actor effect that sends messages to the receiving actor.
/// </summary>
/// <param name="actor">Sending actor instance.</param>
/// <param name="roundCount">Number of messages to send.</param>
/// <param name="msg">Message value to send.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
let private sendingActorEff (actor, roundCount, msg, timerChan: Channel<TimerMessage<int>>, startChan: Channel<int>) =
    fio {
        do! timerChan.Send(Start).Unit()
        do! startChan.Receive().Unit()

        for _ in 1..roundCount do
            do! actor.Chan.Send(msg).Unit()
#if DEBUG
            do! Console.printLine ($"[DEBUG]: %s{actor.Name} sent: %i{msg}", id)
#endif
    }

/// <summary>
/// Receiving actor effect that receives all messages from sending actors.
/// </summary>
/// <param name="actor">Receiving actor instance.</param>
/// <param name="roundCount">Total number of messages to receive.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
let private receivingActorEff (actor, roundCount, timerChan: Channel<TimerMessage<int>>, startChan: Channel<int>) =
    fio {
        do! timerChan.Send(Start).Unit()
        do! startChan.Receive().Unit()

        for _ in 1..roundCount do
            let! _msg = actor.Chan.Receive()
#if DEBUG
            do! Console.printLine ($"[DEBUG]: %s{actor.Name} received: %i{_msg}", id)
#endif
            ()

        do! timerChan.Send(Stop).Unit()
    }

/// <summary>
/// Composes the receiving actor and all sending actors to run concurrently.
/// </summary>
/// <param name="receivingActor">The single receiving actor.</param>
/// <param name="sendingActors">List of sending actors.</param>
/// <param name="actorCount">Number of sending actors.</param>
/// <param name="roundCount">Messages per sending actor.</param>
/// <param name="msg">Initial message value.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
let private bangEff (receivingActor, sendingActors: Actor list, actorCount, roundCount, msg, timerChan, startChan) =
    let receiverEff =
        receivingActorEff (receivingActor, actorCount * roundCount, timerChan, startChan)

    sendingActors
    |> List.indexed
    |> List.fold
        (fun acc (i, actor) -> sendingActorEff (actor, roundCount, msg + i, timerChan, startChan) <&&> acc)
        receiverEff

/// <summary>
/// Creates sending actors that all share the receiving actor's channel.
/// </summary>
/// <param name="receiveActorChan">Channel of the receiving actor.</param>
/// <param name="actorCount">Number of sending actors to create.</param>
/// <returns>List of sending actors.</returns>
let private createSendingActors (receiveActorChan, actorCount) =
    List.map
        (fun index ->
            {
                Chan = receiveActorChan
#if DEBUG
                Name = $"Actor-{index}"
#endif
            })
        [ 1..actorCount ]

/// <summary>
/// Creates and runs the bang benchmark, returning execution time in milliseconds.
/// </summary>
/// <param name="config">Bang benchmark configuration.</param>
/// <returns>Execution time in milliseconds.</returns>
let benchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | BangConfig(ac, rc) -> FIO.succeed (ac, rc)
            | _ ->
                FIO.fail (
                    ArgumentException("Bang benchmark initialization failed: Requires a BangConfig", nameof config)
                )

        if actorCount < 1 then
            return!
                FIO.fail (
                    ArgumentException(
                        $"Bang benchmark initialization failed: At least 1 actor should be specified. actorCount = %i{actorCount}",
                        nameof actorCount
                    )
                )

        if roundCount < 1 then
            return!
                FIO.fail (
                    ArgumentException(
                        $"Bang benchmark initialization failed: At least 1 round should be specified. roundCount = %i{roundCount}",
                        nameof roundCount
                    )
                )

        let timerChan = Channel<TimerMessage<int>>()
        let startChan = Channel<int>()

        let receivingActor =
            {
                Chan = Channel<int>()
#if DEBUG
                Name = "Actor-0"
#endif
            }

        let sendingActors = createSendingActors (receivingActor.Chan, actorCount)
        let! timerFiber = timerEff(actorCount + 1, actorCount + 1, 1, timerChan).Fork()
        do! timerChan.Send(MsgChannel startChan).Unit()
        do! bangEff (receivingActor, sendingActors, actorCount, roundCount, 1, timerChan, startChan)
        return! timerFiber.Join()
    }
