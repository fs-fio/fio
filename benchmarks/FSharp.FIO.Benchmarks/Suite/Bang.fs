(*****************************************************)
(* Bang benchmark                                    *)
(* Measures: Many-to-One message passing             *)
(* A Scalability Benchmark Suite for Erlang/OTP      *)
(* (https://dl.acm.org/doi/10.1145/2364489.2364495I) *)
(*****************************************************)

module private FSharp.FIO.Benchmarks.Suite.Bang

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO
#endif
open FSharp.FIO.Benchmarks.Tools.Timer

open System

type private Actor = 
    { Name: string; 
      Chan: int channel }

let private sendingActorEff (actor, roundCount, msg, timerChan: TimerMessage<int> channel, startChan: int channel) =
    fio {
        do! timerChan.Send(Start).Unit()
        do! startChan.Receive().Unit()
        
        for _ in 1..roundCount do
            do! actor.Chan.Send(msg).Unit()
            #if DEBUG
            do! Console.PrintLine $"[DEBUG]: %s{actor.Name} sent: %i{msg}"
            #endif
    }

let private receivingActorEff (actor, roundCount, timerChan: TimerMessage<int> channel, startChan: int channel) =
    fio {
        do! timerChan.Send(Start).Unit()
        do! startChan.Receive().Unit()
        
        for _ in 1..roundCount do
            let! msg = actor.Chan.Receive()
            #if DEBUG
            do! Console.PrintLine $"[DEBUG]: %s{actor.Name} received: %i{msg}"
            #endif
            return ()
            
        do! timerChan.Send(Stop).Unit()
    }

let private bangEff (receivingActor, sendingActors: Actor list, actorCount, roundCount, msg, timerChan, startChan) =
    fio {
        let mutable currentMsg = msg
        let mutable currentEff = receivingActorEff(receivingActor, actorCount * roundCount, timerChan, startChan)
        
        for actor in sendingActors do
            currentEff <- sendingActorEff(actor, roundCount, currentMsg, timerChan, startChan)
                          <&&> currentEff
            currentMsg <- currentMsg + 1
        
        return! currentEff
    }

let private createSendingActors (receiveActorChan, actorCount) =
    List.map (fun index ->
            { Name = $"Actor-{index}"
              Chan = receiveActorChan })
        [ 1..actorCount ]

let bangBenchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | BangConfig(ac, rc) -> FIO.Succeed(ac, rc)
            | _ -> FIO.Fail(ArgumentException("Bang benchmark failed: Requires a BangConfig", nameof config))
        
        if actorCount < 1 then
            return! FIO.Fail(ArgumentException($"Bang benchmark failed: At least 1 actor should be specified. actorCount = %i{actorCount}", nameof actorCount))
        
        if roundCount < 1 then
            return! FIO.Fail(ArgumentException($"Bang benchmark failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount))
        
        let timerChan = Channel<TimerMessage<int>>()
        let startChan = Channel<int>()

        let receivingActor =
            { Name = "Actor-0"
              Chan = Channel<int>() }

        let sendingActors = createSendingActors(receivingActor.Chan, actorCount)
        let! timerFiber = TimerEff(actorCount + 1, actorCount + 1, 1, timerChan).Fork()
        do! timerChan.Send(MsgChannel startChan).Unit()
        do! bangEff(receivingActor, sendingActors, actorCount, roundCount, 1, timerChan, startChan)
        return! timerFiber.Join()
    }
