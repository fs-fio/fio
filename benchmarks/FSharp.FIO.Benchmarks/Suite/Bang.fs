(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(* ----------------------------------------------------------------------------------------- *)
(* Bang benchmark                                                                            *)
(* Measures: Many-to-One message passing                                                     *)
(* A Scalability Benchmark Suite for Erlang/OTP                                              *)
(* (https://dl.acm.org/doi/10.1145/2364489.2364495I)                                         *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Suite.Bang

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO.Lib.IO
#endif
open FSharp.FIO.Benchmarks.Tools.Timer

open System

type private Actor = 
    { Name: string; 
      Chan: int channel }

let private createSendingActor actor roundCount msg (timerChan: TimerMessage<int> channel) (startChan: int channel) =
    fio {
        do! (timerChan.Send Start).Unit
        do! startChan.Receive().Unit
        
        for _ in 1..roundCount do
            do! (actor.Chan.Send msg).Unit
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{actor.Name} sent: %i{msg}"
            #endif
    }

let private createReceiveActor actor roundCount (timerChan: TimerMessage<int> channel) (startChan: int channel) =
    fio {
        do! (timerChan.Send Start).Unit
        do! startChan.Receive().Unit
        
        for _ in 1..roundCount do
            let! msg = actor.Chan.Receive()
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{actor.Name} received: %i{msg}"
            #endif
            return ()
            
        do! (timerChan.Send Stop).Unit
    }

let private createBang receivingActor (sendingActors: Actor list) actorCount roundCount msg timerChan startChan =
    fio {
        let mutable currentMsg = msg
        let mutable currentEff = createReceiveActor receivingActor (actorCount * roundCount) timerChan startChan
        
        for sendingActor in sendingActors do
            currentEff <- (createSendingActor sendingActor roundCount currentMsg timerChan startChan
                          <&> currentEff).Unit
            currentMsg <- currentMsg + 1
        
        return! currentEff
    }

let private createSendingActors receiveActorChan actorCount =
    List.map (fun index ->
            { Name = $"Actor-{index}"
              Chan = receiveActorChan })
        [ 1..actorCount ]

let createBangBenchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | BangConfig (actorCount, roundCount) -> FIO.Succeed(actorCount, roundCount)
            | _ -> FIO.Fail <| ArgumentException("Bang benchmark requires a BangConfig!", nameof config)
        
        if actorCount < 1 then
            return! FIO.Fail <| ArgumentException($"Bang failed: At least 1 actor should be specified. actorCount = %i{actorCount}", nameof actorCount)
        
        if roundCount < 1 then
            return! FIO.Fail <| ArgumentException($"Bang failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount)
        
        let timerChan = Channel<TimerMessage<int>>()
        let startChan = Channel<int>()

        let receivingActor =
            { Name = "Actor-0"
              Chan = Channel<int>() }

        let sendingActors = createSendingActors receivingActor.Chan actorCount
        let! timerFiber = (TimerEff (actorCount + 1) (actorCount + 1) 1 timerChan).Fork()
        do! timerChan.Send(MsgChannel startChan).Unit
        do! createBang receivingActor sendingActors actorCount roundCount 1 timerChan startChan
        let! res = timerFiber.Join()
        return res
    }
