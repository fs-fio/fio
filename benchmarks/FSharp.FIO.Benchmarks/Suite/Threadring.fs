(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(* ----------------------------------------------------------------------------------------- *)
(* Threadring benchmark                                                                      *)
(* Measures: Message sending; Context switching between actors                               *)
(* Savina benchmark #5                                                                       *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf)                  *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Suite.Threadring

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO.Lib.IO
#endif
open FSharp.FIO.Benchmarks.Tools.Timer

open System

type private Actor =
    { Name: string
      SendChan: int channel
      ReceiveChan: int channel }

let private createActor actor isLastActor roundCount (timerChan: TimerMessage<int> channel) =
    fio {
        do! (timerChan.Send Start).Unit
        
        for round in 1..roundCount do
            let! receivedMsg = actor.ReceiveChan.Receive ()
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{actor.Name} received: %i{receivedMsg}"
            #endif
            if isLastActor && round = roundCount then
                // The last actor of the last round should not send a message
                return ()
            else
                let! sentMsg = actor.SendChan.Send (receivedMsg + 1)
                #if DEBUG
                do! FConsole.PrintLine $"[DEBUG]: %s{actor.Name} sent: %i{sentMsg}"
                #endif
                return ()
        
        do! (timerChan.Send Stop).Unit
    }

let private createThreadring (actors: Actor list) roundCount (timerChan: TimerMessage<int> channel) =
    fio {
        let mutable currentEff = createActor actors.Head false roundCount timerChan
        do! (timerChan.Send (MsgChannel actors.Head.ReceiveChan)).Unit
        
        for index, actor in List.indexed actors.Tail do
            // +2 to compensate for the first actor and 0-indexed for loop
            let isLastActor = index + 2 = actors.Length
            currentEff <- (createActor actor isLastActor roundCount timerChan
                           <&> currentEff).Unit
            
        return! currentEff
    }

[<TailCall>]
let rec private createActors chans (allChans: Channel<int> list) index acc =
    match chans with
    | [] -> acc
    | chan'::chans' ->
        let actor =
            { Name = $"Actor-{index}"
              SendChan = chan'
              ReceiveChan =
                match index with
                | index when index - 1 < 0 -> allChans.Item(List.length allChans - 1)
                | index -> allChans.Item(index - 1) }
        createActors chans' allChans (index + 1) (acc @ [actor])

let createThreadringBenchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | ThreadringConfig (actorCount, roundCount) -> FIO.Succeed (actorCount, roundCount)
            | _ -> FIO.Fail <| ArgumentException ("Threadring benchmark requires a ThreadringConfig!", nameof config)
        
        if actorCount < 2 then
            return! FIO.Fail <| ArgumentException ($"Threadring failed: At least 2 actors should be specified. actorCount = %i{actorCount}", nameof actorCount)
        
        if roundCount < 1 then
            return! FIO.Fail <| ArgumentException ($"Threadring failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount)
        
        let chans = [for _ in 1..actorCount -> Channel<int> ()]
        let timerChan = Channel<TimerMessage<int>> ()
        let actors = createActors chans chans 0 []
        let! fiber = (TimerEff actorCount 1 actorCount timerChan).Fork ()
        do! createThreadring actors roundCount timerChan
        let! res = fiber.Join ()
        return res
    }
