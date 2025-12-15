(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(* ----------------------------------------------------------------------------------------- *)
(* Fork benchmark                                                                            *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Suite.Fork

open FSharp.FIO.DSL
open FSharp.FIO.Benchmarks.Tools.Timer

open System

let private createActor timerChan =
    fio {
        do! timerChan <!-- Stop
    }

let private createFork actorCount timerChan =
    fio {
        let mutable currentEff = createActor timerChan
        
        for _ in 1..actorCount do
            currentEff <- createActor timerChan <~> currentEff
            
        return! currentEff
    }

let createForkBenchmark config : FIO<int64, exn> =
    fio {
        let! actorCount =
            match config with
            | ForkConfig actorCount -> !+ actorCount
            | _ -> !- ArgumentException("Fork benchmark requires a ForkConfig!", nameof(config))
            
        if actorCount < 1 then
            return! !- ArgumentException($"Fork failed: At least 1 actor should be specified. actorCount = %i{actorCount}", nameof(actorCount))
            
        let! timerChan = !+ Channel<TimerMessage<int>>()
        let! timerFiber = !<~ (TimerEff 1 0 actorCount timerChan)
        do! timerChan <!-- Start
        do! createFork actorCount timerChan
        let! res = !<~~ timerFiber
        return res
    }
