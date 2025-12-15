(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(* ----------------------------------------------------------------------------------------- *)
(* Pingpong benchmark                                                                        *)
(* Measures: Message delivery overhead                                                       *)
(* Savina benchmark #1                                                                       *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf)                  *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Suite.Pingpong

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

let private createPinger pinger ping roundCount startChan timerChan =
    fio {
        let mutable currentPing = ping
        do! !<!-- startChan
        do! timerChan <!-- Start
        
        for _ in 1..roundCount do
            do! pinger.SendChan <!-- currentPing
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{pinger.Name} sent ping: %i{currentPing}"
            #endif
            let! pong = !<-- pinger.ReceiveChan
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{pinger.Name} received pong: %i{pong}"
            #endif
            currentPing <- pong + 1
            
        do! timerChan <!-- Stop
    }

let private createPonger ponger roundCount startChan =
    fio {
        do! startChan <!-- 0
        
        for _ in 1..roundCount do
            let! ping = !<-- ponger.ReceiveChan
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{ponger.Name} received ping: %i{ping}"
            #endif
            let! pong = ponger.SendChan <-- ping + 1
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{ponger.Name} sent pong: %i{pong}"
            #endif
            return ()
    }

let createPingpongBenchmark config : FIO<int64, exn> =
    fio {
        let! roundCount =
            match config with
            | PingpongConfig roundCount -> !+ roundCount
            | _ -> !- ArgumentException("Pingpong benchmark requires a PingpongConfig!", nameof(config))
        
        if roundCount < 1 then
            return! !- ArgumentException($"Pingpong failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof(roundCount))
        
        let startChan = Channel<int>()
        let timerChan = Channel<TimerMessage<int>>()
        let pingSendChan = Channel<int>()
        let pongSendChan = Channel<int>()

        let pinger =
            { Name = "Pinger"
              SendChan = pingSendChan
              ReceiveChan = pongSendChan }

        let ponger =
            { Name = "Ponger"
              SendChan = pongSendChan
              ReceiveChan = pingSendChan }

        let! timerFiber = !<~ (TimerEff 1 0 1 timerChan)
        do! createPinger pinger 1 roundCount startChan timerChan
            <~> createPonger ponger roundCount startChan
        let! res = !<~~ timerFiber
        return res
    }
