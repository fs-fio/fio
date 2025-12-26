(****************************************************************************)
(* Pingpong benchmark                                                       *)
(* Measures: Message delivery overhead                                      *)
(* Savina benchmark #1                                                      *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf) *)
(****************************************************************************)

module private FSharp.FIO.Benchmarks.Suite.Pingpong

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

let private pingerEff (pinger, ping, roundCount, startChan: int channel, timerChan: TimerMessage<int> channel) =
    fio {
        let mutable currentPing = ping
        do! startChan.Receive().Unit()
        do! timerChan.Send(Start).Unit()
        
        for _ in 1..roundCount do
            do! pinger.SendChan.Send(currentPing).Unit()
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{pinger.Name} sent ping: %i{currentPing}"
            #endif
            let! pong = pinger.ReceiveChan.Receive()
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{pinger.Name} received pong: %i{pong}"
            #endif
            currentPing <- pong + 1
            
        do! timerChan.Send(Stop).Unit()
    }

let private pongerEff (ponger, roundCount, startChan: int channel) =
    fio {
        do! startChan.Send(0).Unit()
        
        for _ in 1..roundCount do
            let! ping = ponger.ReceiveChan.Receive()
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{ponger.Name} received ping: %i{ping}"
            #endif
            let! pong = ponger.SendChan.Send(ping + 1)
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: %s{ponger.Name} sent pong: %i{pong}"
            #endif
            return ()
    }

let pingpongBenchmark config : FIO<int64, exn> =
    fio {
        let! roundCount =
            match config with
            | PingpongConfig rc -> FIO.Succeed rc
            | _ -> FIO.Fail(ArgumentException("Pingpong benchmark failed: Requires a PingpongConfig", nameof config))
        
        if roundCount < 1 then
            return! FIO.Fail(ArgumentException($"Pingpong benchmark failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount))
        
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

        let! timerFiber = TimerEff(1, 0, 1, timerChan).Fork()
        do! pingerEff(pinger, 1, roundCount, startChan, timerChan)
            <&&> pongerEff(ponger, roundCount, startChan)
        return! timerFiber.Join()
    }
