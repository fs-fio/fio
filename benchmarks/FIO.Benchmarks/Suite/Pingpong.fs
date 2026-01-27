(****************************************************************************)
(* Pingpong benchmark                                                       *)
(* Measures: Message delivery overhead                                      *)
(* Savina benchmark #1                                                      *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf) *)
(****************************************************************************)

/// <summary>
/// Pingpong benchmark measuring message delivery overhead between two actors.
/// </summary>
[<RequireQualifiedAccess>]
module private FIO.Benchmarks.Suite.Pingpong

open FIO.DSL
#if DEBUG
open FIO.Console
#endif
open FIO.Benchmarks.Tools.Timer

open System

/// <summary>
/// Actor with send and receive channels for ping-pong communication.
/// </summary>
type private Actor =
    { /// <summary>Channel for sending messages.</summary>
      SendChan: Channel<int>
      /// <summary>Channel for receiving messages.</summary>
      ReceiveChan: Channel<int>
#if DEBUG
      /// <summary>Actor name for debugging.</summary>
      Name: string
#endif
    }

/// <summary>
/// Pinger actor effect that initiates ping-pong exchanges.
/// </summary>
/// <param name="pinger">Pinger actor instance.</param>
/// <param name="ping">Initial ping value.</param>
/// <param name="roundCount">Number of ping-pong rounds.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
let private pingerEff (pinger, ping, roundCount, startChan: Channel<int>, timerChan: Channel<TimerMessage<int>>) =
    fio {
        let mutable currentPing = ping
        do! startChan.Receive().Unit()
        do! timerChan.Send(Start).Unit()
        
        for _ in 1..roundCount do
            do! pinger.SendChan.Send(currentPing).Unit()
            #if DEBUG
            do! Console.printLineExn $"[DEBUG]: %s{pinger.Name} sent ping: %i{currentPing}"
            #endif
            let! pong = pinger.ReceiveChan.Receive()
            #if DEBUG
            do! Console.printLineExn $"[DEBUG]: %s{pinger.Name} received pong: %i{pong}"
            #endif
            currentPing <- pong + 1
            
        do! timerChan.Send(Stop).Unit()
    }

/// <summary>
/// Ponger actor effect that responds to pings with pongs.
/// </summary>
/// <param name="ponger">Ponger actor instance.</param>
/// <param name="roundCount">Number of ping-pong rounds.</param>
/// <param name="startChan">Channel for synchronizing start.</param>
let private pongerEff (ponger, roundCount, startChan: Channel<int>) =
    fio {
        do! startChan.Send(0).Unit()
        
        for _ in 1..roundCount do
            let! ping = ponger.ReceiveChan.Receive()
            #if DEBUG
            do! Console.printLineExn $"[DEBUG]: %s{ponger.Name} received ping: %i{ping}"
            #endif
            let! _pong = ponger.SendChan.Send(ping + 1)
            #if DEBUG
            do! Console.printLineExn $"[DEBUG]: %s{ponger.Name} sent pong: %i{_pong}"
            #endif
            ()
    }

/// <summary>
/// Creates and runs the pingpong benchmark, returning execution time in milliseconds.
/// </summary>
/// <param name="config">Pingpong benchmark configuration.</param>
/// <returns>Execution time in milliseconds.</returns>
let benchmark config : FIO<int64, exn> =
    fio {
        let! roundCount =
            match config with
            | PingpongConfig rc -> FIO.succeed rc
            | _ -> FIO.fail(ArgumentException("Pingpong benchmark initialization failed: Requires a PingpongConfig", nameof config))
        
        if roundCount < 1 then
            return! FIO.fail(ArgumentException($"Pingpong benchmark initialization failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount))
        
        let startChan = Channel<int>()
        let timerChan = Channel<TimerMessage<int>>()
        let pingSendChan = Channel<int>()
        let pongSendChan = Channel<int>()

        let pinger =
            { SendChan = pingSendChan
              ReceiveChan = pongSendChan
#if DEBUG
              Name = "Pinger"
#endif
            }

        let ponger =
            { SendChan = pongSendChan
              ReceiveChan = pingSendChan
#if DEBUG
              Name = "Ponger"
#endif
            }

        let! timerFiber = timerEff(1, 0, 1, timerChan).Fork()
        do! pingerEff(pinger, 1, roundCount, startChan, timerChan)
            <&&> pongerEff(ponger, roundCount, startChan)
        return! timerFiber.Join()
    }
