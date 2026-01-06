(******************)
(* Fork benchmark *)
(******************)

module private FSharp.FIO.Benchmarks.Suite.Fork

open FSharp.FIO.DSL
open FSharp.FIO.Benchmarks.Tools.Timer

open System

let private actorEff (timerChan: TimerMessage<int> channel) =
    fio {
        do! timerChan.Send(Stop).Unit()
    }

let private forkEff (actorCount, timerChan) =
    fio {
        let mutable currentEff = actorEff timerChan
        
        for _ in 1..actorCount do
            currentEff <- actorEff timerChan <&&> currentEff
            
        return! currentEff
    }

let forkBenchmark config : FIO<int64, exn> =
    fio {
        let! actorCount =
            match config with
            | ForkConfig ac -> FIO.Succeed ac
            | _ -> FIO.Fail(ArgumentException("Fork benchmark failed: Requires a ForkConfig", nameof config))
            
        if actorCount < 1 then
            return! FIO.Fail(ArgumentException($"Fork benchmark failed: At least 1 actor should be specified. actorCount = %i{actorCount}", nameof actorCount))
            
        let timerChan = Channel<TimerMessage<int>>()
        let! timerFiber = TimerEff(1, 0, actorCount, timerChan).Fork()
        do! timerChan.Send(Start).Unit()
        do! forkEff(actorCount, timerChan)
        return! timerFiber.Join()
    }
