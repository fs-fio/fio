(*****************************************************)
(* Fork benchmark                                    *)
(* Measures: Fiber creation and scheduling overhead  *)
(*****************************************************)

/// <summary>Provides the Fork benchmark measuring fiber creation and scheduling overhead.</summary>
[<RequireQualifiedAccess>]
module private FIO.Benchmarks.Suite.Fork

open FIO.DSL
open FIO.Benchmarks.Tools.Timer

open System

/// <summary>Builds the actor effect that signals completion to the timer.</summary>
/// <param name="timerChan">Channel for timer control messages.</param>
let private actorEff (timerChan: Channel<TimerMessage<int>>) = fio { do! timerChan.Send(Stop).Unit() }

/// <summary>Combines multiple actor effects to run concurrently via the parallel operator.</summary>
/// <param name="actorCount">Number of actors to fork.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
let private forkEff (actorCount, timerChan) =
    let baseEff = actorEff timerChan

    [ 1..actorCount ]
    |> List.fold (fun acc _ -> actorEff timerChan <&&> acc) baseEff

/// <summary>Builds the fork benchmark effect, returning execution time in milliseconds.</summary>
/// <param name="config">Fork benchmark configuration.</param>
/// <returns>An effect that produces the execution time in milliseconds.</returns>
let benchmark config : FIO<int64, exn> =
    fio {
        let! actorCount =
            match config with
            | ForkConfig ac -> FIO.succeed ac
            | _ ->
                FIO.fail (
                    ArgumentException("Fork benchmark initialization failed: Requires a ForkConfig", nameof config)
                )

        if actorCount < 1 then
            return!
                FIO.fail (
                    ArgumentException(
                        $"Fork benchmark initialization failed: At least 1 actor should be specified. actorCount = %i{actorCount}",
                        nameof actorCount
                    )
                )

        let timerChan = Channel<TimerMessage<int>>()
        let! timerFiber = timerEff(1, 0, actorCount, timerChan).Fork()
        do! timerChan.Send(Start).Unit()
        do! forkEff (actorCount, timerChan)
        return! timerFiber.Join()
    }
