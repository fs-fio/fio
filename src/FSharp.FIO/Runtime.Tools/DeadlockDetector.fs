(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Deadlock detection tool for monitoring runtime state and detecting potential deadlocks.
/// This implementation is non-destructive and monitors channel state without interfering.
/// </summary>
module internal FSharp.FIO.Runtime.Tools.DeadlockDetector

open FSharp.FIO.DSL

open System.Threading.Tasks
open System.Collections.Concurrent

[<AbstractClass>]
type internal Worker () =
    abstract Working: unit -> bool

/// <summary>
/// Deadlock detector that monitors runtime state for potential deadlock conditions.
/// Detection logic: If there's no work in the queue, all workers are idle, but there
/// are still blocking items, a potential deadlock is detected.
/// </summary>
type internal DeadlockDetector<'B, 'E when 'B :> Worker and 'E :> Worker>(activeWorkItemChan: UnboundedChannel<WorkItem>, intervalMs: int) =
    let blockingItems = ConcurrentDictionary<BlockingItem, unit>()
    let mutable blockingWorkers: List<'B> = []
    let mutable evalWorkers: List<'E> = []
    let mutable countDown = 10
    let mutable running = true

    let allEvalWorkersIdle () =
        evalWorkers.IsEmpty ||
        not (evalWorkers |> List.exists (fun (w: 'E) -> w.Working()))

    let allBlockingWorkersIdle () =
        blockingWorkers.IsEmpty ||
        not (blockingWorkers |> List.exists (fun (w: 'B) -> w.Working()))

    let monitorTask =
        task {
            while running do
                // Non-destructive check: only read channel count
                let workItemCount = activeWorkItemChan.Count
                let blockingItemCount = blockingItems.Count

                // Detect potential deadlock condition
                if workItemCount <= 0 && allEvalWorkersIdle() && blockingItemCount > 0 then
                    if countDown <= 0 then
                        printfn "DEADLOCK_DETECTOR: ############ WARNING: Potential deadlock detected! ############"
                        printfn "DEADLOCK_DETECTOR:     Work items in queue: %i" workItemCount
                        printfn "DEADLOCK_DETECTOR:     Blocking items waiting: %i" blockingItemCount
                        printfn "DEADLOCK_DETECTOR:     All evaluation workers idle: %b" (allEvalWorkersIdle())
                        printfn "DEADLOCK_DETECTOR:     All blocking workers idle: %b" (allBlockingWorkersIdle())
                        printfn "DEADLOCK_DETECTOR: ########################################################################"
                    else
                        countDown <- countDown - 1
                else
                    countDown <- 10

                do! Task.Delay(intervalMs)
        }

    do monitorTask |> ignore

    /// <summary>
    /// Registers a blocking item for deadlock monitoring.
    /// </summary>
    member internal _.AddBlockingItem blockingItem =
        blockingItems.TryAdd (blockingItem, ()) |> ignore

    /// <summary>
    /// Unregisters a blocking item (it was unblocked).
    /// </summary>
    member internal _.RemoveBlockingItem blockingItem =
        blockingItems.TryRemove blockingItem |> ignore

    /// <summary>
    /// Sets the evaluation workers to monitor.
    /// </summary>
    member internal _.SetEvalWorkers workers =
        evalWorkers <- workers

    /// <summary>
    /// Sets the blocking workers to monitor.
    /// </summary>
    member internal _.SetBlockingWorkers workers =
        blockingWorkers <- workers

    /// <summary>
    /// Stops the deadlock detection loop.
    /// </summary>
    member _.Stop() =
        running <- false
