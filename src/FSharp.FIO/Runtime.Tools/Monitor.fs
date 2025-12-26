/// <summary>
/// Runtime monitoring tool for observing channel state.
/// Note: This provides non-destructive monitoring by only reading channel counts
/// and metadata, without removing items from channels.
/// </summary>
module internal FSharp.FIO.Runtime.Tools.Monitor

open FSharp.FIO.DSL

open System.Threading.Tasks

/// <summary>
/// Monitor for observing runtime channel state.
/// </summary>
type internal Monitor (
    activeWorkItemChan: UnboundedChannel<WorkItem>,
    activeBlockingItemChanOpt: UnboundedChannel<BlockingItem> option,
    activeBlockingEventChan: UnboundedChannel<Channel<obj>> option) =

    let mutable running = true
    let monitorTask =
        task {
            while running do
                printfn "\n========== FIO Runtime Monitor Snapshot =========="
                printfn "Timestamp: %s" (System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff"))
                printfn ""

                printfn "Active Work Items:"
                printfn "  Count: %i" activeWorkItemChan.Count
                printfn "  Channel ID: %A" activeWorkItemChan.Id

                match activeBlockingItemChanOpt with
                | Some chan ->
                    printfn ""
                    printfn "Blocking Data:"
                    printfn "  Count: %i" chan.Count
                    printfn "  Channel ID: %A" chan.Id
                | None ->
                    ()

                match activeBlockingEventChan with
                | Some chan ->
                    printfn ""
                    printfn "Blocking Events:"
                    printfn "  Count: %i" chan.Count
                    printfn "  Channel ID: %A" chan.Id
                | None ->
                    ()

                printfn "=================================================="
                printfn ""

                do! Task.Delay 1000
        }

    do monitorTask |> ignore

    /// <summary>
    /// Stops the monitoring loop.
    /// </summary>
    member _.Stop () =
        running <- false
