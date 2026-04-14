/// <summary>
/// Timer utilities for measuring benchmark execution time.
/// </summary>
module internal FIO.Benchmarks.Tools.Timer

open FIO.DSL
#if DEBUG
open FIO.Console
#endif

open System
open System.Diagnostics

/// <summary>
/// Messages for controlling the timer's start/stop behavior and channel communication.
/// </summary>
type internal TimerMessage<'R> =
    /// <summary>Signal to start timing.</summary>
    | Start
    /// <summary>Signal to stop timing.</summary>
    | Stop
    /// <summary>Channel for sending messages to benchmark actors.</summary>
    | MsgChannel of Channel<'R>

/// <summary>
/// Waits for the specified number of Start messages before starting the stopwatch.
/// </summary>
/// <param name="startCount">Number of Start messages to wait for.</param>
/// <param name="timerChan">Channel for receiving timer messages.</param>
/// <param name="stopwatch">Stopwatch instance to start.</param>
let private startLoop (startCount, timerChan: Channel<TimerMessage<int>>, stopwatch: Stopwatch) : FIO<unit, exn> =
    fio {
        if startCount < 0 then
            return!
                FIO.fail (
                    ArgumentException(
                        $"Timer initialization failed: startCount must be non-negative. startCount = %i{startCount}",
                        nameof startCount
                    )
                )

        let mutable currentCount = startCount

        while currentCount > 0 do
            match! timerChan.Receive() with
            | Start ->
#if DEBUG
                do!
                    Console.printLine (
                        $"[DEBUG]: Timer received Start message (%i{startCount - currentCount + 1}/%i{startCount})",
                        id
                    )
#endif
                currentCount <- currentCount - 1
            | _ -> ()

        do! FIO.attempt ((fun () -> stopwatch.Start()), id)
#if DEBUG
        do! Console.printLine ("[DEBUG]: Timer started", id)
#endif
    }

/// <summary>
/// Sends the specified number of messages to the message channel.
/// </summary>
/// <param name="msgCount">Number of messages to send.</param>
/// <param name="msg">Initial message value.</param>
/// <param name="msgChan">Channel to send messages to.</param>
let private msgLoop (msgCount, msg, msgChan: Channel<int>) : FIO<unit, exn> =
    fio {
        if msgCount < 0 then
            return!
                FIO.fail (
                    ArgumentException(
                        $"Timer initialization failed: msgCount must be non-negative. msgCount = %i{msgCount}",
                        nameof msgCount
                    )
                )

        let mutable currentCount = msgCount
        let mutable currentMsg = msg

        while currentCount > 0 do
            do! msgChan.Send(msg).Unit()
#if DEBUG
            do!
                Console.printLine (
                    $"[DEBUG]: Timer sent %i{msg} to MsgChannel (%i{msgCount - currentCount + 1}/%i{msgCount})",
                    id
                )
#endif
            currentCount <- currentCount - 1
            currentMsg <- currentMsg + 1
    }

/// <summary>
/// Waits for the specified number of Stop messages before stopping the stopwatch.
/// </summary>
/// <param name="stopCount">Number of Stop messages to wait for.</param>
/// <param name="timerChan">Channel for receiving timer messages.</param>
/// <param name="stopwatch">Stopwatch instance to stop.</param>
let private stopLoop (stopCount, timerChan: Channel<TimerMessage<int>>, stopwatch: Stopwatch) : FIO<unit, exn> =
    fio {
        if stopCount < 0 then
            return!
                FIO.fail (
                    ArgumentException(
                        $"Timer initialization failed: stopCount must be non-negative. stopCount = %i{stopCount}",
                        nameof stopCount
                    )
                )

        let mutable currentCount = stopCount

        while currentCount > 0 do
            match! timerChan.Receive() with
            | Stop ->
#if DEBUG
                do!
                    Console.printLine (
                        $"[DEBUG]: Timer received Stop message (%i{stopCount - currentCount + 1}/%i{stopCount})",
                        id
                    )
#endif
                currentCount <- currentCount - 1
            | _ -> ()

        do! FIO.attempt ((fun () -> stopwatch.Stop()), id)
#if DEBUG
        do! Console.printLine ("[DEBUG]: Timer stopped", id)
#endif
    }

/// <summary>
/// Timer effect that coordinates benchmark timing via start/stop message counting.
/// </summary>
/// <param name="startCount">Number of Start messages to wait for before timing.</param>
/// <param name="msgCount">Number of messages to send to actors.</param>
/// <param name="stopCount">Number of Stop messages to wait for before stopping.</param>
/// <param name="timerChan">Channel for timer control messages.</param>
/// <returns>Elapsed time in milliseconds.</returns>
let timerEff (startCount, msgCount, stopCount, timerChan: Channel<TimerMessage<int>>) : FIO<int64, exn> =
    fio {
        let mutable msgChan = Channel<int>()
        let! stopwatch = FIO.attempt ((fun () -> Stopwatch()), id)

        if msgCount > 0 then
            match! timerChan.Receive() with
            | MsgChannel chan -> msgChan <- chan
            | _ ->
                return!
                    FIO.fail (
                        InvalidOperationException
                            "Timer: Did not receive MsgChannel as first message when msgCount > 0!"
                    )
        else
            return ()

        do! startLoop (startCount, timerChan, stopwatch)
        do! msgLoop (msgCount, 0, msgChan)
        do! stopLoop (stopCount, timerChan, stopwatch)
        return stopwatch.ElapsedMilliseconds
    }
