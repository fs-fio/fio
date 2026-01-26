module internal FSharp.FIO.Benchmarks.Tools.Timer

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO
#endif

open System
open System.Diagnostics

type internal TimerMessage<'R> =
    | Start
    | Stop
    | MsgChannel of Channel<'R>

let private startLoop (startCount, timerChan: Channel<TimerMessage<int>>, stopwatch: Stopwatch) : FIO<unit, exn> =
    fio {
        if startCount < 0 then
            return! FIO.fail(ArgumentException($"Timer initialization failed: startCount must be non-negative. startCount = %i{startCount}", nameof startCount))
        
        let mutable currentCount = startCount
        
        while currentCount > 0 do
            match! timerChan.Receive() with
            | Start ->
                #if DEBUG
                do! Console.printLineExn $"[DEBUG]: Timer received Start message (%i{startCount - currentCount + 1}/%i{startCount})"
                #endif
                currentCount <- currentCount - 1
            | _ ->
                ()
        
        do! FIO.attemptExn(fun () -> stopwatch.Start())
        #if DEBUG
        do! Console.printLineExn "[DEBUG]: Timer started"
        #endif
    }

let private msgLoop (msgCount, msg, msgChan: Channel<int>) : FIO<unit, exn> =
    fio {
        if msgCount < 0 then
            return! FIO.fail(ArgumentException($"Timer initialization failed: msgCount must be non-negative. msgCount = %i{msgCount}", nameof msgCount))
        
        let mutable currentCount = msgCount
        let mutable currentMsg = msg
        
        while currentCount > 0 do
            do! msgChan.Send(msg).Unit()
            #if DEBUG
            do! Console.printLineExn $"[DEBUG]: Timer sent %i{msg} to MsgChannel (%i{msgCount - currentCount + 1}/%i{msgCount})"
            #endif
            currentCount <- currentCount - 1
            currentMsg <- currentMsg + 1
    }

let private stopLoop (stopCount, timerChan: Channel<TimerMessage<int>>, stopwatch: Stopwatch) : FIO<unit, exn> =
    fio {
        if stopCount < 0 then
            return! FIO.fail(ArgumentException($"Timer initialization failed: stopCount must be non-negative. stopCount = %i{stopCount}", nameof stopCount))
               
        let mutable currentCount = stopCount
        
        while currentCount > 0 do
            match! timerChan.Receive() with
            | Stop ->
                #if DEBUG
                do! Console.printLineExn $"[DEBUG]: Timer received Stop message (%i{stopCount - currentCount + 1}/%i{stopCount})"
                #endif
                currentCount <- currentCount - 1
            | _ ->
                ()
            
        do! FIO.attemptExn(fun () -> stopwatch.Stop())
        #if DEBUG
        do! Console.printLineExn "[DEBUG]: Timer stopped"
        #endif
    }

let TimerEff (startCount, msgCount, stopCount, timerChan: Channel<TimerMessage<int>>) : FIO<int64, exn> =
    fio {
        let mutable msgChan = Channel<int>()
        let! stopwatch = FIO.attemptExn(fun () -> Stopwatch())
        
        if msgCount > 0 then
            match! timerChan.Receive() with
            | MsgChannel chan ->
                msgChan <- chan
            | _ ->
                return! FIO.fail(InvalidOperationException "Timer: Did not receive MsgChannel as first message when msgCount > 0!")
        else
            return ()

        do! startLoop(startCount, timerChan, stopwatch)
        do! msgLoop(msgCount, 0, msgChan)
        do! stopLoop(stopCount, timerChan, stopwatch)
        return stopwatch.ElapsedMilliseconds
    }
