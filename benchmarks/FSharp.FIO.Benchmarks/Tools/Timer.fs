module internal FSharp.FIO.Benchmarks.Tools.Timer

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO.Lib.IO
#endif

open System.Diagnostics

type internal TimerMessage<'R> =
    | Start
    | Stop
    | MsgChannel of 'R channel

let private startLoop (startCount, timerChan: TimerMessage<int> channel, stopwatch: Stopwatch) =
    fio {
        if startCount < 0 then
            return! FIO.Fail(invalidArg "startCount" $"Timer: startCount must be non-negative! startCount = %i{startCount}")
        
        let mutable currentCount = startCount
        
        while currentCount > 0 do
            match! timerChan.Receive() with
            | Start ->
                #if DEBUG
                do! FConsole.PrintLine $"[DEBUG]: Timer received Start message (%i{startCount - currentCount + 1}/%i{startCount})"
                #endif
                currentCount <- currentCount - 1
            | _ -> ()
        
        do! FIO.Attempt(fun () -> stopwatch.Start())
        #if DEBUG
        do! FConsole.PrintLine "[DEBUG]: Timer started"
        #endif
    }

let private msgLoop (msgCount, msg, msgChan: int channel) =
    fio {
        if msgCount < 0 then
            return! FIO.Fail(invalidArg "msgCount" $"Timer: msgCount must be non-negative! msgCount = %i{msgCount}")
        
        let mutable currentCount = msgCount
        let mutable currentMsg = msg
        
        while currentCount > 0 do
            do! msgChan.Send(msg).Unit()
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: Timer sent %i{msg} to MsgChannel (%i{msgCount - currentCount + 1}/%i{msgCount})"
            #endif
            currentCount <- currentCount - 1
            currentMsg <- currentMsg + 1
    }

let private stopLoop (stopCount, timerChan: TimerMessage<int> channel, stopwatch: Stopwatch) =
    fio {
        if stopCount < 0 then
            return! FIO.Fail(invalidArg "stopCount" $"Timer: stopCount must be non-negative! stopCount = %i{stopCount}")
               
        let mutable currentCount = stopCount
        
        while currentCount > 0 do
            match! timerChan.Receive() with
            | Stop ->
                #if DEBUG
                do! FConsole.PrintLine $"[DEBUG]: Timer received Stop message (%i{stopCount - currentCount + 1}/%i{stopCount})"
                #endif
                currentCount <- currentCount - 1
            | _ -> ()
            
        do! FIO.Attempt(fun () -> stopwatch.Stop())
        #if DEBUG
        do! FConsole.PrintLine "[DEBUG]: Timer stopped"
        #endif
    }

let TimerEff (startCount, msgCount, stopCount, timerChan: TimerMessage<int> channel) =
    fio {
        let mutable msgChan = Channel<int>()
        let! stopwatch = FIO.Attempt(fun () -> Stopwatch())
        
        if msgCount > 0 then
            match! timerChan.Receive() with
            | MsgChannel chan ->
                msgChan <- chan
            | _ ->
                return! FIO.Fail(invalidOp "Timer: Did not receive MsgChannel as first message when msgCount > 0!")
        else
            return ()

        do! startLoop(startCount, timerChan, stopwatch)
        do! msgLoop(msgCount, 0, msgChan)
        do! stopLoop(stopCount, timerChan, stopwatch)
        return stopwatch.ElapsedMilliseconds
    }
