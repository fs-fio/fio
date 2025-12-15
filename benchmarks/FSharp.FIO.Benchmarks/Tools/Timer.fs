(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

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

let private startLoop startCount timerChan (stopwatch: Stopwatch) =
    fio {
        if startCount < 0 then
            return! !- (invalidArg "startCount" $"Timer: startCount must be non-negative! startCount = %i{startCount}")
        
        let mutable currentCount = startCount
        
        while currentCount > 0 do
            match! !<-- timerChan with
            | Start ->
                #if DEBUG
                do! FConsole.PrintLine $"[DEBUG]: Timer received Start message (%i{startCount - currentCount + 1}/%i{startCount})"
                #endif
                currentCount <- currentCount - 1
            | _ -> ()
        
        do! !<< (fun () -> stopwatch.Start())
        #if DEBUG
        do! FConsole.PrintLine "[DEBUG]: Timer started"
        #endif
    }

let private msgLoop msgCount msg msgChan =
    fio {
        if msgCount < 0 then
            return! !- (invalidArg "msgCount" $"Timer: msgCount must be non-negative! msgCount = %i{msgCount}")
        
        let mutable currentCount = msgCount
        let mutable currentMsg = msg
        
        while currentCount > 0 do
            do! msgChan <!-- msg
            #if DEBUG
            do! FConsole.PrintLine $"[DEBUG]: Timer sent %i{msg} to MsgChannel (%i{msgCount - currentCount + 1}/%i{msgCount})"
            #endif
            currentCount <- currentCount - 1
            currentMsg <- currentMsg + 1
    }

let private stopLoop stopCount timerChan (stopwatch: Stopwatch) =
    fio {
        if stopCount < 0 then
            return! !- (invalidArg "stopCount" $"Timer: stopCount must be non-negative! stopCount = %i{stopCount}")
               
        let mutable currentCount = stopCount
        
        while currentCount > 0 do
            match! !<-- timerChan with
            | Stop ->
                #if DEBUG
                do! FConsole.PrintLine $"[DEBUG]: Timer received Stop message (%i{stopCount - currentCount + 1}/%i{stopCount})"
                #endif
                currentCount <- currentCount - 1
            | _ -> ()
            
        do! !<< (fun () -> stopwatch.Stop())
        #if DEBUG
        do! FConsole.PrintLine "[DEBUG]: Timer stopped"
        #endif
    }

let TimerEff startCount msgCount stopCount timerChan =
    fio {
        let mutable msgChan = Channel<int>()
        let! stopwatch = !<< (fun () -> Stopwatch())
        
        if msgCount > 0 then
            match! !<-- timerChan with
            | MsgChannel chan ->
                msgChan <- chan
            | _ ->
                return! !- (invalidOp "Timer: Did not receive MsgChannel as first message when msgCount > 0!")
        else
            return ()

        do! startLoop startCount timerChan stopwatch
        do! msgLoop msgCount 0 msgChan
        do! stopLoop stopCount timerChan stopwatch
        return stopwatch.ElapsedMilliseconds
    }
