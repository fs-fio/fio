(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(* ----------------------------------------------------------------------------------------- *)
(* Big benchmark                                                                             *)
(* Measures: Contention on mailbox; Many-to-Many message passing                             *)
(* Savina benchmark #7                                                                       *)
(* (http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf)                  *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Suite.Big

open FSharp.FIO.DSL
#if DEBUG
open FSharp.FIO.Lib.IO
#endif
open FSharp.FIO.Benchmarks.Tools.Timer

open System

type private Message =
    | Ping of int * Message channel
    | Pong of int

type private Actor =
    { Name: string
      PingReceiveChan: Message channel
      PongReceiveChan: Message channel
      SendingChans: Message channel list }

[<TailCall>]
let rec private createSendPings actor roundCount ping (chans: Message channel list) timerChan : FIO<unit, exn> =
    fio {
        for chan in chans do
            do! (chan.Send (Ping (ping, actor.PongReceiveChan))).Unit
            #if DEBUG
            do! FConsole.PrintLine $"DEBUG: %s{actor.Name} sent ping: %i{ping}"
            #endif
        
        return! createReceivePings actor roundCount actor.SendingChans.Length ping timerChan
    }

and private createReceivePings actor rounds receiveCount msg timerChan : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PingReceiveChan.Receive () with
            | Ping (ping, replyChan) ->
                #if DEBUG
                do! FConsole.PrintLine $"DEBUG: %s{actor.Name} received ping: %i{ping}"
                #endif
                match! replyChan.Send <| Pong (ping + 1) with
                | Pong pong ->
                    #if DEBUG
                    do! FConsole.PrintLine $"DEBUG: %s{actor.Name} sent pong: %i{pong}"
                    #endif
                    return ()
                | Ping _ ->
                    return! FIO.Fail <| InvalidOperationException "createReceivePings: Received ping when pong was expected!"
            | _ ->
                return! FIO.Fail <| InvalidOperationException "createReceivePings: Received pong when ping was expected!"
                
        return! createReceivePongs actor rounds actor.SendingChans.Length msg timerChan
    }

and private createReceivePongs actor roundCount receiveCount msg (timerChan: TimerMessage<int> channel) : FIO<unit, exn> =
    fio {
        for _ in 1..receiveCount do
            match! actor.PongReceiveChan.Receive () with
            | Pong pong -> 
                #if DEBUG
                do! FConsole.PrintLine $"DEBUG: %s{actor.Name} received pong: %i{pong}"
                #endif
                return ()
            | _ ->
                return! FIO.Fail <| InvalidOperationException "createRecvPongs: Received ping when pong was expected!"
        
        if roundCount <= 0 then
            do! (timerChan.Send Stop).Unit
        else
            return! createSendPings actor (roundCount - 1) msg actor.SendingChans timerChan
    }

let private createActor actor msg roundCount (timerChan: TimerMessage<int> channel) (startChan: int channel) : FIO<unit, exn> =
    fio {
        do! (timerChan.Send Start).Unit
        do! startChan.Receive().Unit
        return! createSendPings actor (roundCount - 1) msg actor.SendingChans timerChan
    }

[<TailCall>]
let rec private createReceivingActors actorCount acc =
    match actorCount with
    | 0 -> acc
    | count ->
        let actor =
            { Name = $"Actor-{count - 1}"
              PingReceiveChan = Channel<Message>()
              PongReceiveChan = Channel<Message>()
              SendingChans = [] }
        createReceivingActors (count - 1) (acc @ [actor])

[<TailCall>]
let rec private createActorsHelper receivingActors prevReceivingActors acc =
    match receivingActors with
    | [] -> acc
    | ac :: acs ->
        let otherActors = prevReceivingActors @ acs
        let chansSend = List.map _.PingReceiveChan otherActors

        let actor =
            { Name = ac.Name
              PingReceiveChan = ac.PingReceiveChan
              PongReceiveChan = ac.PongReceiveChan
              SendingChans = chansSend }

        createActorsHelper acs (prevReceivingActors @ [ac]) (actor :: acc)

let private createActors actorCount =
    let receivingActors = createReceivingActors actorCount []
    createActorsHelper receivingActors [] []

let private createBig (actors: Actor list) roundCount msg timerChan startChan : FIO<unit, exn> =
    fio {
        let mutable currentMsg = msg
        let mutable currentEff = createActor actors.Head currentMsg roundCount timerChan startChan
       
        for actor in actors.Tail do
            currentMsg <- currentMsg + 10
            currentEff <- (createActor actor currentMsg roundCount timerChan startChan
                           <&> currentEff).Unit
        
        return! currentEff
    }

let createBigBenchmark config : FIO<int64, exn> =
    fio {
        let! actorCount, roundCount =
            match config with
            | BigConfig (actorCount, roundCount) -> FIO.Succeed (actorCount, roundCount)
            | _ -> FIO.Fail <| ArgumentException ("Big benchmark requires a BigConfig!", nameof config)
        
        if actorCount < 2 then
            return! FIO.Fail <| ArgumentException ($"Big failed: At least 2 actors should be specified. actorCount = %i{actorCount}", nameof actorCount)
        
        if roundCount < 1 then
            return! FIO.Fail <| ArgumentException ($"Big failed: At least 1 round should be specified. roundCount = %i{roundCount}", nameof roundCount)
        
        let timerChan = Channel<TimerMessage<int>> ()
        let startChan = Channel<int> ()
        let actors = createActors actorCount

        let! timerFiber = (TimerEff actorCount actorCount actorCount timerChan).Fork ()
        do! (timerChan.Send (MsgChannel startChan)).Unit
        do! createBig actors roundCount 0 timerChan startChan
        let! res = timerFiber.Join ()
        return res
    }
