/// Pingpong benchmark — measures message delivery overhead between two actors.
[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Pingpong

open FIO.DSL

type private Actor = { SendChan: Channel<int>; ReceiveChan: Channel<int> }

let private pingerEff (pinger, ping, roundCount, startChan: Channel<int>) =
    fio {
        let mutable currentPing = ping
        do! startChan.Read().Unit()

        for _ in 1..roundCount do
            do! pinger.SendChan.Write(currentPing).Unit()
            let! pong = pinger.ReceiveChan.Read()
            currentPing <- pong + 1
    }

let private pongerEff (ponger, roundCount, startChan: Channel<int>) =
    fio {
        do! startChan.Write(0).Unit()

        for _ in 1..roundCount do
            let! ping = ponger.ReceiveChan.Read()
            do! ponger.SendChan.Write(ping + 1).Unit()
    }

let effect (roundCount: int) : FIO<unit, exn> =
    fio {
        let startChan = Channel<int>()
        let pingSendChan = Channel<int>()
        let pongSendChan = Channel<int>()
        let pinger = { SendChan = pingSendChan; ReceiveChan = pongSendChan }
        let ponger = { SendChan = pongSendChan; ReceiveChan = pingSendChan }

        do!
            pingerEff (pinger, 1, roundCount, startChan)
            <&&> pongerEff (ponger, roundCount, startChan)
    }
