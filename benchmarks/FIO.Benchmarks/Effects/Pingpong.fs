[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Pingpong

open FIO.DSL

type private Actor =
    {
        SendChannel: Channel<int>
        ReceiveChannel: Channel<int>
    }

// Waits for the start signal, then sends a ping and awaits the pong each round.
let private pingerEffect pinger roundCount (startChannel: Channel<int>) =
    fio {
        let mutable currentPing = 1
        do! startChannel.Read().Unit()
        for _ in 1..roundCount do
            do! pinger.SendChannel.Write(currentPing).Unit()
            let! pong = pinger.ReceiveChannel.Read()
            currentPing <- pong + 1
    }

// Fires the start signal, then echoes each received ping back as a pong.
let private pongerEffect ponger roundCount (startChannel: Channel<int>) =
    fio {
        do! startChannel.Write(0).Unit()
        for _ in 1..roundCount do
            let! ping = ponger.ReceiveChannel.Read()
            do! ponger.SendChannel.Write(ping + 1).Unit()
    }

// Builds the Pingpong workload: two fibers exchanging messages over channels.
let effect roundCount : FIO<unit, exn> =
    fio {
        let startChannel = Channel<int>()
        let pingSendChannel = Channel<int>()
        let pongSendChannel = Channel<int>()

        let pinger =
            {
                SendChannel = pingSendChannel
                ReceiveChannel = pongSendChannel
            }

        let ponger =
            {
                SendChannel = pongSendChannel
                ReceiveChannel = pingSendChannel
            }

        do! pingerEffect pinger roundCount startChannel
            <&&> pongerEffect ponger roundCount startChannel
    }
