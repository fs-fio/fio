[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.BoundedBuffer

open FIO.DSL

open System.Collections.Generic

type private BufferMessage =
    | Put of item: int * ackChannel: Channel<unit>
    | Get of reply: Channel<int>

let private distribute total parts =
    let baseCount = total / parts
    let remainder = total % parts
    [ for index in 0 .. parts - 1 ->
        if index < remainder then baseCount + 1
        else baseCount ]

let private bufferEffect (bufferChannel: Channel<BufferMessage>) capacity totalItems =
    let items = Queue<int>()
    let waitingProducers = Queue<int * Channel<unit>>()
    let waitingConsumers = Queue<Channel<int>>()
    let mutable delivered = 0

    let rec loop () =
        fio {
            if delivered < totalItems then
                match! bufferChannel.Read() with
                | Put(item, ackChannel) ->
                    if waitingConsumers.Count > 0 then
                        let reply = waitingConsumers.Dequeue()
                        delivered <- delivered + 1
                        do! reply.Write(item).Unit()
                        do! ackChannel.Write(()).Unit()
                    elif items.Count < capacity then
                        items.Enqueue item
                        do! ackChannel.Write(()).Unit()
                    else
                        waitingProducers.Enqueue(item, ackChannel)
                | Get replyChannel ->
                    if items.Count > 0 then
                        let item = items.Dequeue()
                        delivered <- delivered + 1
                        do! replyChannel.Write(item).Unit()

                        if waitingProducers.Count > 0 then
                            let parkedItem, parkedAck = waitingProducers.Dequeue()
                            items.Enqueue parkedItem
                            do! parkedAck.Write(()).Unit()
                    else
                        waitingConsumers.Enqueue replyChannel

                return! loop ()
        }

    loop ()

let private producerEffect (bufferChannel: Channel<BufferMessage>) count firstItem =
    fio {
        let ackChannel = Channel<unit>()
        for index in 0 .. count - 1 do
            do! bufferChannel.Write(Put(firstItem + index, ackChannel)).Unit()
            do! ackChannel.Read().Unit()
    }

let private consumerEffect (bufferChannel: Channel<BufferMessage>) count =
    fio {
        let replyChannel = Channel<int>()
        for _ in 1..count do
            do! bufferChannel.Write(Get replyChannel).Unit()
            do! replyChannel.Read().Unit()
    }

let effect producerCount consumerCount capacity itemsPerProducer : FIO<unit, exn> =
    fio {
        let bufferChannel = Channel<BufferMessage>()
        let totalItems = producerCount * itemsPerProducer
        let consumerCounts = distribute totalItems consumerCount

        let producers =
            [ for producerIndex in 0 .. producerCount - 1 ->
                producerEffect bufferChannel itemsPerProducer (producerIndex * itemsPerProducer + 1) ]

        let consumers =
            [ for count in consumerCounts ->
                consumerEffect bufferChannel count ]

        do! FIO.collectAllParDiscard (bufferEffect bufferChannel capacity totalItems :: (producers @ consumers))
    }
