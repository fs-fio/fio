[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Philosophers

open FIO.DSL

open System.Collections.Generic

type private ArbMessage =
    | Hungry of id: int * grantedChannel: Channel<unit>
    | Done of id: int

let private arbitratorEffect (arbChannel: Channel<ArbMessage>) philosopherCount totalRounds =
    let forks = Array.create philosopherCount true
    let pending = Queue<int * Channel<unit>>()
    let mutable doneCount = 0

    let leftFork id =
        id
    let rightFork id =
        (id + 1) % philosopherCount
    let canEat id =
        forks[leftFork id] && forks[rightFork id]
    let takeForks id =
        forks[leftFork id] <- false
        forks[rightFork id] <- false
    let releaseForks id =
        forks[leftFork id] <- true
        forks[rightFork id] <- true

    let tryGrantPendingEffect () =
        fio {
            let scanCount = pending.Count
            for _ in 1..scanCount do
                let id, grantedChannel = pending.Dequeue()

                if canEat id then
                    takeForks id
                    do! grantedChannel.Write(()).Unit()
                else
                    pending.Enqueue(id, grantedChannel)
        }

    let rec loop () =
        fio {
            if doneCount < totalRounds then
                match! arbChannel.Read() with
                | Hungry(id, grantedChannel) ->
                    if canEat id then
                        takeForks id
                        do! grantedChannel.Write(()).Unit()
                    else
                        pending.Enqueue(id, grantedChannel)
                | Done id ->
                    doneCount <- doneCount + 1
                    releaseForks id
                    do! tryGrantPendingEffect ()
                return! loop ()
        }

    loop ()

let private philosopherEffect (arbChannel: Channel<ArbMessage>) id roundCount =
    fio {
        let grantedChannel = Channel<unit>()
        for _ in 1..roundCount do
            do! arbChannel.Write(Hungry(id, grantedChannel)).Unit()
            do! grantedChannel.Read().Unit()
            do! arbChannel.Write(Done id).Unit()
    }

let effect philosopherCount roundCount : FIO<unit, exn> =
    fio {
        let arbChannel = Channel<ArbMessage>()
        let totalRounds = philosopherCount * roundCount

        let philosophers =
            [ for id in 0 .. philosopherCount - 1 ->
                philosopherEffect arbChannel id roundCount ]

        do! FIO.collectAllParDiscard (arbitratorEffect arbChannel philosopherCount totalRounds :: philosophers)
    }
