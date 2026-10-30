namespace FIO.Benchmarks.Benchmarks

open FIO.DSL
open FIO.Benchmarks

open BenchmarkDotNet.Attributes

[<RequireQualifiedAccess>]
module internal Chameneos =

    type private Color =
        | Red
        | Yellow
        | Blue

    type private Response =
        | Paired of Color
        | Stop

    type private Request =
        {
            Color: Color
            ReplyChannel: Channel<Response>
        }

    let private complement = function
        | Red, Red -> Red
        | Yellow, Yellow -> Yellow
        | Blue, Blue -> Blue
        | Red, Yellow
        | Yellow, Red -> Blue
        | Red, Blue
        | Blue, Red -> Yellow
        | Yellow, Blue
        | Blue, Yellow -> Red

    // The meeting-place actor: pairs up arriving creatures until the meeting budget runs out.
    let private mallEffect (mallChannel: Channel<Request>) meetingCount creatureCount =
        let mutable meetingsLeft = meetingCount
        let mutable waiter = None
        let mutable doneCount = 0

        let rec loop () =
            fio {
                if doneCount < creatureCount then
                    let! request = mallChannel.Read()

                    if meetingsLeft <= 0 then
                        doneCount <- doneCount + 1
                        do! request.ReplyChannel.Write(Stop).Unit()
                    else
                        match waiter with
                        | None ->
                            waiter <- Some request
                        | Some first ->
                            meetingsLeft <- meetingsLeft - 1
                            waiter <- None
                            do! first.ReplyChannel.Write(Paired request.Color).Unit()
                            do! request.ReplyChannel.Write(Paired first.Color).Unit()

                    return! loop ()
            }

        loop ()

    // A creature that repeatedly visits the mall and recolors on each meeting until told to stop.
    let private creatureEffect (mallChannel: Channel<Request>) initialColor =
        let replyChannel = Channel<Response>()

        let rec loop color =
            fio {
                do! mallChannel.Write({ Color = color; ReplyChannel = replyChannel }).Unit()
                match! replyChannel.Read() with
                | Stop ->
                    ()
                | Paired partnerColor ->
                    return! loop (complement (color, partnerColor))
            }

        loop initialColor

    // Builds the Chameneos-Redux workload: creatures rendezvousing at a shared mall.
    let effect creatureCount meetingCount =
        fio {
            let mallChannel = Channel<Request>()
            let colors = [| Red; Yellow; Blue |]

            let creatures =
                [ for index in 0 .. creatureCount - 1 ->
                    creatureEffect mallChannel colors[index % 3] ]

            do! FIO.collectAllParDiscard (mallEffect mallChannel meetingCount creatureCount :: creatures)
        }

// Measures the Chameneos-Redux symmetric rendezvous of color-changing creatures.
[<MemoryDiagnoser>]
[<RankColumn>]
type ChameneosBenchmark() =
    inherit FIOBenchmark()

    member _.CreatureCounts =
        RuntimeParam.intParams "FIO_BENCH_CHAMENEOS_CREATURES" [| 100 |]

    member _.MeetingCounts =
        RuntimeParam.intParams "FIO_BENCH_CHAMENEOS_MEETINGS" [| 100_000 |]

    [<ParamsSource("CreatureCounts")>]
    member val CreatureCount = 0 with get, set

    [<ParamsSource("MeetingCounts")>]
    member val MeetingCount = 0 with get, set

    override this.CreateEffect () =
        Chameneos.effect this.CreatureCount this.MeetingCount
