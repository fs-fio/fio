[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Fork

open FIO.DSL

let private actorEff (doneChan: Channel<int>) = fio { do! doneChan.Write(1).Unit() }

let private forkEff (actorCount, doneChan) =
    [ 1..actorCount ] |> List.fold (fun acc _ -> actorEff doneChan <&&> acc) (FIO.unit ())

let effect (actorCount: int) : FIO<unit, exn> =
    fio {
        let doneChan = Channel<int>()
        do! forkEff (actorCount, doneChan)

        for _ in 1..actorCount do
            do! doneChan.Read().Unit()
    }
