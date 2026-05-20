/// Fork benchmark — measures fiber creation and scheduling overhead.
[<RequireQualifiedAccess>]
module internal FIO.Benchmarks.Effects.Fork

open FIO.DSL

let private actorEff (doneChan: Channel<int>) = fio { do! doneChan.Send(1).Unit() }

let private forkEff (actorCount, doneChan) =
    let baseEff = actorEff doneChan

    [ 1..actorCount ] |> List.fold (fun acc _ -> actorEff doneChan <&&> acc) baseEff

let effect (actorCount: int) : FIO<unit, exn> =
    fio {
        let doneChan = Channel<int>()
        do! forkEff (actorCount, doneChan)
    }
