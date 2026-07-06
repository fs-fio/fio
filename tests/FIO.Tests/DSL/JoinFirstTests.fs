module FIO.Tests.JoinFirstTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto

open System

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new PollingRuntime() :> FIORuntime
        new SignalingRuntime() :> FIORuntime
        new WorkStealingRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (func: FIORuntime -> unit) =
    testList name
        [ for rt in runtimes () ->
            testCase (rt.GetType().Name) (fun () -> func rt) ]

let private stressTestAllRuntimes name (func: FIORuntime -> unit) =
    testList name
        [ for rt in runtimes () ->
            stressTestCase (rt.GetType().Name) (fun () -> func rt) ]

[<Tests>]
let joinFirstTests =
    testList
        "JoinFirst Primitive"
        [
            testAllRuntimes "joinFirst - fast path returns the lowest index when several fibers are already terminal" (fun runtime ->
                let effect =
                    (FIO.succeed 1).Fork().FlatMap <| fun fiber1 ->
                        (FIO.succeed 2).Fork().FlatMap <| fun fiber2 ->
                            fiber1.Join().FlatMap <| fun _ ->
                                fiber2.Join().FlatMap <| fun _ ->
                                    FIO.joinFirst [fiber1.Context; fiber2.Context]

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 0 "joinFirst should return the lowest terminal index when several fibers have settled")

            testAllRuntimes "joinFirst - blocking path returns the first fiber to settle" (fun runtime ->
                let sentinel = -1

                let effect =
                    (FIO.sleep (TimeSpan.FromSeconds 10.0) (fun _ -> sentinel)).Fork().FlatMap <| fun slowFiber ->
                        (FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun _ -> sentinel)).Fork().FlatMap <| fun fastFiber ->
                            (FIO.joinFirst [slowFiber.Context; fastFiber.Context]).FlatMap <| fun index ->
                                slowFiber.InterruptNow().Ignore().As index

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result 1 "joinFirst should resume with the index of the first fiber to settle")

            testAllRuntimes "joinFirst - a failed fiber counts as settled" (fun runtime ->
                let sentinel = -1

                let effect =
                    (FIO.never<unit, int>()).Fork().FlatMap <| fun neverFiber ->
                        (FIO.fail<unit, int> 7).Fork().FlatMap <| fun failedFiber ->
                            (FIO.joinFirst [neverFiber.Context; failedFiber.Context]).FlatMap <| fun index ->
                                neverFiber.InterruptNow().Ignore().As index

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result 1 "joinFirst should treat failure as settling, not only success")

            testAllRuntimes "joinFirst - parent interruption while parked yields Interrupted without hanging" (fun runtime ->
                let sentinel = -1

                let parkedJoin =
                    (FIO.never<unit, int>()).Fork().FlatMap <| fun never1 ->
                        (FIO.never<unit, int>()).Fork().FlatMap <| fun never2 ->
                            FIO.joinFirst [never1.Context; never2.Context]

                let effect =
                    parkedJoin.Fork().FlatMap <| fun fiber ->
                        (FIO.sleep (TimeSpan.FromMilliseconds 100.0) (fun _ -> sentinel)).FlatMap <| fun () ->
                            fiber.InterruptAwaitNow().FlatMap <| fun result ->
                                match result with
                                | Interrupted _ -> FIO.succeed true
                                | _ -> FIO.succeed false

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.isTrue result "Interrupting the parent should tear down a parked joinFirst as Interrupted")

            testAllRuntimes "joinFirst - single-element list settles with index 0" (fun runtime ->
                let sentinel = -1

                let effect =
                    ((FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun _ -> sentinel))
                        .FlatMap(fun () -> FIO.succeed 9)).Fork().FlatMap <| fun fiber ->
                            (FIO.joinFirst [fiber.Context]).FlatMap <| fun index ->
                                fiber.Join().Map <| fun value -> index, value

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result (0, 9) "joinFirst on a single fiber should settle with index 0 once it completes")

            stressTestAllRuntimes "joinFirst - stress: park races completion without lost wakeups" (fun runtime ->
                let sentinel = -1
                let iterations = 2000

                let rec loop i : FIO<unit, int> =
                    if i = 0 then
                        FIO.unit ()
                    else
                        (FIO.succeed i).Fork().FlatMap <| fun quickFiber ->
                            (FIO.never<int, int>()).Fork().FlatMap <| fun neverFiber ->
                                (FIO.joinFirst [neverFiber.Context; quickFiber.Context]).FlatMap <| fun index ->
                                    neverFiber.InterruptNow().Ignore().FlatMap <| fun () ->
                                        if index = 1 then loop (i - 1) else FIO.fail i

                let bounded =
                    (loop iterations).TimeoutFail sentinel (TimeSpan.FromSeconds 60.0) (fun _ -> sentinel)

                Expect.equal (runtime.Run(bounded).UnsafeSuccess()) () "every joinFirst in the stress loop should settle on the completed fiber")

            testAllRuntimes "joinFirst - empty list interrupts with InvalidArgument" (fun runtime ->
                let result = runtime.Run(FIO.joinFirst<int> []).UnsafeResult()

                match result with
                | Interrupted ex ->
                    match ex.cause with
                    | InvalidArgument _ -> ()
                    | cause -> failtest $"joinFirst on an empty list should interrupt with InvalidArgument, got cause: %A{cause}"
                | other -> failtest $"joinFirst on an empty list should interrupt with InvalidArgument, got: %A{other}")
        ]
