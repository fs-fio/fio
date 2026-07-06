module FIO.Tests.JoinAllFailFastTests

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
let joinAllFailFastTests =
    testList
        "JoinAllFailFast Primitive"
        [
            testAllRuntimes "joinAllFailFast - fast path returns the lowest index when several failures are already terminal" (fun runtime ->
                let effect =
                    (FIO.succeed 1).Fork().FlatMap <| fun okFiber ->
                        (FIO.fail<int, int> 7).Fork().FlatMap <| fun failed1 ->
                            (FIO.fail<int, int> 8).Fork().FlatMap <| fun failed2 ->
                                okFiber.Join().FlatMap <| fun _ ->
                                    failed1.Await().FlatMap <| fun _ ->
                                        failed2.Await().FlatMap <| fun _ ->
                                            FIO.joinAllFailFast [| okFiber.Context; failed1.Context; failed2.Context |]

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (ValueSome 1) "joinAllFailFast should return the lowest failed index among terminal fibers")

            testAllRuntimes "joinAllFailFast - all successes settle with ValueNone and results are readable" (fun runtime ->
                let sentinel = -1

                let effect =
                    ((FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun _ -> sentinel)).FlatMap(fun () -> FIO.succeed 1)).Fork().FlatMap <| fun fiber1 ->
                        (FIO.succeed 2).Fork().FlatMap <| fun fiber2 ->
                            ((FIO.sleep (TimeSpan.FromMilliseconds 20.0) (fun _ -> sentinel)).FlatMap(fun () -> FIO.succeed 3)).Fork().FlatMap <| fun fiber3 ->
                                (FIO.joinAllFailFast [| fiber1.Context; fiber2.Context; fiber3.Context |]).FlatMap <| fun outcome ->
                                    fiber1.Join().FlatMap <| fun value1 ->
                                        fiber2.Join().FlatMap <| fun value2 ->
                                            fiber3.Join().Map <| fun value3 ->
                                                outcome, value1 + value2 + value3

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result (ValueNone, 6) "joinAllFailFast should settle with ValueNone once every fiber has succeeded")

            testAllRuntimes "joinAllFailFast - fails fast when a later fiber fails behind a never-terminating peer" (fun runtime ->
                let sentinel = -1

                let effect =
                    (FIO.never<int, int>()).Fork().FlatMap <| fun neverFiber ->
                        ((FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun _ -> 0)).FlatMap(fun () -> FIO.fail<int, int> 99)).Fork().FlatMap <| fun failingFiber ->
                            (FIO.joinAllFailFast [| neverFiber.Context; failingFiber.Context |]).FlatMap <| fun outcome ->
                                neverFiber.InterruptNow().Ignore().As outcome

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result (ValueSome 1) "joinAllFailFast should observe the late failure without hanging on the stuck peer")

            testAllRuntimes "joinAllFailFast - an interrupted fiber counts as a non-success" (fun runtime ->
                let sentinel = -1

                let effect =
                    (FIO.never<int, int>()).Fork().FlatMap <| fun interruptedFiber ->
                        (FIO.never<int, int>()).Fork().FlatMap <| fun neverFiber ->
                            (interruptedFiber.InterruptNow().Ignore()).FlatMap <| fun () ->
                                (FIO.joinAllFailFast [| interruptedFiber.Context; neverFiber.Context |]).FlatMap <| fun outcome ->
                                    neverFiber.InterruptNow().Ignore().As outcome

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result (ValueSome 0) "joinAllFailFast should treat an interrupted fiber as settling non-successfully")

            testAllRuntimes "joinAllFailFast - parent interruption while parked yields Interrupted without hanging" (fun runtime ->
                let sentinel = -1

                let parkedJoin =
                    (FIO.never<unit, int>()).Fork().FlatMap <| fun never1 ->
                        (FIO.never<unit, int>()).Fork().FlatMap <| fun never2 ->
                            FIO.joinAllFailFast [| never1.Context; never2.Context |]

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

                Expect.isTrue result "Interrupting the parent should tear down a parked joinAllFailFast as Interrupted")

            testAllRuntimes "joinAllFailFast - empty array settles immediately with ValueNone" (fun runtime ->
                let result = runtime.Run(FIO.joinAllFailFast<int> [||]).UnsafeSuccess()

                Expect.equal result ValueNone "joinAllFailFast on an empty array should succeed immediately with ValueNone")

            testAllRuntimes "joinAllFailFast - single failing fiber settles with ValueSome 0" (fun runtime ->
                let sentinel = -1

                let effect =
                    ((FIO.sleep (TimeSpan.FromMilliseconds 50.0) (fun _ -> 0)).FlatMap(fun () -> FIO.fail<int, int> 5)).Fork().FlatMap <| fun fiber ->
                        FIO.joinAllFailFast [| fiber.Context |]

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 5.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result (ValueSome 0) "joinAllFailFast on a single failing fiber should settle with its index")

            stressTestAllRuntimes "joinAllFailFast - stress: park races completion without lost wakeups" (fun runtime ->
                let sentinel = -1
                let iterations = 1000

                let rec loop i : FIO<unit, int> =
                    if i = 0 then
                        FIO.unit ()
                    else
                        (FIO.succeed 1).Fork().FlatMap <| fun fiber1 ->
                            (FIO.succeed 2).Fork().FlatMap <| fun fiber2 ->
                                (FIO.succeed 3).Fork().FlatMap <| fun fiber3 ->
                                    (FIO.joinAllFailFast [| fiber1.Context; fiber2.Context; fiber3.Context |]).FlatMap <| fun outcome ->
                                        match outcome with
                                        | ValueNone -> loop (i - 1)
                                        | ValueSome _ -> FIO.fail i

                let bounded =
                    (loop iterations).TimeoutFail sentinel (TimeSpan.FromSeconds 60.0) (fun _ -> sentinel)

                Expect.equal (runtime.Run(bounded).UnsafeSuccess()) () "every joinAllFailFast in the stress loop should settle with ValueNone")

            testAllRuntimes "joinAllFailFast - settles across a large fan-out" (fun runtime ->
                let sentinel = -1
                let count = 10000

                let rec forkAll i (forked: Fiber<int, int> list) =
                    if i >= count then
                        FIO.succeed (List.rev forked |> List.toArray)
                    else
                        (FIO.succeed i).Fork().FlatMap <| fun fiber ->
                            forkAll (i + 1) (fiber :: forked)

                let effect =
                    (forkAll 0 []).FlatMap <| fun fibers ->
                        FIO.joinAllFailFast (fibers |> Array.map (fun fiber -> fiber.Context))

                let bounded =
                    effect.TimeoutFail sentinel (TimeSpan.FromSeconds 10.0) (fun _ -> sentinel)

                let result = runtime.Run(bounded).UnsafeSuccess()

                Expect.equal result ValueNone "joinAllFailFast should settle with ValueNone across a 10k-fiber fan-out")
        ]
