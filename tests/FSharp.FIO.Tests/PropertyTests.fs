(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module FSharp.FIO.Tests.PropertyTests

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp

[<Properties(Arbitrary = [| typeof<PropertyTests> |])>]
type PropertyTests () =

    let result (fiber: Fiber<'R, 'E>) =
        match fiber.Task().GetAwaiter().GetResult() with
        | Ok res -> res
        | Error _ -> failwith "Error happened when result was expected!"

    let error (fiber: Fiber<'R, 'E>) =
        match fiber.Task().GetAwaiter().GetResult() with
        | Ok _ -> failwith "Result happened when error was expected!"
        | Error err -> err
    
    static member Runtime() : Arbitrary<FRuntime> =
        Arb.fromGen <| Gen.oneof [
            Gen.constant (new Direct.Runtime())
            Gen.constant (new Cooperative.Runtime())
            Gen.constant (new Concurrent.Runtime())
        ]

    [<Property>]
    member _.``Functor identity for result`` (runtime: FRuntime, res: int) =
        let eff = FIO.Succeed res
        
        let lhs = eff.Map id
        let rhs = eff
        
        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Functor identity for error`` (runtime: FRuntime, err: int) =
        let eff = FIO.Fail err
        
        let lhs = eff.Map id
        let rhs = eff
        
        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Functor composition for success`` (runtime: FRuntime, res: int) =
        let eff = FIO.Succeed res
        let f x = x + 1
        let g x = x - 2
        
        let lhs = (eff.Map f).Map g
        let rhs = (eff.Map g).Map f
        
        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Functor composition for error`` (runtime: FRuntime, err: int) =
        let eff = FIO.Fail err
        let f x = x + 1
        let g x = x - 2
        
        let lhs = (eff.MapError f).MapError g
        let rhs = (eff.MapError g).MapError f
        
        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Applicative identity for success`` (runtime: FRuntime, res: int) =
        let eff = FIO.Succeed res

        let lhs = eff.Apply <| FIO.Succeed id
        let rhs = eff

        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Applicative identity for error`` (runtime: FRuntime, err: int) =
        let eff = FIO.Fail err
        
        let lhs = eff.ApplyError <| FIO.Fail id
        let rhs = eff
        
        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Applicative homomorphism for success`` (res: int, runtime: FRuntime, f: int -> int) =
        let eff = FIO.Succeed res
        let effF = FIO.Succeed f
        
        let lhs = eff.Apply effF
        let rhs = FIO.Succeed <| f res
        
        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]   
    member _.``Applicative homomorphism for error`` (err: int, runtime: FRuntime, f: int -> int) =
        let eff = FIO.Fail err
        let effF = FIO.Fail f
        
        let lhs = eff.ApplyError effF
        let rhs = FIO.Fail <| f err
        
        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Applicative composition for success`` (f: int -> int, g: int -> int, runtime: FRuntime, res: int) =
        let compose (f: int -> int) (g: int -> int) (x: int) : int =
            f (g x)
        let ff = FIO.Succeed f
        let gg = FIO.Succeed g
        let eff = FIO.Succeed res
        
        let lhs = eff.Apply(ff.Apply(gg.Apply(FIO.Succeed compose)))
        let rhs = eff.Apply(ff).Apply gg
        
        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Applicative composition for error`` (f: int -> int, g: int -> int, runtime: FRuntime, err: int) =
        let compose (f: int -> int) (g: int -> int) (x: int) : int =
            f (g x)
        let ff = FIO.Fail f
        let gg = FIO.Fail g
        let eff = FIO.Fail err
        
        let lhs = eff.ApplyError(ff.ApplyError(gg.ApplyError(FIO.Fail compose)))
        let rhs = eff.ApplyError(ff).ApplyError gg
        
        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Bind identity for success`` (runtime: FRuntime, res: int) =
        let f = FIO.Succeed
        let lhs = (FIO.Succeed res).FlatMap f
        let rhs = f res
        
        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Bind identity for error`` (runtime: FRuntime, err: int) =
        let f = FIO.Fail
        let lhs = (FIO.Fail err).CatchAll f
        let rhs = f err
        
        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Bind associativity for success`` (runtime: FRuntime, res: int) =
        let eff = FIO.Succeed res
        let f = FIO.Succeed
        let g = FIO.Succeed
        
        let lhs = (eff.FlatMap f).FlatMap g
        let rhs = (eff.FlatMap (fun x -> (f x).FlatMap g))
        
        let lhs' = result <| runtime.Run lhs
        let rhs' = result <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Bind associativity for error`` (runtime: FRuntime, err: int) =
        let eff = FIO.Fail err
        let f = FIO.Fail
        let g = FIO.Fail

        let lhs = (eff.CatchAll f).CatchAll g
        let rhs = eff.CatchAll (fun x -> (f x).CatchAll g)

        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``Channel Send and Receive maintains message order`` (runtime: FRuntime, messages: int list) =
        if List.isEmpty messages then
            true
        else
            let eff = fio {
                let chan = Channel<int>()
                for msg in messages do
                    do! (chan.Send msg).FlatMap (fun _ -> FIO.Succeed ())
                let mutable received = []
                for _ in messages do
                    let! msg = chan.Receive ()
                    received <- msg :: received
                return List.rev received
            }
            let received = result <| runtime.Run eff
            received = messages

    [<Property>]
    member _.``Channel operations compose with Bind correctly`` (runtime: FRuntime, msg: int) =
        let eff = fio {
            let chan = Channel<int>()
            let! sentMsg = chan.Send msg
            let! receivedMsg = chan.Receive ()
            return sentMsg = receivedMsg && receivedMsg = msg
        }
        result <| runtime.Run eff

    [<Property>]
    member _.``Channel Count is accurate after operations`` (runtime: FRuntime, messages: int list) =
        if List.isEmpty messages then
            true
        else
            let eff = fio {
                let chan = Channel<int>()
                for msg in messages do
                    do! (chan.Send msg).FlatMap (fun _ -> FIO.Succeed ())
                let countAfterSend = chan.Count
                let mutable received = []
                for _ in messages do
                    let! msg = chan.Receive ()
                    received <- msg :: received
                let countAfterReceive = chan.Count
                return countAfterSend = int64 (List.length messages) && countAfterReceive = 0L
            }
            result <| runtime.Run eff

    [<Property>]
    member _.``Channel with no receivers maintains all messages`` (runtime: FRuntime, messages: int list) =
        if List.isEmpty messages then
            true
        else
            let eff = fio {
                let chan = Channel<int>()
                for msg in messages do
                    do! (chan.Send msg).FlatMap (fun _ -> FIO.Succeed ())
                return chan.Count = int64 (List.length messages)
            }
            result <| runtime.Run eff

    [<Property>]
    member _.``Fork then Await equals identity`` (runtime: FRuntime, res: int) =
        let eff = fio {
            let! fiber = (FIO.Succeed res).Fork ()
            let! result = fiber.Join ()
            return result
        }
        let forkAwaitResult = result <| runtime.Run eff
        let directResult = res
        forkAwaitResult = directResult

    [<Property>]
    member _.``Fiber ID is unique per fork`` (runtime: FRuntime, res: int) =
        let eff = fio {
            let effect = FIO.Succeed res
            let! fiber1 = effect.Fork ()
            let! fiber2 = effect.Fork ()
            let! fiber3 = effect.Fork ()
            return fiber1.Id <> fiber2.Id && fiber2.Id <> fiber3.Id && fiber1.Id <> fiber3.Id
        }
        result <| runtime.Run eff

    [<Property>]
    member _.``MapError composes correctly`` (runtime: FRuntime, err: int) =
        let eff : FIO<int, int> = FIO.Fail err
        let f = fun x -> x + 10
        let g = fun x -> x * 2

        let intermediate : FIO<int, int> = eff.MapError f
        let lhs : FIO<int, int> = intermediate.MapError g
        let rhs : FIO<int, int> = eff.MapError (fun x -> g (f x))

        let lhs' = error <| runtime.Run lhs
        let rhs' = error <| runtime.Run rhs
        lhs' = rhs'

    [<Property>]
    member _.``BindError propagates errors correctly`` (runtime: FRuntime, err: int) =
        let eff = FIO.Fail err
        let recover x = FIO.Fail (x + 100)

        let transformed = eff.CatchAll recover
        let expected = err + 100

        let actual = error <| runtime.Run transformed
        actual = expected

    [<Property>]
    member _.``Parallel effects with mixed results handle errors`` (runtime: FRuntime, res: int, err: int) =
        let successEff = FIO.Succeed res
        let failEff = FIO.Fail err

        let parallelEff = successEff.ZipPar failEff

        let actualErr = error <| runtime.Run parallelEff
        actualErr = err

    [<Property>]
    member _.``MapBoth transforms success value when effect succeeds`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).MapBoth((fun r -> r * 2), (fun e -> e + 100))
        let actual = result <| runtime.Run eff
        let expected = res * 2
        actual = expected

    [<Property>]
    member _.``MapBoth transforms error value when effect fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).MapBoth((fun r -> r * 2), (fun e -> e + 100))
        let actual = error <| runtime.Run eff
        let expected = err + 100
        actual = expected

    [<Property>]
    member _.``MapBoth composes MapError and Map in correct order`` (runtime: FRuntime, res: int) =
        let eff = FIO.Succeed res
        let bimap = eff.MapBoth((fun r -> r + 1), string)
        let manual = eff.MapError(string).Map(fun r -> r + 1)
        let actualBimap = result <| runtime.Run bimap
        let actualManual = result <| runtime.Run manual
        actualBimap = actualManual

    [<Property>]
    member _.``Tap executes side effect and preserves original success value`` (runtime: FRuntime, res: int) =
        let mutable sideEffectValue = 0
        let eff = (FIO.Succeed res).Tap(fun r ->
            FIO.Succeed (sideEffectValue <- r * 2))
        let actual = result <| runtime.Run eff
        actual = res && sideEffectValue = res * 2

    [<Property>]
    member _.``Tap propagates error from tap effect`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Succeed res).Tap(fun _ -> FIO.Fail err)
        let actual = error <| runtime.Run eff
        actual = err

    [<Property>]
    member _.``Tap does not execute when effect fails`` (runtime: FRuntime, err: int) =
        let mutable executed = false
        let eff = (FIO.Fail err).Tap(fun _ ->
            FIO.Succeed (executed <- true))
        let actual = error <| runtime.Run eff
        actual = err && not executed

    [<Property>]
    member _.``Tap discards result of tap effect`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Tap(fun _ -> FIO.Succeed 999)
        let actual = result <| runtime.Run eff
        actual = res

    [<Property>]
    member _.``TapError executes side effect and preserves original error value`` (runtime: FRuntime, err: int) =
        let mutable sideEffectValue = 0
        let eff = (FIO.Fail err).TapError(fun e ->
            FIO.Succeed (sideEffectValue <- e * 2))
        let actual = error <| runtime.Run eff
        actual = err && sideEffectValue = err * 2

    [<Property>]
    member _.``TapError propagates error from tap effect`` (runtime: FRuntime, err: int, newErr: int) =
        let eff = (FIO.Fail err).TapError(fun _ -> FIO.Fail newErr)
        let actual = error <| runtime.Run eff
        actual = newErr

    [<Property>]
    member _.``TapError does not execute when effect succeeds`` (runtime: FRuntime, res: int) =
        let mutable executed = false
        let eff = (FIO.Succeed res).TapError(fun _ ->
            FIO.Succeed (executed <- true))
        let actual = result <| runtime.Run eff
        actual = res && not executed

    [<Property>]
    member _.``TapError discards result of tap effect`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).TapError(fun _ -> FIO.Succeed 999)
        let actual = error <| runtime.Run eff
        actual = err

    [<Property>]
    member _.``Fold handles success case with pure function`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Fold((fun e -> e), (fun r -> r * 2))
        let actual = result <| runtime.Run eff
        let expected = res * 2
        actual = expected

    [<Property>]
    member _.``Fold handles error case with pure function`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Fold((fun e -> e + 100), (fun r -> r * 2))
        let actual = result <| runtime.Run eff
        let expected = err + 100
        actual = expected

    [<Property>]
    member _.``Fold produces infallible result`` (runtime: FRuntime, res: int, err: int) =
        let successEff = (FIO.Succeed res).Fold((fun _ -> -1), (fun r -> r + 1))
        let errorEff = (FIO.Fail err).Fold((fun e -> e * 2), (fun _ -> 0))

        let actualSuccess = result <| runtime.Run successEff
        let actualError = result <| runtime.Run errorEff

        actualSuccess = (res + 1) && actualError = (err * 2)

    [<Property>]
    member _.``FoldFIO handles success case with effectful function`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).FoldFIO((fun e -> FIO.Succeed e), (fun r -> FIO.Succeed (r * 2)))
        let actual = result <| runtime.Run eff
        let expected = res * 2
        actual = expected

    [<Property>]
    member _.``FoldFIO handles error case with effectful function`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).FoldFIO((fun e -> FIO.Succeed (e + 100)), (fun r -> FIO.Succeed (r * 2)))
        let actual = result <| runtime.Run eff
        let expected = err + 100
        actual = expected

    [<Property>]
    member _.``FoldFIO error handler catches errors from success handler`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Succeed res).FoldFIO((fun e -> FIO.Succeed (e * 10)), (fun _ -> FIO.Fail err))
        let actual = result <| runtime.Run eff
        let expected = err * 10
        actual = expected

    [<Property>]
    member _.``FoldFIO propagates error when error handler fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).FoldFIO((fun _ -> FIO.Fail err2), (fun r -> FIO.Succeed r))
        let actual = error <| runtime.Run eff
        actual = err2

    [<Property>]
    member _.``Retry does not retry when effect succeeds immediately`` (runtime: FRuntime, res: int) =
        let mutable attempts = 0
        let eff = FIO.Attempt (fun () -> attempts <- attempts + 1; res)
        let retried = eff.RetryN 3

        let actual = result <| runtime.Run retried
        actual = res && attempts = 1

    [<Property>]
    member _.``Retry retries failed effect up to max times`` (runtime: FRuntime) =
        let mutable attempts = 0
        let eff = fio {
            attempts <- attempts + 1
            return! FIO.Fail "error"
        }
        let retried = eff.RetryN 4

        let _ = error <| runtime.Run retried
        attempts = 4

    [<Property>]
    member _.``Retry succeeds on intermediate retry`` (runtime: FRuntime, res: int) =
        let mutable attempts = 0
        let eff = fio {
            attempts <- attempts + 1
            if attempts < 3 then
                return! FIO.Fail "error"
            else
                return res
        }
        let retried = eff.RetryN 5

        let actual = result <| runtime.Run retried
        actual = res && attempts = 3

    [<Property>]
    member _.``Retry final failure after max retries`` (runtime: FRuntime) =
        let eff = FIO.Fail "error"
        let retried = eff.RetryN 2

        let actual = error <| runtime.Run retried
        actual = "error"

    [<Property>]
    member _.``Retry preserves success value when retrying`` (runtime: FRuntime, res: int) =
        let mutable attempts = 0
        let eff = FIO.Attempt (fun () ->
            attempts <- attempts + 1
            if attempts = 1 then failwith "Fail once" else res)

        let retried = eff.RetryN 5

        let actual = result <| runtime.Run retried
        actual = res
