module FSharp.FIO.Tests.PropertyTests

open FSharp.FIO.Tests.Utilities.FsCheckProperties

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open Expecto
open FsCheck

[<Tests>]
let mapLaws =
    testList "Map Laws" [

        // Law: eff.Map(res => res) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map identity law (success)"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res

            let lhs = eff.Map id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Map identity law should hold for success"

        // Law: eff.Map(res => res) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map identity law (error)"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err

            let lhs = eff.Map id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "Map identity law should hold for error"

        // Law: eff.MapError(err => err) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError identity law (success)"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res

            let lhs = eff.MapError id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapError identity law should hold for success"

        // Law: eff.MapError(err => err) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError identity law (error)"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err

            let lhs = eff.MapError id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapError identity law should hold for error"

        // Law: eff.Map(f).Map(g) == eff.Map(f andThen g) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map composition law (success)"
        <| fun (runtime: FRuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.Succeed res

            let lhs = eff.Map(f).Map g
            let rhs = eff.Map(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Map composition law should hold for success"

        // Law: eff.Map(f).Map(g) == eff.Map(f andThen g) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map composition law (error)"
        <| fun (runtime: FRuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.Fail err

            let lhs = eff.Map(f).Map g
            let rhs = eff.Map(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "Map composition law should hold for error"

        // Law: eff.MapError(f).MapError(g) == eff.MapError(f andThen g) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError composition law (success)"
        <| fun (runtime: FRuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.Succeed res

            let lhs = eff.MapError(f).MapError g
            let rhs = eff.MapError(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapError composition law should hold for success"

        // Law: eff.MapError(f).MapError(g) == eff.MapError(f andThen g) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError composition law (error)"
        <| fun (runtime: FRuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.Fail err

            let lhs = eff.MapError(f).MapError g
            let rhs = eff.MapError(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapError composition law should hold for error"

        // Law: eff.MapBoth(res => res, err => err) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth identity law (success)"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res

            let lhs = eff.MapBoth(id, id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapBoth identity law should hold for success"

        // Law: eff.MapBoth(res => res, err => err) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth identity law (error)"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err

            let lhs = eff.MapBoth(id, id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapBoth identity law should hold for error"

        // Law: eff.MapBoth(f1 >> f2, g1 >> g2) == eff.MapBoth(f1, g1).MapBoth(f2, g2) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth composition law (success)"
        <| fun (runtime: FRuntime, res: int, f1: int -> int, f2: int -> int, g1: int -> int, g2: int -> int) ->
            let eff = FIO.Succeed res

            let lhs = eff.MapBoth(f1 >> f2, g1 >> g2)
            let rhs = eff.MapBoth(f1, g1).MapBoth(f2, g2)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapBoth composition law should hold for success"

        // Law: eff.MapBoth(f1 >> f2, g1 >> g2) == eff.MapBoth(f1, g1).MapBoth(f2, g2) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth composition law (error)"
        <| fun (runtime: FRuntime, err: int, f1: int -> int, f2: int -> int, g1: int -> int, g2: int -> int) ->
            let eff = FIO.Fail err

            let lhs = eff.MapBoth(f1 >> f2, g1 >> g2)
            let rhs = eff.MapBoth(f1, g1).MapBoth(f2, g2)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapBoth composition law should hold for error"

        // TODO: Perhaps move to another file?
        // Law: eff.MapBoth(f, g) == eff.Map(f).MapError(g) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth consistency (success)"
        <| fun (runtime: FRuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.Succeed res

            let lhs = eff.MapBoth(f, g)
            let rhs = eff.Map(f).MapError g

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapBoth consistency should hold for success"

        // TODO: Perhaps move to another file?
        // Law: eff.MapBoth(f, g) == eff.Map(f).MapError(g) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth consistency (error)"
        <| fun (runtime: FRuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.Fail err

            let lhs = eff.MapBoth(f, g)
            let rhs = eff.Map(f).MapError g

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapBoth consistency should hold for error"

        // TODO: Perhaps move to another file?
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth transforms success value when effect succeeds"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed(res).MapBoth((fun r -> r * 2), (fun e -> e + 100))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = res * 2

            Expect.equal actual expected "MapBoth should transform success value correctly"

        // TODO: Perhaps move to another file?
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth transforms error value when effect fails"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail(err).MapBoth((fun r -> r * 2), (fun e -> e + 100))
            let actual = runtime.Run(eff).UnsafeError()
            let expected = err + 100

            Expect.equal actual expected "MapBoth should transform error value correctly"
    ]

[<Tests>]
let applicativeLaws =
    testList "Applicative Laws" [

        // Law: eff.Apply(Succeed id) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply identity (success)"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res

            let lhs = eff.Apply(FIO.Succeed id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply identity law should hold for success"

        // Law: eff.Apply(Succeed id) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply identity (error)"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err

            let lhs = eff.Apply(FIO.Succeed id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "Apply identity law should hold for error"

        // Law: eff.ApplyError(Fail id) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError identity (success)"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res

            let lhs = eff.ApplyError(FIO.Fail id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "ApplyError identity law should hold for success"

        // Law: eff.ApplyError(Fail id) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError identity (error)"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err

            let lhs = eff.ApplyError(FIO.Fail id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError identity law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply composition (success)"
        <| fun (runtime: FRuntime, res: int, f: int -> int, g: int -> int) ->
            let compose (f: int -> int) (g: int -> int) (x: int) : int =
                f (g x)

            let eff = FIO.Succeed res
            let ff = FIO.Succeed f
            let gg = FIO.Succeed g

            let lhs = eff.Apply(ff.Apply(gg.Apply(FIO.Succeed compose)))
            let rhs = eff.Apply(ff).Apply gg

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply composition law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError composition (error)"
        <| fun (runtime: FRuntime, err: int, f: int -> int, g: int -> int) ->
            let compose (f: int -> int) (g: int -> int) (x: int) : int =
                f (g x)
            
            let eff = FIO.Fail err
            let ff = FIO.Fail f
            let gg = FIO.Fail g

            let lhs = eff.ApplyError(ff.ApplyError(gg.ApplyError(FIO.Fail compose)))
            let rhs = eff.ApplyError(ff).ApplyError gg

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError composition law should hold for error"

        // Law: eff.Apply(Succeed f) == Succeed(f res) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply homomorphism (success)"
        <| fun (runtime: FRuntime, res: int, f: int -> int) ->
            let eff = FIO.Succeed res
            let effF = FIO.Succeed f

            let lhs = eff.Apply effF
            let rhs = FIO.Succeed(f res)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply homomorphism law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError homomorphism (error)"
        <| fun (runtime: FRuntime, err: int, f: int -> int) ->
            let eff = FIO.Fail err
            let effF = FIO.Fail f

            let lhs = eff.ApplyError effF
            let rhs = FIO.Fail(f err)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError homomorphism law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply interchange (success)"
        <| fun (runtime: FRuntime, res: int, f: int -> int) ->
            let eff = FIO.Succeed f

            let lhs = FIO.Succeed(res).Apply eff
            let rhs = eff.Apply(FIO.Succeed(fun g -> g res))

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply interchange law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError interchange (error)"
        <| fun (runtime: FRuntime, err: int, f: int -> int) ->
            let eff = FIO.Fail f

            let lhs = FIO.Fail(err).ApplyError eff
            let rhs = eff.ApplyError(FIO.Fail(fun g -> g err))

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError interchange law should hold for error"
       ]

[<Tests>]
let monadLaws =
    testList "Monad Laws" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "FlatMap left identity for success"
        <| fun (runtime: FRuntime, res: int) ->
            let f = FIO.Succeed

            let lhs = (FIO.Succeed res).FlatMap f
            let rhs = f res

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "FlatMap left identity law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FlatMap right identity for success"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res

            let lhs = eff.FlatMap FIO.Succeed
            let rhs = eff

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "FlatMap right identity law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FlatMap associativity for success"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res
            let f = FIO.Succeed
            let g = FIO.Succeed

            let lhs = (eff.FlatMap f).FlatMap g
            let rhs = eff.FlatMap(fun x -> (f x).FlatMap g)

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "FlatMap associativity law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll left identity for error"
        <| fun (runtime: FRuntime, err: int) ->
            let f = FIO.Fail

            let lhs = (FIO.Fail err).CatchAll f
            let rhs = f err

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "CatchAll left identity law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll right identity for error"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err

            let lhs = eff.CatchAll FIO.Fail
            let rhs = eff

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "CatchAll right identity law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll associativity for error"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err
            let f = FIO.Fail
            let g = FIO.Fail

            let lhs = (eff.CatchAll f).CatchAll g
            let rhs = eff.CatchAll(fun x -> (f x).CatchAll g)

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "CatchAll associativity law should hold for error"
    ]

[<Tests>]
let errorHandling =
    testList "Error Handling" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError composes correctly"
        <| fun (runtime: FRuntime, err: int) ->
            let eff : FIO<int, int> = FIO.Fail err
            let f = fun x -> x + 10
            let g = fun x -> x * 2

            let intermediate : FIO<int, int> = eff.MapError f
            let lhs : FIO<int, int> = intermediate.MapError g
            let rhs : FIO<int, int> = eff.MapError (fun x -> g (f x))

            let lhs' = (runtime.Run lhs).UnsafeResult()
            let rhs' = (runtime.Run rhs).UnsafeResult()

            Expect.equal lhs' rhs' "MapError should compose correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll propagates errors correctly"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = FIO.Fail err
            let recover x = FIO.Fail (x + 100)

            let transformed = eff.CatchAll recover
            let expected = Error (err + 100)

            let actual = (runtime.Run transformed).UnsafeResult()

            Expect.equal actual expected "BindError should propagate errors correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipPar effects with mixed results handle errors"
        <| fun (runtime: FRuntime, res: int, err: int) ->
            let successEff = FIO.Succeed res
            let failEff = FIO.Fail err

            let parallelEff = successEff.ZipPar failEff

            let actualErr = (runtime.Run parallelEff).UnsafeError()

            Expect.equal actualErr err "ZipPar should propagate error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth composes MapError and Map in correct order"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = FIO.Succeed res
            let bimap = eff.MapBoth((fun r -> r + 1), string)
            let manual = eff.MapError(string).Map(fun r -> r + 1)

            let actualBimap = (runtime.Run bimap).UnsafeResult()
            let actualManual = (runtime.Run manual).UnsafeResult()

            Expect.equal actualBimap actualManual "MapBoth should compose correctly"
    ]

[<Tests>]
let sideEffects =
    testList "Side Effects (Tap/TapError)" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap executes side effect and preserves original success value"
        <| fun (runtime: FRuntime, res: int) ->
            let mutable sideEffectValue = 0
            let eff = (FIO.Succeed res).Tap(fun r ->
                FIO.Succeed (sideEffectValue <- r * 2))

            let actual = (runtime.Run eff).UnsafeSuccess()

            Expect.equal actual res "Tap should preserve original value" .&.
            Expect.equal sideEffectValue (res * 2) "Tap should execute side effect"
    

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap propagates error from tap effect"
        <| fun (runtime: FRuntime, res: int, err: int) ->
            let eff = (FIO.Succeed res).Tap(fun _ -> FIO.Fail err)

            let actual = (runtime.Run eff).UnsafeError()

            Expect.equal actual err "Tap should propagate error from tap effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap does not execute when effect fails"
        <| fun (runtime: FRuntime, err: int) ->
            let mutable executed = false
            let eff = (FIO.Fail err).Tap(fun _ ->
                FIO.Succeed (executed <- true))

            let actual = (runtime.Run eff).UnsafeError()

            Expect.equal actual err "Tap should preserve error" .&.
            Expect.isFalse executed "Tap should not execute on failure"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap discards result of tap effect"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = (FIO.Succeed res).Tap(fun _ -> FIO.Succeed 999)

            let actual = (runtime.Run eff).UnsafeSuccess()

            Expect.equal actual res "Tap should discard tap effect result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError executes side effect and preserves original error value"
        <| fun (runtime: FRuntime, err: int) ->
            let mutable sideEffectValue = 0
            let eff = (FIO.Fail err).TapError(fun e ->
                FIO.Succeed (sideEffectValue <- e * 2))

            let actual = (runtime.Run eff).UnsafeError()

            Expect.equal actual err "TapError should preserve error" .&.
            Expect.equal sideEffectValue (err * 2) "TapError should execute side effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError propagates error from tap effect"
        <| fun (runtime: FRuntime, err: int, newErr: int) ->
            let eff = (FIO.Fail err).TapError(fun _ -> FIO.Fail newErr)

            let actual = (runtime.Run eff).UnsafeError()

            Expect.equal actual newErr "TapError should propagate error from tap effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError does not execute when effect succeeds"
        <| fun (runtime: FRuntime, res: int) ->
            let mutable executed = false
            let eff = (FIO.Succeed res).TapError(fun _ ->
                FIO.Succeed (executed <- true))

            let actual = (runtime.Run eff).UnsafeSuccess()

            Expect.equal actual res "TapError should preserve success" .&.
            Expect.isFalse executed "TapError should not execute on success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError discards result of tap effect"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = (FIO.Fail err).TapError(fun _ -> FIO.Succeed 999)

            let actual = (runtime.Run eff).UnsafeError()

            Expect.equal actual err "TapError should discard tap effect result"
    ]

[<Tests>]
let foldOperations =
    testList "Fold Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fold handles success case with pure function"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = (FIO.Succeed res).Fold((fun e -> e), (fun r -> r * 2))

            let actual = (runtime.Run eff).UnsafeSuccess()
            let expected = res * 2

            Expect.equal actual expected "Fold should handle success correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fold handles error case with pure function"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = (FIO.Fail err).Fold((fun e -> e + 100), (fun r -> r * 2))

            let actual = (runtime.Run eff).UnsafeSuccess()
            let expected = err + 100

            Expect.equal actual expected "Fold should handle error correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fold produces infallible result"
        <| fun (runtime: FRuntime, res: int, err: int) ->
            let successEff = (FIO.Succeed res).Fold((fun _ -> -1), (fun r -> r + 1))
            let errorEff = (FIO.Fail err).Fold((fun e -> e * 2), (fun _ -> 0))

            let actualSuccess = (runtime.Run successEff).UnsafeSuccess()
            let actualError = (runtime.Run errorEff).UnsafeSuccess()

            Expect.equal actualSuccess (res + 1) "Fold success case" .&.
            Expect.equal actualError (err * 2) "Fold error case"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FoldFIO handles success case with effectful function"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = (FIO.Succeed res).FoldFIO((fun e -> FIO.Succeed e), (fun r -> FIO.Succeed (r * 2)))

            let actual = (runtime.Run eff).UnsafeSuccess()
            let expected = res * 2

            Expect.equal actual expected "FoldFIO should handle success correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FoldFIO handles error case with effectful function"
        <| fun (runtime: FRuntime, err: int) ->
            let eff = (FIO.Fail err).FoldFIO((fun e -> FIO.Succeed (e + 100)), (fun r -> FIO.Succeed (r * 2)))

            let actual = (runtime.Run eff).UnsafeSuccess()
            let expected = err + 100

            Expect.equal actual expected "FoldFIO should handle error correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FoldFIO error handler catches errors from success handler"
        <| fun (runtime: FRuntime, res: int, err: int) ->
            let eff = (FIO.Succeed res).FoldFIO((fun e -> FIO.Succeed (e * 10)), (fun _ -> FIO.Fail err))

            let actual = (runtime.Run eff).UnsafeSuccess()
            let expected = err * 10

            Expect.equal actual expected "FoldFIO error handler should catch errors"
    ]

[<Tests>]
let retryOperations =
    testList "Retry Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry does not retry when effect succeeds immediately"
        <| fun (runtime: FRuntime, res: int) ->
            let mutable attempts = 0
            let eff = FIO.Attempt (fun () -> attempts <- attempts + 1; res)
            let retried = eff.RetryN 3

            let actual = (runtime.Run retried).UnsafeSuccess()

            Expect.equal actual res "RetryN should succeed" .&.
            Expect.equal attempts 1 "RetryN should not retry on immediate success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry retries failed effect up to max times"
        <| fun (runtime: FRuntime) ->
            let mutable attempts = 0
            let eff = fio {
                attempts <- attempts + 1
                return! FIO.Fail "error"
            }
            let retried = eff.RetryN 4

            let _ = (runtime.Run retried).UnsafeResult()

            Expect.equal attempts 4 "RetryN should retry up to max attempts"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry succeeds on intermediate retry"
        <| fun (runtime: FRuntime, res: int) ->
            let mutable attempts = 0
            let eff = fio {
                attempts <- attempts + 1
                if attempts < 3 then
                    return! FIO.Fail "error"
                else
                    return res
            }
            let retried = eff.RetryN 5

            let actual = (runtime.Run retried).UnsafeSuccess()

            Expect.equal actual res "RetryN should succeed" .&.
            Expect.equal attempts 3 "RetryN should succeed on third attempt"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry final failure after max retries"
        <| fun (runtime: FRuntime) ->
            let eff = FIO.Fail "error"
            let retried = eff.RetryN 2

            let actual = (runtime.Run retried).UnsafeError()

            Expect.equal actual "error" "RetryN should fail after max retries"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry preserves success value when retrying"
        <| fun (runtime: FRuntime, res: int) ->
            let mutable attempts = 0
            let eff = FIO.Attempt (fun () ->
                attempts <- attempts + 1
                if attempts = 1 then failwith "Fail once" else res)

            let retried = eff.RetryN 5

            let actual = (runtime.Run retried).UnsafeSuccess()

            Expect.equal actual res "RetryN should preserve success value"
    ]

[<Tests>]
let channelOperations =
    testList "Channel Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel Send and Receive maintains message order"
        <| fun (runtime: FRuntime, messages: int list) ->
            if List.isEmpty messages then
                ()
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
                let result = (runtime.Run eff).UnsafeSuccess()
                Expect.equal result messages "Channel should maintain message order"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel operations compose with Bind correctly"
        <| fun (runtime: FRuntime, msg: int) ->
            let eff = fio {
                let chan = Channel<int>()
                let! sentMsg = chan.Send msg
                let! receivedMsg = chan.Receive ()
                return sentMsg = receivedMsg && receivedMsg = msg
            }
            let result = (runtime.Run eff).UnsafeSuccess()
            Expect.isTrue result "Channel send/receive should compose correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel Count is accurate after operations"
        <| fun (runtime: FRuntime, messages: int list) ->
            if List.isEmpty messages then
                ()
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
                let result = (runtime.Run eff).UnsafeSuccess()
                Expect.isTrue result "Channel count should be accurate"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel with no receivers maintains all messages"
        <| fun (runtime: FRuntime, messages: int list) ->
            if List.isEmpty messages then
                ()
            else
                let eff = fio {
                    let chan = Channel<int>()
                    for msg in messages do
                        do! (chan.Send msg).FlatMap (fun _ -> FIO.Succeed ())
                    return chan.Count = int64 (List.length messages)
                }
                let result = (runtime.Run eff).UnsafeSuccess()
                Expect.isTrue result "Channel should maintain all messages"
    ]

[<Tests>]
let fiberOperations =
    testList "Fiber Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fork then Join equals identity"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = fio {
                let! fiber = (FIO.Succeed res).Fork()
                let! result = fiber.Join()
                return result
            }
            let forkAwaitResult = (runtime.Run eff).UnsafeSuccess()
            Expect.equal forkAwaitResult res "Fork then Join should equal identity"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber ID is unique per fork"
        <| fun (runtime: FRuntime, res: int) ->
            let eff = fio {
                let effect = FIO.Succeed res
                let! fiber1 = effect.Fork()
                let! fiber2 = effect.Fork()
                let! fiber3 = effect.Fork()
                return fiber1.Id <> fiber2.Id && fiber2.Id <> fiber3.Id && fiber1.Id <> fiber3.Id
            }
            let result = (runtime.Run eff).UnsafeSuccess()
            Expect.isTrue result "Fiber IDs should be unique"
    ]
