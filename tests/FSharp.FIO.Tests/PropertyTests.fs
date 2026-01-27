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
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res

            let lhs = eff.Map id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Map identity law should hold for success"

        // Law: eff.Map(res => res) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map identity law (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err

            let lhs = eff.Map id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "Map identity law should hold for error"

        // Law: eff.MapError(err => err) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError identity law (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res

            let lhs = eff.MapError id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapError identity law should hold for success"

        // Law: eff.MapError(err => err) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError identity law (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err



            let lhs = eff.MapError id
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapError identity law should hold for error"

        // Law: eff.Map(f).Map(g) == eff.Map(f andThen g) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map composition law (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.succeed res

            let lhs = eff.Map(f).Map g
            let rhs = eff.Map(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Map composition law should hold for success"

        // Law: eff.Map(f).Map(g) == eff.Map(f andThen g) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "Map composition law (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.fail err

            let lhs = eff.Map(f).Map g
            let rhs = eff.Map(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "Map composition law should hold for error"

        // Law: eff.MapError(f).MapError(g) == eff.MapError(f andThen g) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError composition law (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.succeed res

            let lhs = eff.MapError(f).MapError g
            let rhs = eff.MapError(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapError composition law should hold for success"

        // Law: eff.MapError(f).MapError(g) == eff.MapError(f andThen g) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError composition law (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.fail err

            let lhs = eff.MapError(f).MapError g
            let rhs = eff.MapError(f >> g)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapError composition law should hold for error"

        // Law: eff.MapBoth(res => res, err => err) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth identity law (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res

            let lhs = eff.MapBoth(id, id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapBoth identity law should hold for success"

        // Law: eff.MapBoth(res => res, err => err) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth identity law (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err

            let lhs = eff.MapBoth(id, id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapBoth identity law should hold for error"

        // Law: eff.MapBoth(f1 >> f2, g1 >> g2) == eff.MapBoth(f1, g1).MapBoth(f2, g2) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth composition law (success)"
        <| fun (runtime: FIORuntime, res: int, f1: int -> int, f2: int -> int, g1: int -> int, g2: int -> int) ->
            let eff = FIO.succeed res

            let lhs = eff.MapBoth(f1 >> f2, g1 >> g2)
            let rhs = eff.MapBoth(f1, g1).MapBoth(f2, g2)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapBoth composition law should hold for success"

        // Law: eff.MapBoth(f1 >> f2, g1 >> g2) == eff.MapBoth(f1, g1).MapBoth(f2, g2) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth composition law (error)"
        <| fun (runtime: FIORuntime, err: int, f1: int -> int, f2: int -> int, g1: int -> int, g2: int -> int) ->
            let eff = FIO.fail err

            let lhs = eff.MapBoth(f1 >> f2, g1 >> g2)
            let rhs = eff.MapBoth(f1, g1).MapBoth(f2, g2)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapBoth composition law should hold for error"

        // TODO: Perhaps move to another file?
        // Law: eff.MapBoth(f, g) == eff.Map(f).MapError(g) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth consistency (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.succeed res

            let lhs = eff.MapBoth(f, g)
            let rhs = eff.Map(f).MapError g

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "MapBoth consistency should hold for success"

        // TODO: Perhaps move to another file?
        // Law: eff.MapBoth(f, g) == eff.Map(f).MapError(g) when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth consistency (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.fail err

            let lhs = eff.MapBoth(f, g)
            let rhs = eff.Map(f).MapError g

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "MapBoth consistency should hold for error"

        // TODO: Perhaps move to another file?
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth transforms success value when effect succeeds"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed(res).MapBoth((fun r -> r * 2), (fun e -> e + 100))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = res * 2

            Expect.equal actual expected "MapBoth should transform success value correctly"

        // TODO: Perhaps move to another file?
        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth transforms error value when effect fails"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail(err).MapBoth((fun r -> r * 2), (fun e -> e + 100))
            let actual = runtime.Run(eff).UnsafeError()
            let expected = err + 100

            Expect.equal actual expected "MapBoth should transform error value correctly"
    ]

[<Tests>]
let applicativeLaws =
    testList "Applicative Laws" [

        // Law: eff.Apply(Succeed id) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res

            let lhs = eff.Apply(FIO.succeed id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply identity law should hold for success"

        // Law: eff.Apply(Succeed id) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err

            let lhs = eff.Apply(FIO.succeed id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "Apply identity law should hold for error"

        // Law: eff.ApplyError(Fail id) == eff when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res

            let lhs = eff.ApplyError(FIO.fail id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "ApplyError identity law should hold for success"

        // Law: eff.ApplyError(Fail id) == eff when eff fails
        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err

            let lhs = eff.ApplyError(FIO.fail id)
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError identity law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply composition (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let compose (f: int -> int) (g: int -> int) (x: int) : int =
                f (g x)

            let eff = FIO.succeed res
            let ff = FIO.succeed f
            let gg = FIO.succeed g

            let lhs = eff.Apply(ff.Apply(gg.Apply(FIO.succeed compose)))
            let rhs = eff.Apply(ff).Apply gg

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply composition law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError composition (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let compose (f: int -> int) (g: int -> int) (x: int) : int =
                f (g x)
            
            let eff = FIO.fail err
            let ff = FIO.fail f
            let gg = FIO.fail g

            let lhs = eff.ApplyError(ff.ApplyError(gg.ApplyError(FIO.fail compose)))
            let rhs = eff.ApplyError(ff).ApplyError gg

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError composition law should hold for error"

        // Law: eff.Apply(Succeed f) == Succeed(f res) when eff succeeds
        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply homomorphism (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int) ->
            let eff = FIO.succeed res
            let effF = FIO.succeed f

            let lhs = eff.Apply effF
            let rhs = FIO.succeed(f res)

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply homomorphism law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError homomorphism (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int) ->
            let eff = FIO.fail err
            let effF = FIO.fail f

            let lhs = eff.ApplyError effF
            let rhs = FIO.fail(f err)

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError homomorphism law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Apply interchange (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int) ->
            let eff = FIO.succeed f

            let lhs = FIO.succeed(res).Apply eff
            let rhs = eff.Apply(FIO.succeed(fun g -> g res))

            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()

            Expect.equal lhs' rhs' "Apply interchange law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ApplyError interchange (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int) ->
            let eff = FIO.fail f

            let lhs = FIO.fail(err).ApplyError eff
            let rhs = eff.ApplyError(FIO.fail(fun g -> g err))

            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()

            Expect.equal lhs' rhs' "ApplyError interchange law should hold for error"
       ]

[<Tests>]
let monadLaws =
    testList "Monad Laws" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "FlatMap left identity for success"
        <| fun (runtime: FIORuntime, res: int) ->
            let f = FIO.succeed

            let lhs = FIO.succeed(res).FlatMap f
            let rhs = f res

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()

            Expect.equal lhs' rhs' "FlatMap left identity law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FlatMap right identity for success"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res

            let lhs = eff.FlatMap FIO.succeed
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()

            Expect.equal lhs' rhs' "FlatMap right identity law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FlatMap associativity for success"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let f = FIO.succeed
            let g = FIO.succeed

            let lhs = eff.FlatMap(f).FlatMap g
            let rhs = eff.FlatMap(fun x -> f(x).FlatMap g)

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()

            Expect.equal lhs' rhs' "FlatMap associativity law should hold for success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll left identity for error"
        <| fun (runtime: FIORuntime, err: int) ->
            let f = FIO.fail

            let lhs = FIO.fail(err).CatchAll f
            let rhs = f err

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()

            Expect.equal lhs' rhs' "CatchAll left identity law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll right identity for error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err

            let lhs = eff.CatchAll FIO.fail
            let rhs = eff

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()

            Expect.equal lhs' rhs' "CatchAll right identity law should hold for error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll associativity for error"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let f = FIO.fail
            let g = FIO.fail

            let lhs = eff.CatchAll(f).CatchAll g
            let rhs = eff.CatchAll(fun x -> f(x).CatchAll g)

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "CatchAll associativity law should hold for error"
    ]

[<Tests>]
let errorHandling =
    testList "Error Handling" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "MapError composes correctly"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff : FIO<int, int> = FIO.fail err
            let f = fun x -> x + 10
            let g = fun x -> x * 2

            let intermediate : FIO<int, int> = eff.MapError f
            let lhs : FIO<int, int> = intermediate.MapError g
            let rhs : FIO<int, int> = eff.MapError (fun x -> g (f x))

            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()

            Expect.equal lhs' rhs' "MapError should compose correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "CatchAll propagates errors correctly"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let recover x = FIO.fail (x + 100)

            let transformed = eff.CatchAll recover
            let expected = Failed(err + 100)

            let actual = runtime.Run(transformed).UnsafeResult()

            Expect.equal actual expected "BindError should propagate errors correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "ZipPar effects with mixed results handle errors"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let successEff = FIO.succeed res
            let failEff = FIO.fail err

            let parallelEff = successEff.ZipPar failEff

            let actualErr = runtime.Run(parallelEff).UnsafeError()

            Expect.equal actualErr err "ZipPar should propagate error"

        testPropertyWithConfig fsCheckPropertyTestsConfig "MapBoth composes MapError and Map in correct order"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let bimap = eff.MapBoth((fun r -> r + 1), string)
            let manual = eff.MapError(string).Map(fun r -> r + 1)

            let actualBimap = runtime.Run(bimap).UnsafeResult()
            let actualManual = runtime.Run(manual).UnsafeResult()

            Expect.equal actualBimap actualManual "MapBoth should compose correctly"
    ]

[<Tests>]
let sideEffects =
    testList "Side Effects (Tap/TapError)" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap executes side effect and preserves original success value"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable sideEffectValue = 0
            let eff = FIO.succeed(res).Tap(fun r ->
                FIO.succeed(sideEffectValue <- r * 2))

            let actual = runtime.Run(eff).UnsafeSuccess()

            Expect.equal actual res "Tap should preserve original value" .&.
            Expect.equal sideEffectValue (res * 2) "Tap should execute side effect"
    

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap propagates error from tap effect"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let eff = FIO.succeed(res).Tap(fun _ -> FIO.fail err)

            let actual = runtime.Run(eff).UnsafeError()

            Expect.equal actual err "Tap should propagate error from tap effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap does not execute when effect fails"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable executed = false
            let eff = FIO.fail(err).Tap(fun _ ->
                FIO.succeed(executed <- true))

            let actual = runtime.Run(eff).UnsafeError()

            Expect.equal actual err "Tap should preserve error" .&.
            Expect.isFalse executed "Tap should not execute on failure"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Tap discards result of tap effect"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed(res).Tap(fun _ -> FIO.succeed 999)

            let actual = runtime.Run(eff).UnsafeSuccess()

            Expect.equal actual res "Tap should discard tap effect result"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError executes side effect and preserves original error value"
        <| fun (runtime: FIORuntime, err: int) ->
            let mutable sideEffectValue = 0
            let eff = FIO.fail(err).TapError(fun e ->
                FIO.succeed (sideEffectValue <- e * 2))

            let actual = runtime.Run(eff).UnsafeError()

            Expect.equal actual err "TapError should preserve error" .&.
            Expect.equal sideEffectValue (err * 2) "TapError should execute side effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError propagates error from tap effect"
        <| fun (runtime: FIORuntime, err: int, newErr: int) ->
            let eff = FIO.fail(err).TapError(fun _ -> FIO.fail newErr)

            let actual = runtime.Run(eff).UnsafeError()

            Expect.equal actual newErr "TapError should propagate error from tap effect"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError does not execute when effect succeeds"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable executed = false
            let eff = FIO.succeed(res).TapError(fun _ ->
                FIO.succeed(executed <- true))

            let actual = runtime.Run(eff).UnsafeSuccess()

            Expect.equal actual res "TapError should preserve success" .&.
            Expect.isFalse executed "TapError should not execute on success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "TapError discards result of tap effect"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail(err).TapError(fun _ -> FIO.succeed 999)

            let actual = runtime.Run(eff).UnsafeError()

            Expect.equal actual err "TapError should discard tap effect result"
    ]

[<Tests>]
let foldOperations =
    testList "Fold Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fold handles success case with pure function"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed(res).Fold((fun e -> e), (fun r -> r * 2))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = res * 2

            Expect.equal actual expected "Fold should handle success correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fold handles error case with pure function"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail(err).Fold((fun e -> e + 100), (fun r -> r * 2))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = err + 100

            Expect.equal actual expected "Fold should handle error correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fold produces infallible result"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let successEff = FIO.succeed(res).Fold((fun _ -> -1), (fun r -> r + 1))
            let errorEff = FIO.fail(err).Fold((fun e -> e * 2), (fun _ -> 0))

            let actualSuccess = runtime.Run(successEff).UnsafeSuccess()
            let actualError = runtime.Run(errorEff).UnsafeSuccess()

            Expect.equal actualSuccess (res + 1) "Fold success case" .&.
            Expect.equal actualError (err * 2) "Fold error case"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FoldFIO handles success case with effectful function"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed(res).FoldFIO((fun e -> FIO.succeed e), (fun r -> FIO.succeed(r * 2)))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = res * 2

            Expect.equal actual expected "FoldFIO should handle success correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FoldFIO handles error case with effectful function"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail(err).FoldFIO((fun e -> FIO.succeed (e + 100)), (fun r -> FIO.succeed(r * 2)))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = err + 100

            Expect.equal actual expected "FoldFIO should handle error correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "FoldFIO error handler catches errors from success handler"
        <| fun (runtime: FIORuntime, res: int, err: int) ->
            let eff = FIO.succeed(res).FoldFIO((fun e -> FIO.succeed (e * 10)), (fun _ -> FIO.fail err))

            let actual = runtime.Run(eff).UnsafeSuccess()
            let expected = err * 10

            Expect.equal actual expected "FoldFIO error handler should catch errors"
    ]

[<Tests>]
let retryOperations =
    testList "Retry Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry does not retry when effect succeeds immediately"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable attempts = 0
            let eff = FIO.attemptExn(fun () -> attempts <- attempts + 1; res)
            let retried = eff.Retry 3

            let actual = runtime.Run(retried).UnsafeSuccess()

            Expect.equal actual res "RetryN should succeed" .&.
            Expect.equal attempts 1 "RetryN should not retry on immediate success"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry retries failed effect up to max times"
        <| fun (runtime: FIORuntime) ->
            let mutable attempts = 0
            let eff = fio {
                attempts <- attempts + 1
                return! FIO.fail "error"
            }
            let retried = eff.Retry 4

            let _ = runtime.Run(retried).UnsafeResult()

            Expect.equal attempts 4 "RetryN should retry up to max attempts"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry succeeds on intermediate retry"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable attempts = 0
            let eff = fio {
                attempts <- attempts + 1
                if attempts < 3 then
                    return! FIO.fail "error"
                else
                    return res
            }
            let retried = eff.Retry 5

            let actual = runtime.Run(retried).UnsafeSuccess()

            Expect.equal actual res "RetryN should succeed" .&.
            Expect.equal attempts 3 "RetryN should succeed on third attempt"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry final failure after max retries"
        <| fun (runtime: FIORuntime) ->
            let eff = FIO.fail "error"
            let retried = eff.Retry 2

            let actual = runtime.Run(retried).UnsafeError()

            Expect.equal actual "error" "RetryN should fail after max retries"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Retry preserves success value when retrying"
        <| fun (runtime: FIORuntime, res: int) ->
            let mutable attempts = 0
            let eff = FIO.attemptExn(fun () ->
                attempts <- attempts + 1
                if attempts = 1 then failwith "Fail once" else res)

            let retried = eff.Retry 5

            let actual = (runtime.Run retried).UnsafeSuccess()

            Expect.equal actual res "RetryN should preserve success value"
    ]

[<Tests>]
let channelOperations =
    testList "Channel Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel Send and Receive maintains message order"
        <| fun (runtime: FIORuntime, messages: int list) ->
            if List.isEmpty messages then
                ()
            else
                let eff = fio {
                    let chan = Channel<int>()
                    for msg in messages do
                        do! chan.Send(msg).FlatMap (fun _ -> FIO.succeed ())
                    let mutable received = []
                    for _ in messages do
                        let! msg = chan.Receive ()
                        received <- msg :: received
                    return List.rev received
                }
                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.equal result messages "Channel should maintain message order"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel operations compose with Bind correctly"
        <| fun (runtime: FIORuntime, msg: int) ->
            let eff = fio {
                let chan = Channel<int>()
                let! sentMsg = chan.Send msg
                let! receivedMsg = chan.Receive ()
                return sentMsg = receivedMsg && receivedMsg = msg
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Channel send/receive should compose correctly"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel Count is accurate after operations"
        <| fun (runtime: FIORuntime, messages: int list) ->
            if List.isEmpty messages then
                ()
            else
                let eff = fio {
                    let chan = Channel<int>()
                    for msg in messages do
                        do! chan.Send(msg).FlatMap(fun _ -> FIO.succeed())
                    let countAfterSend = chan.Count
                    let mutable received = []
                    for _ in messages do
                        let! msg = chan.Receive()
                        received <- msg :: received
                    let countAfterReceive = chan.Count
                    return countAfterSend = List.length messages && countAfterReceive = 0
                }
                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.isTrue result "Channel count should be accurate"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Channel with no receivers maintains all messages"
        <| fun (runtime: FIORuntime, messages: int list) ->
            if List.isEmpty messages then
                ()
            else
                let eff = fio {
                    let chan = Channel<int>()
                    for msg in messages do
                        do! chan.Send(msg).FlatMap (fun _ -> FIO.succeed())
                    return chan.Count = List.length messages
                }
                let result = runtime.Run(eff).UnsafeSuccess()
                Expect.isTrue result "Channel should maintain all messages"
    ]

[<Tests>]
let fiberOperations =
    testList "Fiber Operations" [

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fork then Join equals identity"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! fiber = FIO.succeed(res).Fork()
                let! result = fiber.Join()
                return result
            }
            let forkAwaitResult = runtime.Run(eff).UnsafeSuccess()
            Expect.equal forkAwaitResult res "Fork then Join should equal identity"

        testPropertyWithConfig fsCheckPropertyTestsConfig "Fiber ID is unique per fork"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let! fiber1 = (FIO.succeed res).Fork()
                let! fiber2 = (FIO.succeed res).Fork()
                let! fiber3 = (FIO.succeed res).Fork()
                let ids = [fiber1.Id; fiber2.Id; fiber3.Id]
                return List.distinct ids |> List.length = 3
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Fiber IDs should be unique"
    ]
