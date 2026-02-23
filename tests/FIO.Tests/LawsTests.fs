module FIO.Tests.LawsTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

[<Tests>]
let functorLaws =
    testList "Functor Laws" [

        // Map identity: eff.Map(id) = eff
        testPropertyWithConfig fsCheckConfig "Map identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let lhs = eff.Map id
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "Map id should equal original"

        testPropertyWithConfig fsCheckConfig "Map identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let lhs = eff.Map id
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "Map id preserves error"

        // Map composition: eff.Map(f).Map(g) = eff.Map(g << f)
        testPropertyWithConfig fsCheckConfig "Map composition (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.succeed res
            let lhs = eff.Map(f).Map g
            let rhs = eff.Map(f >> g)
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "Map composition should hold"

        testPropertyWithConfig fsCheckConfig "Map composition (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.fail err
            let lhs = eff.Map(f).Map g
            let rhs = eff.Map(f >> g)
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "Map composition preserves error"

        // MapError identity
        testPropertyWithConfig fsCheckConfig "MapError identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let lhs = eff.MapError id
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "MapError id preserves success"

        testPropertyWithConfig fsCheckConfig "MapError identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let lhs = eff.MapError id
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "MapError id should equal original error"

        // MapError composition
        testPropertyWithConfig fsCheckConfig "MapError composition (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.succeed res
            let lhs = eff.MapError(f).MapError g
            let rhs = eff.MapError(f >> g)
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "MapError composition preserves success"

        testPropertyWithConfig fsCheckConfig "MapError composition (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.fail err
            let lhs = eff.MapError(f).MapError g
            let rhs = eff.MapError(f >> g)
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "MapError composition should hold"

        // MapBoth identity
        testPropertyWithConfig fsCheckConfig "MapBoth identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let lhs = eff.MapBoth(id, id)
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "MapBoth id should equal original"

        testPropertyWithConfig fsCheckConfig "MapBoth identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let lhs = eff.MapBoth(id, id)
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "MapBoth id preserves error"

        // MapBoth composition
        testPropertyWithConfig fsCheckConfig "MapBoth composition (success)"
        <| fun (runtime: FIORuntime, res: int, f1: int -> int, f2: int -> int, g1: int -> int, g2: int -> int) ->
            let eff = FIO.succeed res
            let lhs = eff.MapBoth(f1 >> f2, g1 >> g2)
            let rhs = eff.MapBoth(f1, g1).MapBoth(f2, g2)
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "MapBoth composition should hold"

        testPropertyWithConfig fsCheckConfig "MapBoth composition (error)"
        <| fun (runtime: FIORuntime, err: int, f1: int -> int, f2: int -> int, g1: int -> int, g2: int -> int) ->
            let eff = FIO.fail err
            let lhs = eff.MapBoth(f1 >> f2, g1 >> g2)
            let rhs = eff.MapBoth(f1, g1).MapBoth(f2, g2)
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "MapBoth composition preserves error"

        // MapBoth consistency with Map and MapError
        testPropertyWithConfig fsCheckConfig "MapBoth equals Map then MapError (success)"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let eff = FIO.succeed res
            let lhs = eff.MapBoth(f, g)
            let rhs = eff.Map(f).MapError g
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "MapBoth should equal Map then MapError"

        testPropertyWithConfig fsCheckConfig "MapBoth equals Map then MapError (error)"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let eff = FIO.fail err
            let lhs = eff.MapBoth(f, g)
            let rhs = eff.Map(f).MapError g
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "MapBoth should equal Map then MapError for errors"
    ]

[<Tests>]
let applicativeLaws =
    testList "Applicative Laws" [

        // Identity: eff.Apply(succeed id) = eff
        testPropertyWithConfig fsCheckConfig "Apply identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let lhs = eff.Apply(FIO.succeed id)
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "Apply id should equal original"

        testPropertyWithConfig fsCheckConfig "Apply identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let lhs = eff.Apply(FIO.succeed id)
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "Apply id preserves error"

        // Homomorphism: succeed(f).Apply(succeed(x)) = succeed(f(x))
        testPropertyWithConfig fsCheckConfig "Apply homomorphism"
        <| fun (runtime: FIORuntime, res: int, f: int -> int) ->
            let lhs = (FIO.succeed res).Apply(FIO.succeed f)
            let rhs = FIO.succeed(f res)
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "Apply homomorphism should hold"

        // Interchange: succeed(y).Apply(u) = u.Apply(succeed(fun f -> f y))
        testPropertyWithConfig fsCheckConfig "Apply interchange"
        <| fun (runtime: FIORuntime, res: int, f: int -> int) ->
            let u = FIO.succeed f
            let lhs = (FIO.succeed res).Apply u
            let rhs = u.Apply(FIO.succeed(fun g -> g res))
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "Apply interchange should hold"

        // Composition
        testPropertyWithConfig fsCheckConfig "Apply composition"
        <| fun (runtime: FIORuntime, res: int, f: int -> int, g: int -> int) ->
            let compose (f: int -> int) (g: int -> int) (x: int) : int = f (g x)
            let eff = FIO.succeed res
            let ff = FIO.succeed f
            let gg = FIO.succeed g
            let lhs = eff.Apply(ff.Apply(gg.Apply(FIO.succeed compose)))
            let rhs = eff.Apply(ff).Apply gg
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "Apply composition should hold"

        // ApplyError identity
        testPropertyWithConfig fsCheckConfig "ApplyError identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let lhs = eff.ApplyError(FIO.fail id)
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeSuccess()
            let rhs' = runtime.Run(rhs).UnsafeSuccess()
            Expect.equal lhs' rhs' "ApplyError id preserves success"

        testPropertyWithConfig fsCheckConfig "ApplyError identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let lhs = eff.ApplyError(FIO.fail id)
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "ApplyError id should equal original"

        // ApplyError homomorphism
        testPropertyWithConfig fsCheckConfig "ApplyError homomorphism"
        <| fun (runtime: FIORuntime, err: int, f: int -> int) ->
            let lhs = (FIO.fail err).ApplyError(FIO.fail f)
            let rhs = FIO.fail(f err)
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "ApplyError homomorphism should hold"

        // ApplyError interchange
        testPropertyWithConfig fsCheckConfig "ApplyError interchange"
        <| fun (runtime: FIORuntime, err: int, f: int -> int) ->
            let u = FIO.fail f
            let lhs = (FIO.fail err).ApplyError u
            let rhs = u.ApplyError(FIO.fail(fun g -> g err))
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "ApplyError interchange should hold"

        // ApplyError composition
        testPropertyWithConfig fsCheckConfig "ApplyError composition"
        <| fun (runtime: FIORuntime, err: int, f: int -> int, g: int -> int) ->
            let compose (f: int -> int) (g: int -> int) (x: int) : int = f (g x)
            let eff = FIO.fail err
            let ff = FIO.fail f
            let gg = FIO.fail g
            let lhs = eff.ApplyError(ff.ApplyError(gg.ApplyError(FIO.fail compose)))
            let rhs = eff.ApplyError(ff).ApplyError gg
            let lhs' = runtime.Run(lhs).UnsafeError()
            let rhs' = runtime.Run(rhs).UnsafeError()
            Expect.equal lhs' rhs' "ApplyError composition should hold"
    ]

[<Tests>]
let monadLaws =
    testList "Monad Laws" [

        // Left identity: succeed(a).FlatMap(f) = f(a)
        testPropertyWithConfig fsCheckConfig "FlatMap left identity"
        <| fun (runtime: FIORuntime, res: int) ->
            let f x = FIO.succeed(x * 2)
            let lhs = (FIO.succeed res).FlatMap f
            let rhs = f res
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "FlatMap left identity should hold"

        // Right identity: m.FlatMap(succeed) = m
        testPropertyWithConfig fsCheckConfig "FlatMap right identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = FIO.succeed res
            let lhs = eff.FlatMap FIO.succeed
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "FlatMap right identity should hold"

        testPropertyWithConfig fsCheckConfig "FlatMap right identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff : FIO<int, int> = FIO.fail err
            let lhs = eff.FlatMap FIO.succeed
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "FlatMap right identity preserves error"

        // Associativity: m.FlatMap(f).FlatMap(g) = m.FlatMap(x -> f(x).FlatMap(g))
        testPropertyWithConfig fsCheckConfig "FlatMap associativity"
        <| fun (runtime: FIORuntime, res: int) ->
            let f x = FIO.succeed(x + 1)
            let g x = FIO.succeed(x * 2)
            let eff = FIO.succeed res
            let lhs = eff.FlatMap(f).FlatMap g
            let rhs = eff.FlatMap(fun x -> f(x).FlatMap g)
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "FlatMap associativity should hold"

        // CatchAll left identity
        testPropertyWithConfig fsCheckConfig "CatchAll left identity"
        <| fun (runtime: FIORuntime, err: int) ->
            let f e = FIO.fail(e * 2)
            let lhs = (FIO.fail err).CatchAll f
            let rhs = f err
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "CatchAll left identity should hold"

        // CatchAll right identity
        testPropertyWithConfig fsCheckConfig "CatchAll right identity (error)"
        <| fun (runtime: FIORuntime, err: int) ->
            let eff = FIO.fail err
            let lhs = eff.CatchAll FIO.fail
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "CatchAll right identity should hold"

        testPropertyWithConfig fsCheckConfig "CatchAll right identity (success)"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff : FIO<int, int> = FIO.succeed res
            let lhs = eff.CatchAll FIO.fail
            let rhs = eff
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "CatchAll right identity preserves success"

        // CatchAll associativity
        testPropertyWithConfig fsCheckConfig "CatchAll associativity"
        <| fun (runtime: FIORuntime, err: int) ->
            let f e = FIO.fail(e + 1)
            let g e = FIO.fail(e * 2)
            let eff = FIO.fail err
            let lhs = eff.CatchAll(f).CatchAll g
            let rhs = eff.CatchAll(fun e -> f(e).CatchAll g)
            let lhs' = runtime.Run(lhs).UnsafeResult()
            let rhs' = runtime.Run(rhs).UnsafeResult()
            Expect.equal lhs' rhs' "CatchAll associativity should hold"
    ]
