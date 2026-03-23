module FIO.Tests.OperatorTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

[<Tests>]
let operatorTests =
    testList "Operators" [

        // ( *> ) - ZipRight: sequences two effects, returns second result
        testPropertyWithConfig fsCheckConfig "( *> ) ZipRight success"
        <| fun (runtime: FIORuntime, a: int, b: string) ->
            let eff1 = FIO.succeed a
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 *> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.ZipRight eff2).UnsafeSuccess()

            Expect.equal operatorResult b "( *> ) should return second result"
            Expect.equal operatorResult methodResult "( *> ) should equal ZipRight"

        testPropertyWithConfig fsCheckConfig "( *> ) ZipRight error propagation"
        <| fun (runtime: FIORuntime, err: string, b: int) ->
            let eff1: FIO<int, string> = FIO.fail err
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 *> eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.ZipRight eff2).UnsafeError()

            Expect.equal operatorResult err "( *> ) should propagate first error"
            Expect.equal operatorResult methodResult "( *> ) should equal ZipRight on error"

        // ( <* ) - ZipLeft: sequences two effects, returns first result
        testPropertyWithConfig fsCheckConfig "( <* ) ZipLeft success"
        <| fun (runtime: FIORuntime, a: int, b: string) ->
            let eff1 = FIO.succeed a
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <* eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.ZipLeft eff2).UnsafeSuccess()

            Expect.equal operatorResult a "( <* ) should return first result"
            Expect.equal operatorResult methodResult "( <* ) should equal ZipLeft"

        testPropertyWithConfig fsCheckConfig "( <* ) ZipLeft error propagation"
        <| fun (runtime: FIORuntime, a: int, err: string) ->
            let eff1 = FIO.succeed a
            let eff2: FIO<int, string> = FIO.fail err

            let operatorResult = runtime.Run(eff1 <* eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.ZipLeft eff2).UnsafeError()

            Expect.equal operatorResult err "( <* ) should propagate second error"
            Expect.equal operatorResult methodResult "( <* ) should equal ZipLeft on error"

        // ( <*> ) - Zip: combines results into tuple
        testPropertyWithConfig fsCheckConfig "( <*> ) Zip success"
        <| fun (runtime: FIORuntime, a: int, b: string) ->
            let eff1 = FIO.succeed a
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <*> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.Zip eff2).UnsafeSuccess()

            Expect.equal operatorResult (a, b) "( <*> ) should return tuple"
            Expect.equal operatorResult methodResult "( <*> ) should equal Zip"

        testPropertyWithConfig fsCheckConfig "( <*> ) Zip error propagation"
        <| fun (runtime: FIORuntime, err: string, b: int) ->
            let eff1: FIO<int, string> = FIO.fail err
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <*> eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.Zip eff2).UnsafeError()

            Expect.equal operatorResult err "( <*> ) should propagate error"
            Expect.equal operatorResult methodResult "( <*> ) should equal Zip on error"

        // ( <&> ) - ZipPar: parallel execution, returns tuple
        testPropertyWithConfig fsCheckConfig "( <&> ) ZipPar success"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff1 : FIO<int, string> = FIO.succeed a
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <&> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()

            Expect.equal operatorResult (a, b) "( <&> ) should return tuple"
            Expect.equal operatorResult methodResult "( <&> ) should equal ZipPar"

        testPropertyWithConfig fsCheckConfig "( <&> ) ZipPar error propagation"
        <| fun (runtime: FIORuntime, err: string, b: int) ->
            let eff1: FIO<int, string> = FIO.fail err
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <&> eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.ZipPar eff2).UnsafeError()

            Expect.equal operatorResult err "( <&> ) should propagate error"
            Expect.equal operatorResult methodResult "( <&> ) should equal ZipPar on error"

        // ( <&&> ) - Parallel fire-and-forget: both run, returns unit
        testPropertyWithConfig fsCheckConfig "( <&&> ) parallel fire-and-forget success"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff1 : FIO<int, string> = FIO.succeed a
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <&&> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.ZipPar(eff2).Unit()).UnsafeSuccess()

            Expect.equal operatorResult () "( <&&> ) should return unit"
            Expect.equal operatorResult methodResult "( <&&> ) should equal ZipPar().Unit()"

        testPropertyWithConfig fsCheckConfig "( <&&> ) parallel fire-and-forget error propagation"
        <| fun (runtime: FIORuntime, err: string, b: int) ->
            let eff1: FIO<int, string> = FIO.fail err
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <&&> eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.ZipPar(eff2).Unit()).UnsafeError()

            Expect.equal operatorResult err "( <&&> ) should propagate error"
            Expect.equal operatorResult methodResult "( <&&> ) should equal ZipPar().Unit() on error"

        // ( &> ) - ZipParRight: parallel, returns second
        testPropertyWithConfig fsCheckConfig "( &> ) ZipParRight success"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff1 : FIO<int, string> = FIO.succeed a
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 &> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.ZipParRight eff2).UnsafeSuccess()

            Expect.equal operatorResult b "( &> ) should return second result"
            Expect.equal operatorResult methodResult "( &> ) should equal ZipParRight"

        testPropertyWithConfig fsCheckConfig "( &> ) ZipParRight error propagation"
        <| fun (runtime: FIORuntime, err: string, b: int) ->
            let eff1: FIO<int, string> = FIO.fail err
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 &> eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.ZipParRight eff2).UnsafeError()

            Expect.equal operatorResult err "( &> ) should propagate error"
            Expect.equal operatorResult methodResult "( &> ) should equal ZipParRight on error"

        // ( <& ) - ZipParLeft: parallel, returns first
        testPropertyWithConfig fsCheckConfig "( <& ) ZipParLeft success"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff1 : FIO<int, string> = FIO.succeed a
            let eff2 : FIO<int, string> = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <& eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.ZipParLeft eff2).UnsafeSuccess()

            Expect.equal operatorResult a "( <& ) should return first result"
            Expect.equal operatorResult methodResult "( <& ) should equal ZipParLeft"

        testPropertyWithConfig fsCheckConfig "( <& ) ZipParLeft error propagation"
        <| fun (runtime: FIORuntime, a: int, err: string) ->
            let eff1 : FIO<int, string> = FIO.succeed a
            let eff2: FIO<int, string> = FIO.fail err

            let operatorResult = runtime.Run(eff1 <& eff2).UnsafeResult()
            let methodResult = runtime.Run(eff1.ZipParLeft eff2).UnsafeResult()

            match operatorResult with
            | Failed e -> Expect.equal e err "( <& ) should propagate error"
            | _ -> failtest "Expected error"
            match methodResult with
            | Failed e -> Expect.equal e err "ZipParLeft should propagate error"
            | _ -> failtest "Expected error"

        // ( <|> ) - OrElse: tries first, falls back to second on error
        testPropertyWithConfig fsCheckConfig "( <|> ) OrElse first succeeds"
        <| fun (runtime: FIORuntime, a: int, b: int) ->
            let eff1 = FIO.succeed a
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeSuccess()

            Expect.equal operatorResult a "( <|> ) should return first on success"
            Expect.equal operatorResult methodResult "( <|> ) should equal OrElse"

        testPropertyWithConfig fsCheckConfig "( <|> ) OrElse fallback on error"
        <| fun (runtime: FIORuntime, err: string, b: int) ->
            let eff1: FIO<int, string> = FIO.fail err
            let eff2 = FIO.succeed b

            let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeSuccess()
            let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeSuccess()

            Expect.equal operatorResult b "( <|> ) should return fallback on first error"
            Expect.equal operatorResult methodResult "( <|> ) should equal OrElse on fallback"

        testPropertyWithConfig fsCheckConfig "( <|> ) OrElse both fail"
        <| fun (runtime: FIORuntime, err1: string, err2: int) ->
            let eff1: FIO<int, string> = FIO.fail err1
            let eff2: FIO<int, int> = FIO.fail err2

            let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeError()
            let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeError()

            Expect.equal operatorResult err2 "( <|> ) should return second error when both fail"
            Expect.equal operatorResult methodResult "( <|> ) should equal OrElse when both fail"

        // ( >>= ) - FlatMap/bind: chains effects
        testPropertyWithConfig fsCheckConfig "( >>= ) FlatMap success"
        <| fun (runtime: FIORuntime, a: int, f: int -> int) ->
            let eff = FIO.succeed a
            let cont = fun x -> FIO.succeed (f x)

            let operatorResult = runtime.Run(eff >>= cont).UnsafeSuccess()
            let methodResult = runtime.Run(eff.FlatMap cont).UnsafeSuccess()

            Expect.equal operatorResult (f a) "( >>= ) should apply continuation"
            Expect.equal operatorResult methodResult "( >>= ) should equal FlatMap"

        testPropertyWithConfig fsCheckConfig "( >>= ) FlatMap error propagation"
        <| fun (runtime: FIORuntime, err: string) ->
            let eff: FIO<int, string> = FIO.fail err
            let cont = fun x -> FIO.succeed (x + 1)

            let operatorResult = runtime.Run(eff >>= cont).UnsafeError()
            let methodResult = runtime.Run(eff.FlatMap cont).UnsafeError()

            Expect.equal operatorResult err "( >>= ) should propagate error"
            Expect.equal operatorResult methodResult "( >>= ) should equal FlatMap on error"

        // ( <!> ) - Map: transforms result
        testPropertyWithConfig fsCheckConfig "( <!> ) Map success"
        <| fun (runtime: FIORuntime, a: int, f: int -> int) ->
            let eff = FIO.succeed a

            let operatorResult = runtime.Run(f <!> eff).UnsafeSuccess()
            let methodResult = runtime.Run(eff.Map f).UnsafeSuccess()

            Expect.equal operatorResult (f a) "( <!> ) should apply mapper"
            Expect.equal operatorResult methodResult "( <!> ) should equal Map"

        testPropertyWithConfig fsCheckConfig "( <!> ) Map error propagation"
        <| fun (runtime: FIORuntime, err: string, f: int -> int) ->
            let eff: FIO<int, string> = FIO.fail err

            let operatorResult = runtime.Run(f <!> eff).UnsafeError()
            let methodResult = runtime.Run(eff.Map f).UnsafeError()

            Expect.equal operatorResult err "( <!> ) should propagate error"
            Expect.equal operatorResult methodResult "( <!> ) should equal Map on error"
    ]
