/// <summary>Provides property-based tests for FIO operators including bind, map, parallel, and fallback operators.</summary>
module FIO.Tests.OperatorTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System

[<Tests>]
let operatorTests =
    testList
        "Operators"
        [
            // ─── Sequential composition (*>, <*, <*>) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "( *> ) ZipRight success"
            <| fun (runtime: FIORuntime, a: int, b: string) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 *> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.ZipRight eff2).UnsafeSuccess()

                Expect.equal operatorResult b "( *> ) should return second result"
                Expect.equal operatorResult methodResult "( *> ) should equal ZipRight"

            testPropertyWithConfig fsCheckConfig "( *> ) ZipRight error propagation"
            <| fun (runtime: FIORuntime, error: string, b: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 *> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.ZipRight eff2).UnsafeError()

                Expect.equal operatorResult error "( *> ) should propagate first error"
                Expect.equal operatorResult methodResult "( *> ) should equal ZipRight on error"

            testPropertyWithConfig fsCheckConfig "( <* ) ZipLeft success"
            <| fun (runtime: FIORuntime, a: int, b: string) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <* eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.ZipLeft eff2).UnsafeSuccess()

                Expect.equal operatorResult a "( <* ) should return first result"
                Expect.equal operatorResult methodResult "( <* ) should equal ZipLeft"

            testPropertyWithConfig fsCheckConfig "( <* ) ZipLeft error propagation"
            <| fun (runtime: FIORuntime, a: int, error: string) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.fail error

                let operatorResult = runtime.Run(eff1 <* eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.ZipLeft eff2).UnsafeError()

                Expect.equal operatorResult error "( <* ) should propagate second error"
                Expect.equal operatorResult methodResult "( <* ) should equal ZipLeft on error"

            testPropertyWithConfig fsCheckConfig "( <*> ) Zip success"
            <| fun (runtime: FIORuntime, a: int, b: string) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <*> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.Zip eff2).UnsafeSuccess()

                Expect.equal operatorResult (a, b) "( <*> ) should return tuple"
                Expect.equal operatorResult methodResult "( <*> ) should equal Zip"

            testPropertyWithConfig fsCheckConfig "( <*> ) Zip error propagation"
            <| fun (runtime: FIORuntime, error: string, b: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <*> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.Zip eff2).UnsafeError()

                Expect.equal operatorResult error "( <*> ) should propagate error"
                Expect.equal operatorResult methodResult "( <*> ) should equal Zip on error"

            // ─── Parallel composition (<&>, <&&>, &>, <&) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "( <&> ) ZipPar success"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <&> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()

                Expect.equal operatorResult (a, b) "( <&> ) should return tuple"
                Expect.equal operatorResult methodResult "( <&> ) should equal ZipPar"

            testPropertyWithConfig fsCheckConfig "( <&> ) ZipPar error propagation"
            <| fun (runtime: FIORuntime, error: string, b: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <&> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.ZipPar eff2).UnsafeError()

                Expect.equal operatorResult error "( <&> ) should propagate error"
                Expect.equal operatorResult methodResult "( <&> ) should equal ZipPar on error"

            testPropertyWithConfig fsCheckConfig "( <&&> ) parallel fire-and-forget success"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <&&> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.ZipPar(eff2).Unit()).UnsafeSuccess()

                Expect.equal operatorResult () "( <&&> ) should return unit"
                Expect.equal operatorResult methodResult "( <&&> ) should equal ZipPar().Unit()"

            testPropertyWithConfig fsCheckConfig "( <&&> ) parallel fire-and-forget error propagation"
            <| fun (runtime: FIORuntime, error: string, b: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <&&> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.ZipPar(eff2).Unit()).UnsafeError()

                Expect.equal operatorResult error "( <&&> ) should propagate error"
                Expect.equal operatorResult methodResult "( <&&> ) should equal ZipPar().Unit() on error"

            testPropertyWithConfig fsCheckConfig "( &> ) ZipParRight success"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 &> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.ZipParRight eff2).UnsafeSuccess()

                Expect.equal operatorResult b "( &> ) should return second result"
                Expect.equal operatorResult methodResult "( &> ) should equal ZipParRight"

            testPropertyWithConfig fsCheckConfig "( &> ) ZipParRight error propagation"
            <| fun (runtime: FIORuntime, error: string, b: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 &> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.ZipParRight eff2).UnsafeError()

                Expect.equal operatorResult error "( &> ) should propagate error"
                Expect.equal operatorResult methodResult "( &> ) should equal ZipParRight on error"

            testPropertyWithConfig fsCheckConfig "( <& ) ZipParLeft success"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <& eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.ZipParLeft eff2).UnsafeSuccess()

                Expect.equal operatorResult a "( <& ) should return first result"
                Expect.equal operatorResult methodResult "( <& ) should equal ZipParLeft"

            testPropertyWithConfig fsCheckConfig "( <& ) ZipParLeft error propagation"
            <| fun (runtime: FIORuntime, a: int, error: string) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.fail error

                let operatorResult = runtime.Run(eff1 <& eff2).UnsafeResult()
                let methodResult = runtime.Run(eff1.ZipParLeft eff2).UnsafeResult()

                match operatorResult with
                | Failed e -> Expect.equal e error "( <& ) should propagate error"
                | _ -> failtest "Expected error"

                match methodResult with
                | Failed e -> Expect.equal e error "ZipParLeft should propagate error"
                | _ -> failtest "Expected error"

            // ─── Fallback (<|>) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "( <|> ) OrElse first succeeds"
            <| fun (runtime: FIORuntime, a: int, b: int) ->
                let eff1 = FIO.succeed a
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeSuccess()

                Expect.equal operatorResult a "( <|> ) should return first on success"
                Expect.equal operatorResult methodResult "( <|> ) should equal OrElse"

            testPropertyWithConfig fsCheckConfig "( <|> ) OrElse fallback on error"
            <| fun (runtime: FIORuntime, error: string, b: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed b

                let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeSuccess()

                Expect.equal operatorResult b "( <|> ) should return fallback on first error"
                Expect.equal operatorResult methodResult "( <|> ) should equal OrElse on fallback"

            testPropertyWithConfig fsCheckConfig "( <|> ) OrElse both fail"
            <| fun (runtime: FIORuntime, err1: string, err2: int) ->
                let eff1 = FIO.fail err1
                let eff2 = FIO.fail err2

                let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeError()

                Expect.equal operatorResult err2 "( <|> ) should return second error when both fail"
                Expect.equal operatorResult methodResult "( <|> ) should equal OrElse when both fail"

            // ─── Either fallback (<+>) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "( <+> ) OrElseEither this succeeds returns Choice1Of2"
            <| fun (runtime: FIORuntime, value: int, fallback: string) ->
                let eff1 = FIO.succeed value
                let eff2 = FIO.succeed fallback

                let operatorResult = runtime.Run(eff1 <+> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.OrElseEither eff2).UnsafeSuccess()

                Expect.equal operatorResult (Choice1Of2 value) "( <+> ) should return Choice1Of2 on success"
                Expect.equal operatorResult methodResult "( <+> ) should equal OrElseEither"

            testPropertyWithConfig fsCheckConfig "( <+> ) OrElseEither this fails, fallback succeeds returns Choice2Of2"
            <| fun (runtime: FIORuntime, error: string, fallback: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.succeed fallback

                let operatorResult = runtime.Run(eff1 <+> eff2).UnsafeSuccess()
                let methodResult = runtime.Run(eff1.OrElseEither eff2).UnsafeSuccess()

                Expect.equal operatorResult (Choice2Of2 fallback) "( <+> ) should return Choice2Of2 when fallback succeeds"
                Expect.equal operatorResult methodResult "( <+> ) should equal OrElseEither on fallback"

            testPropertyWithConfig fsCheckConfig "( <+> ) OrElseEither both fail returns fallback error"
            <| fun (runtime: FIORuntime, error: string, fallbackErr: int) ->
                let eff1 = FIO.fail error
                let eff2 = FIO.fail fallbackErr

                let operatorResult = runtime.Run(eff1 <+> eff2).UnsafeError()
                let methodResult = runtime.Run(eff1.OrElseEither eff2).UnsafeError()

                Expect.equal operatorResult fallbackErr "( <+> ) should propagate the fallback error when both fail"
                Expect.equal operatorResult methodResult "( <+> ) should equal OrElseEither when both fail"

            // ─── Either race (<?>) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "( <?> ) RaceEither first racer wins returns Choice1Of2"
            <| fun (runtime: FIORuntime) ->
                let fast = FIO.succeed 1
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed "slow")

                let operatorResult = runtime.Run(fast <?> slow).UnsafeSuccess()
                let methodResult = runtime.Run(fast.RaceEither slow).UnsafeSuccess()

                Expect.equal operatorResult (Choice1Of2 1) "( <?> ) should return Choice1Of2 when the left racer wins"
                Expect.equal operatorResult methodResult "( <?> ) should equal RaceEither"

            testPropertyWithConfig fsCheckConfig "( <?> ) RaceEither second racer wins returns Choice2Of2"
            <| fun (runtime: FIORuntime) ->
                let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0) id).FlatMap(fun () -> FIO.succeed 1)
                let fast = FIO.succeed "fast"

                let operatorResult = runtime.Run(slow <?> fast).UnsafeSuccess()
                let methodResult = runtime.Run(slow.RaceEither fast).UnsafeSuccess()

                Expect.equal operatorResult (Choice2Of2 "fast") "( <?> ) should return Choice2Of2 when the right racer wins"
                Expect.equal operatorResult methodResult "( <?> ) should equal RaceEither"

            // ─── Bind / Map (>>=, <!>) ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "( >>= ) FlatMap success"
            <| fun (runtime: FIORuntime, a: int, f: int -> int) ->
                let effect = FIO.succeed a
                let cont = fun x -> FIO.succeed (f x)

                let operatorResult = runtime.Run(effect >>= cont).UnsafeSuccess()
                let methodResult = runtime.Run(effect.FlatMap cont).UnsafeSuccess()

                Expect.equal operatorResult (f a) "( >>= ) should apply continuation"
                Expect.equal operatorResult methodResult "( >>= ) should equal FlatMap"

            testPropertyWithConfig fsCheckConfig "( >>= ) FlatMap error propagation"
            <| fun (runtime: FIORuntime, error: string) ->
                let effect = FIO.fail error
                let cont = fun x -> FIO.succeed (x + 1)

                let operatorResult = runtime.Run(effect >>= cont).UnsafeError()
                let methodResult = runtime.Run(effect.FlatMap cont).UnsafeError()

                Expect.equal operatorResult error "( >>= ) should propagate error"
                Expect.equal operatorResult methodResult "( >>= ) should equal FlatMap on error"

            testPropertyWithConfig fsCheckConfig "( <!> ) Map success"
            <| fun (runtime: FIORuntime, a: int, f: int -> int) ->
                let effect = FIO.succeed a

                let operatorResult = runtime.Run(f <!> effect).UnsafeSuccess()
                let methodResult = runtime.Run(effect.Map f).UnsafeSuccess()

                Expect.equal operatorResult (f a) "( <!> ) should apply mapper"
                Expect.equal operatorResult methodResult "( <!> ) should equal Map"

            testPropertyWithConfig fsCheckConfig "( <!> ) Map error propagation"
            <| fun (runtime: FIORuntime, error: string, f: int -> int) ->
                let effect = FIO.fail error

                let operatorResult = runtime.Run(f <!> effect).UnsafeError()
                let methodResult = runtime.Run(effect.Map f).UnsafeError()

                Expect.equal operatorResult error "( <!> ) should propagate error"
                Expect.equal operatorResult methodResult "( <!> ) should equal Map on error"
        ]
