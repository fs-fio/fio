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
            testList
                "Sequential composition (*>, <*, <*>)"
                [
                    testPropertyWithConfig fsCheckConfig "( *> ) - keeps the right result"
                    <| fun (runtime: FIORuntime, a: int, b: string) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 *> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.ZipRight eff2).UnsafeSuccess()

                        Expect.equal operatorResult b "( *> ) should return second result"
                        Expect.equal operatorResult methodResult "( *> ) should equal ZipRight"

                    testPropertyWithConfig fsCheckConfig "( *> ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string, b: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 *> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.ZipRight eff2).UnsafeError()

                        Expect.equal operatorResult error "( *> ) should propagate first error"
                        Expect.equal operatorResult methodResult "( *> ) should equal ZipRight on error"

                    testPropertyWithConfig fsCheckConfig "( <* ) - keeps the left result"
                    <| fun (runtime: FIORuntime, a: int, b: string) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <* eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.ZipLeft eff2).UnsafeSuccess()

                        Expect.equal operatorResult a "( <* ) should return first result"
                        Expect.equal operatorResult methodResult "( <* ) should equal ZipLeft"

                    testPropertyWithConfig fsCheckConfig "( <* ) - propagates an error"
                    <| fun (runtime: FIORuntime, a: int, error: string) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.fail error

                        let operatorResult = runtime.Run(eff1 <* eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.ZipLeft eff2).UnsafeError()

                        Expect.equal operatorResult error "( <* ) should propagate second error"
                        Expect.equal operatorResult methodResult "( <* ) should equal ZipLeft on error"

                    testPropertyWithConfig fsCheckConfig "( <*> ) - zips both results"
                    <| fun (runtime: FIORuntime, a: int, b: string) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <*> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.Zip eff2).UnsafeSuccess()

                        Expect.equal operatorResult (a, b) "( <*> ) should return tuple"
                        Expect.equal operatorResult methodResult "( <*> ) should equal Zip"

                    testPropertyWithConfig fsCheckConfig "( <*> ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string, b: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <*> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.Zip eff2).UnsafeError()

                        Expect.equal operatorResult error "( <*> ) should propagate error"
                        Expect.equal operatorResult methodResult "( <*> ) should equal Zip on error"
                ]

            testList
                "Sequential ordering"
                [
                    testPropertyWithConfig fsCheckConfig "( *> ) - evaluates its operands left to right"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let log = ResizeArray<string>()
                        let left = FIO.attempt (fun () -> log.Add "left"; a) (fun ex -> ex.Message)
                        let right = FIO.attempt (fun () -> log.Add "right"; b) (fun ex -> ex.Message)

                        let result = runtime.Run(left *> right).UnsafeSuccess()

                        Expect.equal result b "( *> ) should yield the right operand's value"
                        Expect.sequenceEqual log [ "left"; "right" ] "( *> ) must evaluate the left operand first"

                    testPropertyWithConfig fsCheckConfig "( <* ) - evaluates its operands left to right"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let log = ResizeArray<string>()
                        let left = FIO.attempt (fun () -> log.Add "left"; a) (fun ex -> ex.Message)
                        let right = FIO.attempt (fun () -> log.Add "right"; b) (fun ex -> ex.Message)

                        let result = runtime.Run(left <* right).UnsafeSuccess()

                        Expect.equal result a "( <* ) should yield the left operand's value"
                        Expect.sequenceEqual log [ "left"; "right" ] "( <* ) must evaluate the left operand first"

                    testPropertyWithConfig fsCheckConfig "( <*> ) - evaluates its operands left to right"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let log = ResizeArray<string>()
                        let left = FIO.attempt (fun () -> log.Add "left"; a) (fun ex -> ex.Message)
                        let right = FIO.attempt (fun () -> log.Add "right"; b) (fun ex -> ex.Message)

                        let result = runtime.Run(left <*> right).UnsafeSuccess()

                        Expect.equal result (a, b) "( <*> ) should pair the operands in order"
                        Expect.sequenceEqual log [ "left"; "right" ] "( <*> ) must evaluate the left operand first"

                    testPropertyWithConfig fsCheckConfig "( >>= ) - evaluates the source before the continuation"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let log = ResizeArray<string>()
                        let source = FIO.attempt (fun () -> log.Add "source"; a) (fun ex -> ex.Message)

                        let effect =
                            source >>= fun _ ->
                                FIO.attempt (fun () -> log.Add "continuation"; b) (fun ex -> ex.Message)

                        let result = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal result b "( >>= ) should yield the continuation's value"
                        Expect.sequenceEqual log [ "source"; "continuation" ] "( >>= ) must run the source before the continuation"

                    testPropertyWithConfig fsCheckConfig "( *> ) - does not evaluate the right operand when the left fails"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let log = ResizeArray<string>()
                        let left: FIO<int, string> = FIO.fail error
                        let right = FIO.attempt (fun () -> log.Add "right"; 0) (fun ex -> ex.Message)

                        let result = runtime.Run(left *> right).UnsafeError()

                        Expect.equal result error "( *> ) should propagate the left error"
                        Expect.isEmpty log "( *> ) must not evaluate the right operand after the left fails"
                ]

            testList
                "Parallel composition (<&>, <&&>, &>, <&)"
                [
                    testPropertyWithConfig fsCheckConfig "( <&> ) - zips both results in parallel"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <&> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.ZipPar eff2).UnsafeSuccess()

                        Expect.equal operatorResult (a, b) "( <&> ) should return tuple"
                        Expect.equal operatorResult methodResult "( <&> ) should equal ZipPar"

                    testPropertyWithConfig fsCheckConfig "( <&> ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string, b: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <&> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.ZipPar eff2).UnsafeError()

                        Expect.equal operatorResult error "( <&> ) should propagate error"
                        Expect.equal operatorResult methodResult "( <&> ) should equal ZipPar on error"

                    testPropertyWithConfig fsCheckConfig "( <&&> ) - runs both in parallel and yields unit"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <&&> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.ZipPar(eff2).Unit()).UnsafeSuccess()

                        Expect.equal operatorResult () "( <&&> ) should return unit"
                        Expect.equal operatorResult methodResult "( <&&> ) should equal ZipPar().Unit()"

                    testPropertyWithConfig fsCheckConfig "( <&&> ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string, b: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <&&> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.ZipPar(eff2).Unit()).UnsafeError()

                        Expect.equal operatorResult error "( <&&> ) should propagate error"
                        Expect.equal operatorResult methodResult "( <&&> ) should equal ZipPar().Unit() on error"

                    testPropertyWithConfig fsCheckConfig "( &> ) - keeps the right result"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 &> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.ZipParRight eff2).UnsafeSuccess()

                        Expect.equal operatorResult b "( &> ) should return second result"
                        Expect.equal operatorResult methodResult "( &> ) should equal ZipParRight"

                    testPropertyWithConfig fsCheckConfig "( &> ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string, b: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 &> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.ZipParRight eff2).UnsafeError()

                        Expect.equal operatorResult error "( &> ) should propagate error"
                        Expect.equal operatorResult methodResult "( &> ) should equal ZipParRight on error"

                    testPropertyWithConfig fsCheckConfig "( <& ) - keeps the left result"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <& eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.ZipParLeft eff2).UnsafeSuccess()

                        Expect.equal operatorResult a "( <& ) should return first result"
                        Expect.equal operatorResult methodResult "( <& ) should equal ZipParLeft"

                    testPropertyWithConfig fsCheckConfig "( <& ) - propagates an error"
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
                ]

            testList
                "Fallback (<|>)"
                [
                    testPropertyWithConfig fsCheckConfig "( <|> ) - keeps the first success"
                    <| fun (runtime: FIORuntime, a: int, b: int) ->
                        let eff1 = FIO.succeed a
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeSuccess()

                        Expect.equal operatorResult a "( <|> ) should return first on success"
                        Expect.equal operatorResult methodResult "( <|> ) should equal OrElse"

                    testPropertyWithConfig fsCheckConfig "( <|> ) - falls back on error"
                    <| fun (runtime: FIORuntime, error: string, b: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed b

                        let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeSuccess()

                        Expect.equal operatorResult b "( <|> ) should return fallback on first error"
                        Expect.equal operatorResult methodResult "( <|> ) should equal OrElse on fallback"

                    testPropertyWithConfig fsCheckConfig "( <|> ) - fails when both fail"
                    <| fun (runtime: FIORuntime, err1: string, err2: int) ->
                        let eff1 = FIO.fail err1
                        let eff2 = FIO.fail err2

                        let operatorResult = runtime.Run(eff1 <|> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.OrElse eff2).UnsafeError()

                        Expect.equal operatorResult err2 "( <|> ) should return second error when both fail"
                        Expect.equal operatorResult methodResult "( <|> ) should equal OrElse when both fail"
                ]

            testList
                "Either fallback (<+>)"
                [
                    testPropertyWithConfig fsCheckConfig "( <+> ) - returns Choice1Of2 when the first succeeds"
                    <| fun (runtime: FIORuntime, value: int, fallback: string) ->
                        let eff1 = FIO.succeed value
                        let eff2 = FIO.succeed fallback

                        let operatorResult = runtime.Run(eff1 <+> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.OrElseEither eff2).UnsafeSuccess()

                        Expect.equal operatorResult (Choice1Of2 value) "( <+> ) should return Choice1Of2 on success"
                        Expect.equal operatorResult methodResult "( <+> ) should equal OrElseEither"

                    testPropertyWithConfig fsCheckConfig "( <+> ) - returns Choice2Of2 when the fallback succeeds"
                    <| fun (runtime: FIORuntime, error: string, fallback: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.succeed fallback

                        let operatorResult = runtime.Run(eff1 <+> eff2).UnsafeSuccess()
                        let methodResult = runtime.Run(eff1.OrElseEither eff2).UnsafeSuccess()

                        Expect.equal operatorResult (Choice2Of2 fallback) "( <+> ) should return Choice2Of2 when fallback succeeds"
                        Expect.equal operatorResult methodResult "( <+> ) should equal OrElseEither on fallback"

                    testPropertyWithConfig fsCheckConfig "( <+> ) - returns the fallback's error when both fail"
                    <| fun (runtime: FIORuntime, error: string, fallbackErr: int) ->
                        let eff1 = FIO.fail error
                        let eff2 = FIO.fail fallbackErr

                        let operatorResult = runtime.Run(eff1 <+> eff2).UnsafeError()
                        let methodResult = runtime.Run(eff1.OrElseEither eff2).UnsafeError()

                        Expect.equal operatorResult fallbackErr "( <+> ) should propagate the fallback error when both fail"
                        Expect.equal operatorResult methodResult "( <+> ) should equal OrElseEither when both fail"
                ]

            testList
                "Either race (<?>)"
                [
                    testPropertyWithConfig fsCheckConfig "( <?> ) - returns Choice1Of2 when the first racer wins"
                    <| fun (runtime: FIORuntime) ->
                        let fast = FIO.succeed 1
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed "slow")

                        let operatorResult = runtime.Run(fast <?> slow).UnsafeSuccess()
                        let methodResult = runtime.Run(fast.RaceEither slow).UnsafeSuccess()

                        Expect.equal operatorResult (Choice1Of2 1) "( <?> ) should return Choice1Of2 when the left racer wins"
                        Expect.equal operatorResult methodResult "( <?> ) should equal RaceEither"

                    testPropertyWithConfig fsCheckConfig "( <?> ) - returns Choice2Of2 when the second racer wins"
                    <| fun (runtime: FIORuntime) ->
                        let slow = (FIO.sleep (TimeSpan.FromSeconds 10.0)).FlatMap(fun () -> FIO.succeed 1)
                        let fast = FIO.succeed "fast"

                        let operatorResult = runtime.Run(slow <?> fast).UnsafeSuccess()
                        let methodResult = runtime.Run(slow.RaceEither fast).UnsafeSuccess()

                        Expect.equal operatorResult (Choice2Of2 "fast") "( <?> ) should return Choice2Of2 when the right racer wins"
                        Expect.equal operatorResult methodResult "( <?> ) should equal RaceEither"
                ]

            testList
                "Bind / Map (>>=, <!>)"
                [
                    testPropertyWithConfig fsCheckConfig "( >>= ) - chains the result into the continuation"
                    <| fun (runtime: FIORuntime, a: int, f: int -> int) ->
                        let effect = FIO.succeed a
                        let cont = fun x -> FIO.succeed (f x)

                        let operatorResult = runtime.Run(effect >>= cont).UnsafeSuccess()
                        let methodResult = runtime.Run(effect.FlatMap cont).UnsafeSuccess()

                        Expect.equal operatorResult (f a) "( >>= ) should apply continuation"
                        Expect.equal operatorResult methodResult "( >>= ) should equal FlatMap"

                    testPropertyWithConfig fsCheckConfig "( >>= ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string) ->
                        let effect = FIO.fail error
                        let cont = fun x -> FIO.succeed (x + 1)

                        let operatorResult = runtime.Run(effect >>= cont).UnsafeError()
                        let methodResult = runtime.Run(effect.FlatMap cont).UnsafeError()

                        Expect.equal operatorResult error "( >>= ) should propagate error"
                        Expect.equal operatorResult methodResult "( >>= ) should equal FlatMap on error"

                    testPropertyWithConfig fsCheckConfig "( <!> ) - maps the result"
                    <| fun (runtime: FIORuntime, a: int, f: int -> int) ->
                        let effect = FIO.succeed a

                        let operatorResult = runtime.Run(f <!> effect).UnsafeSuccess()
                        let methodResult = runtime.Run(effect.Map f).UnsafeSuccess()

                        Expect.equal operatorResult (f a) "( <!> ) should apply mapper"
                        Expect.equal operatorResult methodResult "( <!> ) should equal Map"

                    testPropertyWithConfig fsCheckConfig "( <!> ) - propagates an error"
                    <| fun (runtime: FIORuntime, error: string, f: int -> int) ->
                        let effect = FIO.fail error

                        let operatorResult = runtime.Run(f <!> effect).UnsafeError()
                        let methodResult = runtime.Run(effect.Map f).UnsafeError()

                        Expect.equal operatorResult error "( <!> ) should propagate error"
                        Expect.equal operatorResult methodResult "( <!> ) should equal Map on error"
                ]
        ]
