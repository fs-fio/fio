(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module private FSharp.FIO.Examples

open FSharp.FIO.DSL
open FSharp.FIO.Lib.IO
open FSharp.FIO.Runtime.Default

open System

let helloWorld1 () =
    let hello = FIO.Succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello
    
    task {
        let! result = fiber.Task()
        match result with
        | Ok result -> printfn $"Success: %s{result}"
        | Error error -> printfn $"Error: %A{error}"
    } |> _.Wait()

let helloWorld2 () =
    let hello: FIO<string, obj> = FIO.Succeed "Hello world! 🪻"
    let fiber: Fiber<string, obj> = (new DefaultRuntime()).Run hello

    task {
        let! result: Result<string, obj> = fiber.Task()
        match result with
        | Ok result -> printfn $"Success: %s{result}"
        | Error error -> printfn $"Error: %A{error}"
    } |> _.Wait()

let helloWorld3 () =
    let hello: FIO<obj, string> = FIO.Fail "Hello world! 🪻"
    let fiber: Fiber<obj, string> = (new DefaultRuntime()).Run hello

    task {
        let! result: Result<obj, string> = fiber.Task()
        match result with
        | Ok result -> printfn $"Success: %A{result}"
        | Error error -> printfn $"Error: %s{error}"
    } |> _.Wait()

let helloWorld4 () =
    let hello = FIO.Succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello
    FIO.UnsafePrintFiberResult fiber

let concurrency1 () =
    let concurrent = FIO.Succeed("Hello, concurrency! 🚀").Fork().FlatMap _.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    FIO.UnsafePrintFiberResult fiber

let concurrency2 () =
    let concurrent = FIO.Succeed("Hello, concurrency! 🚀").Fork() >>= _.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    FIO.UnsafePrintFiberResult fiber

let concurrency3 () =
    let taskA = FIO.Succeed "Task A completed! ✅"
    let taskB = FIO.Succeed(200, "Task B OK ✅")
    let concurrent = taskA <&> taskB
    let fiber = (new DefaultRuntime()).Run concurrent
    FIO.UnsafePrintFiberResult fiber

let computationExpression1 () =
    let hello : FIO<string, obj> =
        fio {
            return "Hello world! 🪻"
        }

    let fiber = (new DefaultRuntime()).Run hello
    FIO.UnsafePrintFiberResult fiber

let computationExpression2 () =
    let hello : FIO<obj, string> =
        fio {
            return! FIO.Fail "Hello world! 🪻"
        }

    let fiber = (new DefaultRuntime()).Run hello
    FIO.UnsafePrintFiberResult fiber

let computationExpression3 () =
    let welcome =
        fio {
            do! FConsole.PrintLine "Hello! What is your name?"
            let! name = FConsole.ReadLine
            do! FConsole.PrintLine $"Hello, %s{name}! Welcome to FIO! 🪻💜"
        }

    let fiber = (new DefaultRuntime()).Run welcome
    FIO.UnsafePrintFiberResult fiber

let unitSuccess () =
    let unit = FIO.Unit()
    let fiber = (new DefaultRuntime()).Run unit
    FIO.UnsafePrintFiberResult fiber

let interruptFiber () = 
    let longRunning =
        fio {
            do! FConsole.PrintLine "Started long-running task for 10 seconds."
            do! FIO.Sleep (TimeSpan.FromSeconds 10.0)
            do! FConsole.PrintLine "Long-running task completed!"
        }

    let interrupter =
        fio {
            let! longRunningFiber = longRunning.Fork()
            do! FConsole.PrintLine "Press Enter to interrupt the long-running task..."
            do! FConsole.ReadLine.Unit()
            do! longRunningFiber.Interrupt()
            do! FConsole.PrintLine "Interrupted long-running task."
        }

    let fiber = (new DefaultRuntime()).Run interrupter
    FIO.UnsafePrintFiberResult fiber

let examples = [
    // nameof helloWorld1, helloWorld1
    // nameof helloWorld2, helloWorld2
    // nameof helloWorld3, helloWorld3
    // nameof helloWorld4, helloWorld4
    // nameof concurrency1, concurrency1
    // nameof concurrency2, concurrency2
    // nameof concurrency3, concurrency3
    // nameof computationExpression1, computationExpression1
    // nameof computationExpression2, computationExpression2
    // nameof computationExpression3, computationExpression3
    // nameof unitSuccess, unitSuccess
    nameof interruptFiber, interruptFiber
]

examples |> List.iteri (fun i (name, example) ->
    printfn $"🔥 Running example: {name}\n"
    example()
    if i < examples.Length - 1 then
        Console.WriteLine "\n⏩ Press Enter to run next example..."
        Console.ReadLine() |> ignore)

Console.WriteLine "\n✅ All examples completed. Press Enter to exit."
Console.ReadLine() |> ignore
