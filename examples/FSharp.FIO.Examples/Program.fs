(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module private FSharp.FIO.Examples

open FSharp.FIO.DSL
open FSharp.FIO.Lib.IO
open FSharp.FIO.Runtime.Concurrent

open System

let helloWorld1 () =
    let runtime = new Runtime()
    let hello = FIO.Succeed "Hello world! 🪻"
    let fiber = runtime.Run hello
    
    task {
        let! result = fiber.Task ()
        match result with
        | Ok result -> printfn $"Success: %s{result}"
        | Error error -> printfn $"Error: %A{error}"
    } |> ignore

let helloWorld2 () : unit =
    let runtime = new Runtime()
    let hello: FIO<string, obj> = FIO.Succeed "Hello world! 🪻"
    let fiber: Fiber<string, obj> = runtime.Run hello
    
    task {
        let! result: Result<string, obj> = fiber.Task ()
        match result with
        | Ok result -> printfn $"Success: %s{result}"
        | Error error -> printfn $"Error: %A{error}"
    } |> ignore

let helloWorld3 () : unit =
    let runtime = new Runtime()
    let hello: FIO<obj, string> = FIO.Fail "Hello world! 🪻"
    let fiber: Fiber<obj, string> = runtime.Run hello
    
    task {
        let! result: Result<obj, string> = fiber.Task ()
        match result with
        | Ok result -> printfn $"Success: %A{result}"
        | Error error -> printfn $"Error: %s{error}"
    } |> ignore

let helloWorld4 () =
    let runtime = new Runtime()
    let hello = FIO.Succeed "Hello world! 🪻"
    let fiber = runtime.Run hello
    
    task {
        let! result = fiber.Task()
        printfn $"%A{result}"
    } |> ignore

let helloWorld5 () =
    let runtime = new Runtime()
    let hello = !+ "Hello world! 🪻"
    let fiber = runtime.Run hello
    
    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

let helloWorld6 () =
    let runtime = new Runtime()
    let hello = !- "Hello world! 🪻"
    let fiber = runtime.Run hello
    
    task {
        let! result = fiber.Task()
        printfn $"%A{result}"
    } |> ignore

let concurrency1 () =
    let runtime = new Runtime()
    let concurrent = (FIO.Succeed 42).Fork().FlatMap _.Join()
    let fiber = runtime.Run concurrent
    
    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

let concurrency2 () =
    let runtime = new Runtime()
    let concurrent = !~> !+ 42 >>= fun fiber -> !~~> fiber
    let fiber = runtime.Run concurrent
    
    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

let concurrency3 () =
    let runtime = new Runtime()
    let taskA = !+ "Task A completed!"
    let taskB = !+ (200, "Task B OK")
    let concurrent = taskA <!> taskB
    let fiber = runtime.Run concurrent
    
    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

let computationExpression1 () =
    let runtime = new Runtime()
    let hello : FIO<string, obj> =
        fio {
            return "Hello world! 🪻"
        }

    let fiber = runtime.Run hello
    
    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

let computationExpression2 () =
    let runtime = new Runtime()
    let hello : FIO<obj, string> =
        fio {
            return! !- "Hello world! 🪻"
        }

    let fiber = runtime.Run hello

    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

let computationExpression3 () =
    let runtime = new Runtime()
    let welcome =
        fio {
            do! FConsole.PrintLine "Hello! What is your name?"
            let! name = FConsole.ReadLine ()
            do! FConsole.PrintLine $"Hello, %s{name}! Welcome to FIO! 🪻💜"
        }

    let fiber = runtime.Run welcome

    task {
        let! result = fiber.Task ()
        printfn $"%A{result}"
    } |> ignore

helloWorld1 ()
Console.ReadLine () |> ignore

helloWorld2 ()
Console.ReadLine () |> ignore

helloWorld3 ()
Console.ReadLine () |> ignore

helloWorld4 ()
Console.ReadLine () |> ignore

helloWorld5 ()
Console.ReadLine () |> ignore

helloWorld6 ()
Console.ReadLine () |> ignore

concurrency1 ()
Console.ReadLine () |> ignore

concurrency2 ()
Console.ReadLine () |> ignore

concurrency3 ()
Console.ReadLine () |> ignore

computationExpression1 ()
Console.ReadLine () |> ignore

computationExpression2 ()
Console.ReadLine () |> ignore

computationExpression3 ()
Console.ReadLine () |> ignore
