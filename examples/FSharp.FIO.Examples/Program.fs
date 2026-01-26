module private FSharp.FIO.Examples

open FSharp.FIO
open FSharp.FIO.DSL
open FSharp.FIO.Runtime.Default

open System

let helloWorld1 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello

    task {
        let! result = fiber.Task()
        match result with
        | Succeeded result -> printfn $"Success: %s{result}"
        | Failed error -> printfn $"Error: %A{error}"
        | Interrupted exn -> printfn $"Interrupted: %s{exn.Message}"
    } |> _.GetAwaiter().GetResult()

let helloWorld2 () =
    let hello: FIO<string, obj> = FIO.succeed "Hello world! 🪻"
    let fiber: Fiber<string, obj> = (new DefaultRuntime()).Run hello

    task {
        let! result: FiberResult<string, obj> = fiber.Task()
        match result with
        | Succeeded result -> printfn $"Success: %s{result}"
        | Failed error -> printfn $"Error: %A{error}"
        | Interrupted exn -> printfn $"Interrupted: %s{exn.Message}"
    } |> _.GetAwaiter().GetResult()

let helloWorld3 () =
    let hello: FIO<obj, string> = FIO.fail "Hello world! 🪻"
    let fiber: Fiber<obj, string> = (new DefaultRuntime()).Run hello

    task {
        let! result: FiberResult<obj, string> = fiber.Task()
        match result with
        | Succeeded result -> printfn $"Success: %A{result}"
        | Failed error -> printfn $"Error: %s{error}"
        | Interrupted exn -> printfn $"Interrupted: %s{exn.Message}"
    } |> _.GetAwaiter().GetResult()

let helloWorld4 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

let concurrency1 () =
    let concurrent = FIO.succeed("Hello, concurrency! 🚀").Fork().FlatMap _.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

let concurrency2 () =
    let concurrent = FIO.succeed("Hello, concurrency! 🚀").Fork() >>= _.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

let concurrency3 () =
    let taskA = FIO.succeed "Task A completed! ✅"
    let taskB = FIO.succeed(200, "Task B OK ✅")
    let concurrent = taskA <&> taskB
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

let computationExpression1 () =
    let hello : FIO<string, obj> =
        fio {
            return "Hello world! 🪻"
        }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

let computationExpression2 () =
    let hello : FIO<obj, string> =
        fio {
            return! FIO.fail "Hello world! 🪻"
        }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

let computationExpression3 () =
    let welcome =
        fio {
            do! Console.printLineExn "Hello! What is your name?"
            let! name = Console.readLineExn
            do! Console.printLineExn $"Hello, %s{name}! Welcome to FIO! 🪻💜"
        }

    let fiber = (new DefaultRuntime()).Run welcome
    fiber.UnsafePrintResult()

let interruptFiber () =
    let longRunning =
        fio {
            do! Console.printLineExn "Started long-running task for 10 seconds."
            do! FIO.sleepExn(TimeSpan.FromSeconds 10.0)
            do! Console.printLineExn "Long-running task completed!"
        }

    let interrupter =
        fio {
            let! longRunningFiber = longRunning.Fork()
            do! Console.printLineExn "Press Enter to interrupt the long-running task..."
            do! Console.readLineExn.Unit()
            do! longRunningFiber.Interrupt()
            do! Console.printLineExn "Interrupted long-running task."
        }

    let fiber = (new DefaultRuntime()).Run interrupter
    fiber.UnsafePrintResult()

let refCounter () =
    let effect =
        fio {
            let! counter = Ref.makeValue 0
            let increment = counter.UpdateAndGetExn(fun n -> n + 1)
            let! _ = FIO.collectAllPar(List.replicate 10 increment)
            let! final = counter.Get()
            do! Console.printLineExn $"Final counter value: {final} (expected: 10)"
        }
    let fiber = (new DefaultRuntime()).Run effect
    fiber.UnsafePrintResult()

let promiseHandoff () =
    let effect =
        fio {
            let! promise = Promise.make<string, exn>()
            let waiter = fio {
                do! Console.printLineExn "Waiter: waiting for value..."
                let! value = promise.AwaitExn()
                do! Console.printLineExn $"Waiter: received '{value}'"
            }
            let producer = fio {
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 100.0)
                do! Console.printLineExn "Producer: sending value..."
                let! _ = promise.SucceedExn "Hello from producer!"
                return ()
            }
            let! _ = waiter <&> producer
            return ()
        }
    let fiber = (new DefaultRuntime()).Run effect
    fiber.UnsafePrintResult()

let semaphorePool () =
    let effect =
        fio {
            let! sem = Semaphore.make 2
            let worker id =
                sem.WithPermitExn(fio {
                    do! Console.printLineExn $"Worker {id}: acquired permit"
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 50.0)
                    do! Console.printLineExn $"Worker {id}: releasing permit"
                })
            do! FIO.collectAllPar(List.init 5 worker).Unit()
            do! Console.printLineExn "All workers completed."
        }
    let fiber = (new DefaultRuntime()).Run effect
    fiber.UnsafePrintResult()

let examples = [
    nameof helloWorld1, helloWorld1
    nameof helloWorld2, helloWorld2
    nameof helloWorld3, helloWorld3
    nameof helloWorld4, helloWorld4
    nameof concurrency1, concurrency1
    nameof concurrency2, concurrency2
    nameof concurrency3, concurrency3
    nameof computationExpression1, computationExpression1
    nameof computationExpression2, computationExpression2
    nameof computationExpression3, computationExpression3
    nameof interruptFiber, interruptFiber
    nameof refCounter, refCounter
    nameof promiseHandoff, promiseHandoff
    nameof semaphorePool, semaphorePool
]

examples |> List.iteri (fun i (name, example) ->
    printfn $"🔥 Running example: {name}\n"
    example()
    if i < examples.Length - 1 then
        System.Console.WriteLine "\n⏩ Press Enter to run next example..."
        System.Console.ReadLine() |> ignore)

System.Console.WriteLine "\n✅ All examples completed. Press Enter to exit."
System.Console.ReadLine() |> ignore
