/// <summary>
/// Core FIO examples demonstrating basic effect creation, execution, and concurrency patterns.
/// </summary>
module private FIO.Examples

open FIO.DSL
open FIO.Console
open FIO.Runtime.Default

open System

/// <summary>
/// Basic hello world using FIO.succeed with Task-based result handling.
/// </summary>
let helloWorld1 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello

    task {
        let! result = fiber.Task()

        match result with
        | Succeeded result -> printfn $"Success: %s{result}"
        | Failed error -> printfn $"Error: %A{error}"
        | Interrupted ex -> printfn $"Interrupted: %s{ex.Message}"
    }
    |> (fun t -> t.GetAwaiter().GetResult())

/// <summary>
/// Hello world with explicit type annotations showing FIO type signatures.
/// </summary>
let helloWorld2 () =
    let hello: FIO<string, obj> = FIO.succeed "Hello world! 🪻"
    let fiber: Fiber<string, obj> = (new DefaultRuntime()).Run hello

    task {
        let! result = fiber.Task()

        match result with
        | Succeeded result -> printfn $"Success: %s{result}"
        | Failed error -> printfn $"Error: %A{error}"
        | Interrupted ex -> printfn $"Interrupted: %s{ex.Message}"
    }
    |> (fun t -> t.GetAwaiter().GetResult())

/// <summary>
/// Demonstrates FIO.fail for effect failure with typed errors.
/// </summary>
let helloWorld3 () =
    let hello: FIO<obj, string> = FIO.fail "Hello world! 🪻"
    let fiber: Fiber<obj, string> = (new DefaultRuntime()).Run hello

    task {
        let! result = fiber.Task()

        match result with
        | Succeeded result -> printfn $"Success: %A{result}"
        | Failed error -> printfn $"Error: %s{error}"
        | Interrupted ex -> printfn $"Interrupted: %s{ex.Message}"
    }
    |> (fun t -> t.GetAwaiter().GetResult())

/// <summary>
/// Simplified hello world using UnsafePrintResult for quick debugging.
/// </summary>
let helloWorld4 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

/// <summary>
/// Fork and join pattern using FlatMap method syntax.
/// </summary>
let concurrency1 () =
    let concurrent =
        FIO.succeed("Hello, concurrency! 🚀").Fork().FlatMap(fun f -> f.Join())

    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

/// <summary>
/// Fork and join pattern using bind operator (&gt;&gt;=).
/// </summary>
let concurrency2 () =
    let concurrent = FIO.succeed("Hello, concurrency! 🚀").Fork() >>= fun f -> f.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

/// <summary>
/// Parallel composition using the &lt;&amp;&gt; operator.
/// </summary>
let concurrency3 () =
    let taskA = FIO.succeed "Task A completed! ✅"
    let taskB = FIO.succeed (200, "Task B OK ✅")
    let concurrent = taskA <&> taskB
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

/// <summary>
/// Basic computation expression with return.
/// </summary>
let computationExpression1 () =
    let hello: FIO<string, obj> = fio { return "Hello world! 🪻" }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

/// <summary>
/// Computation expression with return! for effect chaining.
/// </summary>
let computationExpression2 () =
    let hello: FIO<obj, string> = fio { return! FIO.fail "Hello world! 🪻" }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

/// <summary>
/// Interactive console I/O using the fio computation expression.
/// </summary>
let computationExpression3 () =
    let welcome =
        fio {
            do! Console.printLine ("Hello! What is your name?", id)
            let! name = Console.readLine id
            do! Console.printLine ($"Hello, %s{name}! Welcome to FIO! 🪻💜", id)
        }

    let fiber = (new DefaultRuntime()).Run welcome
    fiber.UnsafePrintResult()

/// <summary>
/// Demonstrates fiber interruption for cancelling long-running tasks.
/// </summary>
let interruptFiber () =
    let longRunning =
        fio {
            do! Console.printLine ("Started long-running task for 10 seconds.", id)
            do! FIO.sleep (TimeSpan.FromSeconds 10.0, id)
            do! Console.printLine ("Long-running task completed!", id)
        }

    let interrupter =
        fio {
            let! longRunningFiber = longRunning.Fork()
            do! Console.printLine ("Press Enter to interrupt the long-running task...", id)
            do! (Console.readLine id).Unit()
            do! longRunningFiber.Interrupt()
            do! Console.printLine ("Interrupted long-running task.", id)
        }

    let fiber = (new DefaultRuntime()).Run interrupter
    fiber.UnsafePrintResult()

/// <summary>
/// Atomic reference counter using Ref for thread-safe state.
/// </summary>
let refCounter () =
    let effect =
        fio {
            let! counter = Ref.makeValue 0
            let increment = counter.UpdateAndGet((fun n -> n + 1), id)
            let! _ = FIO.collectAllPar (List.replicate 10 increment)
            let! final = counter.Get()
            do! Console.printLine ($"Final counter value: {final} (expected: 10)", id)
        }

    let fiber = (new DefaultRuntime()).Run effect
    fiber.UnsafePrintResult()

/// <summary>
/// One-shot synchronization using Promise for producer-consumer handoff.
/// </summary>
let promiseHandoff () =
    let effect =
        fio {
            let! promise = Promise.make<string, exn> ()

            let waiter =
                fio {
                    do! Console.printLine ("Waiter: waiting for value...", id)
                    let! value = promise.Await(id)
                    do! Console.printLine ($"Waiter: received '{value}'", id)
                }

            let producer =
                fio {
                    do! FIO.sleep (TimeSpan.FromMilliseconds 100.0, id)
                    do! Console.printLine ("Producer: sending value...", id)
                    let! _ = promise.Succeed("Hello from producer!", id)
                    return ()
                }

            let! _ = waiter <&> producer
            return ()
        }

    let fiber = (new DefaultRuntime()).Run effect
    fiber.UnsafePrintResult()

/// <summary>
/// Counting semaphore for limiting concurrent access to a resource pool.
/// </summary>
let semaphorePool () =
    let effect =
        fio {
            let! sem = Semaphore.make 2

            let worker id =
                sem.WithPermit(
                    fio {
                        do! Console.printLine ($"Worker {id}: acquired permit", Operators.id)
                        do! FIO.sleep (TimeSpan.FromMilliseconds 50.0, Operators.id)
                        do! Console.printLine ($"Worker {id}: releasing permit", Operators.id)
                    },
                    Operators.id
                )

            do! FIO.collectAllPar(List.init 5 worker).Unit()
            do! Console.printLine ("All workers completed.", id)
        }

    let fiber = (new DefaultRuntime()).Run effect
    fiber.UnsafePrintResult()

/// <summary>
/// List of all examples with their names for sequential execution.
/// </summary>
let examples =
    [
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

examples
|> List.iteri (fun i (name, example) ->
    printfn $"🔥 Running example: {name}\n"
    example ()

    if i < examples.Length - 1 then
        System.Console.WriteLine "\n⏩ Press Enter to run next example..."
        System.Console.ReadLine() |> ignore)

System.Console.WriteLine "\n✅ All examples completed. Press Enter to exit."
System.Console.ReadLine() |> ignore
