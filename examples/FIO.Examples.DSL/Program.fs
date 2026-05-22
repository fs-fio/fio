/// <summary>Provides standalone examples of core FIO primitives including effect creation, concurrency with fibers, channels, atomic references, promises, and semaphores.</summary>
module private FIO.Examples

open FIO.DSL
open FIO.Console
open FIO.Runtime.Default

open System

/// <summary>Builds a hello-world effect using FIO.succeed and runs it with DefaultRuntime, demonstrating Task-based result pattern matching on Succeeded, Failed, and Interrupted outcomes.</summary>
/// <remarks>Shows the lowest-level way to run an effect: create a runtime, call Run to obtain a Fiber, await its Task, and match on FiberResult.</remarks>
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
    |> fun t -> t.GetAwaiter().GetResult()

/// <summary>Builds a hello-world effect with explicit FIO and Fiber type annotations, illustrating the type signatures involved in creating and running an effect.</summary>
/// <remarks>Identical to helloWorld1 but adds type annotations to clarify that FIO.succeed produces FIO&lt;string, obj&gt; and Run yields Fiber&lt;string, obj&gt;.</remarks>
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
    |> fun t -> t.GetAwaiter().GetResult()

/// <summary>Builds a failing effect using FIO.fail and runs it, demonstrating how typed errors flow through the Failed branch of FiberResult pattern matching.</summary>
/// <remarks>Shows that FIO.fail places the value on the error channel, which surfaces as the Failed case when the fiber completes.</remarks>
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
    |> fun t -> t.GetAwaiter().GetResult()

/// <summary>Builds a hello-world effect and runs it using UnsafePrintResult, demonstrating the simplest way to execute and display an effect's outcome.</summary>
/// <remarks>UnsafePrintResult is a convenience method that blocks, awaits the fiber, and prints the result — useful for quick debugging and examples.</remarks>
let helloWorld4 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

/// <summary>Builds a concurrent effect that forks a succeed effect onto a new fiber and joins it, using FlatMap method syntax for composition.</summary>
/// <remarks>Fork creates a new fiber running the effect concurrently, and Join awaits its result — FlatMap chains these two steps sequentially.</remarks>
let concurrency1 () =
    let concurrent =
        FIO.succeed("Hello, concurrency! 🚀").Fork().FlatMap(fun f -> f.Join())

    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

/// <summary>Builds a concurrent effect that forks and joins a fiber using the bind operator (&gt;&gt;=), demonstrating operator-style composition as an alternative to method syntax.</summary>
/// <remarks>Functionally identical to concurrency1 but uses the &gt;&gt;= infix operator instead of FlatMap, showing the more compact operator pipeline style.</remarks>
let concurrency2 () =
    let concurrent = FIO.succeed("Hello, concurrency! 🚀").Fork() >>= fun f -> f.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

/// <summary>Builds two independent effects and combines them for parallel execution using the &lt;&amp;&gt; operator, producing a tuple of both results.</summary>
/// <remarks>The &lt;&amp;&gt; operator forks both effects onto separate fibers, runs them concurrently, and returns a tuple of their results when both complete.</remarks>
let concurrency3 () =
    let taskA = FIO.succeed "Task A completed! ✅"
    let taskB = FIO.succeed (200, "Task B OK ✅")
    let concurrent = taskA <&> taskB
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

/// <summary>Builds a simple effect using the fio computation expression with return, demonstrating the most basic CE syntax for wrapping a pure value.</summary>
/// <remarks>The fio { return x } expression is equivalent to FIO.succeed x, lifting a value into a successful effect.</remarks>
let computationExpression1 () =
    let hello: FIO<string, obj> = fio { return "Hello world! 🪻" }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

/// <summary>Builds a failing effect using the fio computation expression with return! to delegate to FIO.fail, demonstrating CE syntax for effect chaining.</summary>
/// <remarks>The return! keyword inside fio { } unwraps the inner effect, allowing composition of effects that may succeed or fail.</remarks>
let computationExpression2 () =
    let hello: FIO<obj, string> = fio { return! FIO.fail "Hello world! 🪻" }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

/// <summary>Builds an interactive console I/O effect using the fio computation expression, demonstrating let! for reading input and do! for printing output.</summary>
/// <remarks>Shows how the fio CE enables imperative-style sequencing of effectful operations: printing a prompt, reading user input, and printing a personalized greeting.</remarks>
let computationExpression3 () =
    let welcome =
        fio {
            do! Console.printLine ("Hello! What is your name?", id)
            let! name = Console.readLine id
            do! Console.printLine ($"Hello, %s{name}! Welcome to FIO! 🪻💜", id)
        }

    let fiber = (new DefaultRuntime()).Run welcome
    fiber.UnsafePrintResult()

/// <summary>Builds an effect that forks a long-running sleep task and interrupts it on user input, demonstrating fiber interruption for cancelling concurrent work.</summary>
/// <remarks>Shows Fork to create a background fiber, Console.readLine to wait for user action, and Interrupt to cancel the sleeping fiber — a common pattern for timeout or cancellation logic.</remarks>
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


/// <summary>Provides the registry of all example names paired with their runner functions for sequential interactive execution.</summary>
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
