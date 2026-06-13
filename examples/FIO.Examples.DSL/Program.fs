module private FIO.Examples.DSL

open FIO.DSL
open FIO.Console
open FIO.Runtime.Default

open System

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

let helloWorld4 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

let concurrency1 () =
    let concurrent =
        FIO.succeed("Hello, concurrency! 🚀").Fork().FlatMap(fun f -> f.Join())

    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

let concurrency2 () =
    let concurrent = FIO.succeed("Hello, concurrency! 🚀").Fork() >>= fun f -> f.Join()
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

let concurrency3 () =
    let taskA = FIO.succeed "Task A completed! ✅"
    let taskB = FIO.succeed (200, "Task B OK ✅")
    let concurrent = taskA <&> taskB
    let fiber = (new DefaultRuntime()).Run concurrent
    fiber.UnsafePrintResult()

let computationExpression1 () =
    let hello: FIO<string, obj> = fio { return "Hello world! 🪻" }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

let computationExpression2 () =
    let hello: FIO<obj, string> = fio { return! FIO.fail "Hello world! 🪻" }

    let fiber = (new DefaultRuntime()).Run hello
    fiber.UnsafePrintResult()

let computationExpression3 () =
    let welcome =
        fio {
            do! Console.printLine "Hello! What is your name?" id
            let! name = Console.readLine id
            do! Console.printLine $"Hello, %s{name}! Welcome to FIO! 🪻💜" id
        }

    let fiber = (new DefaultRuntime()).Run welcome
    fiber.UnsafePrintResult()

let interruptFiber () =
    let longRunning =
        fio {
            do! Console.printLine "Started long-running task for 10 seconds." id
            do! FIO.sleep (TimeSpan.FromSeconds 10.0) id
            do! Console.printLine "Long-running task completed!" id
        }

    let interrupter =
        fio {
            let! longRunningFiber = longRunning.Fork()
            do! Console.printLine "Press Enter to interrupt the long-running task..." id
            do! (Console.readLine id).Unit()
            do! longRunningFiber.Interrupt ExplicitInterrupt "User requested interruption"
            do! Console.printLine "Interrupted long-running task." id
        }

    let fiber = (new DefaultRuntime()).Run interrupter
    fiber.UnsafePrintResult()


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
