module private FIO.Examples.DSL

open FIO.DSL
open FIO.Console
open FIO.Runtime.Default

open System

let runtime = new DefaultRuntime()

// Runs a successful effect and prints the result by manually awaiting the fiber's task.
let helloWorld1 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = runtime.Run hello

    task {
        let! result = fiber.Task()
        match result with
        | Succeeded result -> printfn $"Success: %s{result}"
        | Failed error -> printfn $"Error: %A{error}"
        | Interrupted ex -> printfn $"Interrupted: %s{ex.Message}"
    }
    |> fun task -> task.GetAwaiter().GetResult()

// Same as helloWorld1, but spells out the explicit FIO and Fiber type annotations.
let helloWorld2 () =
    let hello: FIO<string, obj> = FIO.succeed "Hello world! 🪻"
    let fiber: Fiber<string, obj> = runtime.Run hello

    task {
        let! result = fiber.Task()
        match result with
        | Succeeded result -> printfn $"Success: %s{result}"
        | Failed error -> printfn $"Error: %A{error}"
        | Interrupted ex -> printfn $"Interrupted: %s{ex.Message}"
    }
    |> fun task -> task.GetAwaiter().GetResult()

// Runs a failing effect and shows the Failed branch of the result.
let helloWorld3 () =
    let hello: FIO<obj, string> = FIO.fail "Hello world! 🪻"
    let fiber: Fiber<obj, string> = runtime.Run hello

    task {
        let! result = fiber.Task()
        match result with
        | Succeeded result -> printfn $"Success: %A{result}"
        | Failed error -> printfn $"Error: %s{error}"
        | Interrupted ex -> printfn $"Interrupted: %s{ex.Message}"
    }
    |> fun task -> task.GetAwaiter().GetResult()

// Same effect as helloWorld1, printed with the UnsafePrintResult convenience helper.
let helloWorld4 () =
    let hello = FIO.succeed "Hello world! 🪻"
    let fiber = runtime.Run hello
    fiber.UnsafePrintResult()

// Forks an effect onto its own fiber and joins it back, wired with .Fork().FlatMap.
let concurrency1 () =
    let concurrent =
        FIO.succeed("Hello, concurrency! 🚀")
            .Fork().FlatMap <| fun fiber -> fiber.Join()

    let fiber = runtime.Run concurrent
    fiber.UnsafePrintResult()

// The same fork/join, expressed with the >>= bind operator instead.
let concurrency2 () =
    let concurrent =
        FIO.succeed("Hello, concurrency! 🚀").Fork()
        >>= fun fiber -> fiber.Join()
    
    let fiber = runtime.Run concurrent
    fiber.UnsafePrintResult()

// Runs two effects in parallel with <&> and collects both results as a tuple.
let concurrency3 () =
    let taskA = FIO.succeed "Task A completed! ✅"
    let taskB = FIO.succeed (200, "Task B OK ✅")
    let concurrent = taskA <&> taskB
    let fiber = runtime.Run concurrent
    fiber.UnsafePrintResult()

// Builds a successful effect with the fio { } computation expression.
let computationExpression1 () =
    let hello: FIO<string, obj> =
        fio {
            return "Hello world! 🪻"
        }

    let fiber = runtime.Run hello
    fiber.UnsafePrintResult()

// Fails from inside a computation expression with return! FIO.fail.
let computationExpression2 () =
    let hello: FIO<obj, string> =
        fio {
            return! FIO.fail "Hello world! 🪻"
        }

    let fiber = runtime.Run hello
    fiber.UnsafePrintResult()

// Sequences console reads and writes inside a computation expression.
let computationExpression3 () =
    let welcome =
        fio {
            do! Console.printLine "Hello! What is your name?" id
            let! name = Console.readLine id
            do! Console.printLine $"Hello, %s{name}! Welcome to FIO! 🪻💜" id
        }

    let fiber = runtime.Run welcome
    fiber.UnsafePrintResult()

// Forks a long-running fiber and interrupts it on user input.
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

    let fiber = runtime.Run interrupter
    fiber.UnsafePrintResult()

// A quick tour of the core operators: map, zipRight, zipPar, and orElse.
let operatorsTour () =
    let run label effect =
        printf $"{label}: "
        (runtime.Run effect).UnsafePrintResult()

    run "map (<!>)" ((+) 10 <!> FIO.succeed 1)
    run "zipRight (*>)" (FIO.succeed "ignored" *> FIO.succeed "kept")
    run "zipPar (<&>)" (FIO.succeed 1 <&> FIO.succeed 2)
    run "orElse (<|>)" (FIO.fail (exn "boom") <|> FIO.succeed "recovered")

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
        nameof operatorsTour, operatorsTour
    ]

examples |> List.iteri (fun index (name, example) ->
    printfn $"Running example: {name}\n"
    example ()

    if index < examples.Length - 1 then
        printfn "\nPress Enter to run next example..."
        Console.ReadLine() |> ignore)

printfn "\nAll examples completed. Press Enter to exit."
Console.ReadLine() |> ignore

(runtime :> IDisposable).Dispose()
