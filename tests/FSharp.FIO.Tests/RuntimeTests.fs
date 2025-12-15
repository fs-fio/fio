(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module FSharp.FIO.Tests.RuntimeTests

open FSharp.FIO.DSL
open FSharp.FIO.Runtime

open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open System.Threading.Tasks

[<Properties(Arbitrary = [| typeof<RuntimeTests> |])>]
type RuntimeTests () =

    let result (fiber: Fiber<'R, 'E>) =
        match fiber.Task().GetAwaiter().GetResult() with
        | Ok res -> res
        | Error _ -> failwith "Error happened when result was expected!"

    let error (fiber: Fiber<'R, 'E>) =
        match fiber.Task().GetAwaiter().GetResult() with
        | Ok _ -> failwith "Result happened when error was expected!"
        | Error err -> err
    
    static member Runtime() : Arbitrary<FRuntime> =
        Arb.fromGen <| Gen.oneof [
            Gen.constant (new Direct.Runtime())
            Gen.constant (new Cooperative.Runtime())
            Gen.constant (new Concurrent.Runtime())
        ]
    
    [<Property>]
    member _.``Succeed always succeeds`` (runtime: FRuntime, res: int) =
        let eff = FIO.Succeed res
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``Fail always fails`` (runtime: FRuntime, err: int) =
        let eff = FIO.Fail err
        
        let actual = error <| runtime.Run eff
        let expected = err

        actual = expected

    [<Property>]
    member _.``FromFunc with onError succeeds when no exception is thrown`` (runtime: FRuntime, res: int) =
        let func () =
            res
            
        let onError exn =
            exn
            
        let eff = FIO.FromFunc<int, exn> (func, onError)
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected

    [<Property>]
    member _.``FromFunc with onError fails when exception is thrown and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let func () =
            invalidOp exnMsg
            res
            
        let onError (exn: exn) =
            exn.Message
            
        let eff = FIO.FromFunc<int, string> (func, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected

    [<Property>]
    member _.``FromFunc succeeds when no exception is thrown`` (runtime: FRuntime, res: int) =
        let func () =
            res
        
        let eff = FIO.FromFunc<int, exn> func
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``FromFunc fails when exception is thrown`` (runtime: FRuntime, res: int, exnMsg: string) =
        let func () =
            invalidOp exnMsg
            res
            
        let eff = FIO.FromFunc<int, exn> func
        
        let actual = (error <| runtime.Run eff).Message
        let expected = exnMsg

        actual = expected
    
    [<Property>]
    member _.``FromResult always succeeds on Ok`` (runtime: FRuntime, res: int) =
        let res' = Ok res
        
        let eff = FIO.FromResult res'
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``FromResult always fails on Error`` (runtime: FRuntime, err: int) =
        let err' = Error err
        
        let eff = FIO.FromResult err'
        
        let actual = error <| runtime.Run eff
        let expected = err

        actual = expected
    
    [<Property>]
    member _.``FromOption always succeeds on Some`` (runtime: FRuntime, res: int, err: string) =
        let some = Some res
        let onNone () = err
        
        let eff = FIO.FromOption (some, onNone)
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``FromOption always fails on None`` (runtime: FRuntime, err: string) =
        let none = None
        let onNone () = err
        
        let eff = FIO.FromOption (none, onNone)
        
        let actual = error <| runtime.Run eff
        let expected = err

        actual = expected
    
    [<Property>]
    member _.``FromChoice always succeeds on Choice1`` (runtime: FRuntime, res: int) =
        let choice = Choice1Of2 res
        
        let eff = FIO.FromChoice choice
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``FromChoice always fails on Choice2`` (runtime: FRuntime, err: int) =
        let choice = Choice2Of2 err
        
        let eff = FIO.FromChoice choice
        
        let actual = error <| runtime.Run eff
        let expected = err

        actual = expected
    
    [<Property>]
    member _.``AwaitTask with onError always succeeds when the task succeeds`` (runtime: FRuntime) =
        let t =
            Task.Run(fun () -> ())
            
        let onError exn =
            exn
        
        let eff = FIO.AwaitTask<unit, exn> (t, onError)
        
        let actual = result <| runtime.Run eff
        let expected = ()

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitTask with onError always fails when the task fails and converts error`` (runtime: FRuntime, exnMsg: string) =
        let t = Task.Run(fun () ->
            invalidOp exnMsg)
        
        let onError (exn: exn) =
            exn.Message
        
        let eff = FIO.AwaitTask<unit, string> (t, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected && t.IsFaulted
    
    [<Property>]
    member _.``AwaitTask always succeeds when the task succeeds`` (runtime: FRuntime) =
        let t =
            Task.Run(fun () -> ())

        let eff = FIO.AwaitTask<unit, exn> t
        
        let actual = result <| runtime.Run eff
        let expected = ()

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitTask always fails when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let t = Task.Run(fun () ->
            invalidOp exnMsg)
        
        let eff = FIO.AwaitTask<unit, string> t
        
        let actual = (error <| runtime.Run eff).Message
        let expected = exnMsg

        actual = expected && t.IsFaulted
    
    [<Property>]
    member _.``AwaitGenericTask with onError always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let t = task {
            return res
        }
        
        let onError exn =
            exn

        let eff = FIO.AwaitGenericTask<int, exn> (t, onError)
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitGenericTask with onError always fails when the task fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let t = task {
            invalidOp exnMsg
            return res
        }
        
        let onError (exn: exn) =
            exn.Message

        let eff = FIO.AwaitGenericTask<int, string> (t, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected && t.IsFaulted
    
    [<Property>]
    member _.``AwaitGenericTask always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let t = task {
            return res
        }
      
        let eff = FIO.AwaitGenericTask<int, exn> t
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitGenericTask always fails when the task fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let t = task {
            invalidOp exnMsg
            return res
        }

        let eff = FIO.AwaitGenericTask<int, string> t
        
        let actual = (error <| runtime.Run eff).Message
        let expected = exnMsg

        actual = expected && t.IsFaulted
    
    [<Property>]
    member _.``AwaitAsync with onError always succeeds when the computation succeeds`` (runtime: FRuntime, res: int) =
        let a = async {
            return res
        }
        
        let onError exn =
            exn

        let eff = FIO.AwaitAsync<int, exn> (a, onError)
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``AwaitAsync with onError always fails when the computation fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let a = async {
            invalidOp exnMsg
            return res
        }
        
        let onError (exn: exn) =
            exn.Message

        let eff = FIO.AwaitAsync<int, string> (a, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected
    
    [<Property>]
    member _.``AwaitAsync always succeeds when the computation succeeds`` (runtime: FRuntime, res: int) =
        let a = async {
            return res
        }
        
        let eff = FIO.AwaitAsync<int, exn> a
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``AwaitAsync always fails when the computation fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let a = async {
            invalidOp exnMsg
            return res
        }
        
        let eff = FIO.AwaitAsync<int, string> a
        
        let actual = (error <| runtime.Run eff).Message
        let expected = exnMsg

        actual = expected
    
    [<Property>]
    member _.``FromTask with onError always succeeds when the task succeeds`` (runtime: FRuntime) =
        let lazyTask = fun () ->
            Task.Run(fun () -> ())
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromTask<Fiber<unit, exn>, exn> (lazyTask, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask effect with onError always succeeds with fiber when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let lazyTask = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromTask<Fiber<unit, exn>, exn> (lazyTask, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask fiber with onError always fails when the task fails and converts error`` (runtime: FRuntime, exnMsg: string) =
        let lazyTask = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let onError (exn: exn) =
            exn.Message
        
        let eff = FIO.FromTask<Fiber<unit, string>, string> (lazyTask, onError)
        
        let actual = error <| result (runtime.Run eff)
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``FromTask always succeeds when the task succeeds`` (runtime: FRuntime) =
        let lazyTask = fun () ->
            Task.Run(fun () -> ())
        
        let eff = FIO.FromTask<Fiber<unit, exn>, exn> lazyTask
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask effect always succeeds with fiber when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let lazyTask = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let eff = FIO.FromTask<Fiber<unit, exn>, exn> lazyTask
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask fiber always fails when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let lazyTask = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let eff = FIO.FromTask<Fiber<unit, exn>, exn> lazyTask
        
        let actual = (error <| result (runtime.Run eff)).Message
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask with onError always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let lazyTask = fun () ->
            task {
                return res
            }
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>, exn> (lazyTask, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask effect with onError always succeeds with fiber when the task fails`` (runtime: FRuntime, res: int, exnMsg: string) =
        let lazyTask = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>, exn> (lazyTask, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask fiber with onError always fails when the task fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let lazyTask = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let onError (exn: exn) =
            exn.Message
        
        let eff = FIO.FromGenericTask<Fiber<int, string>, string> (lazyTask, onError)
        
        let actual = error <| result (runtime.Run eff)
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let lazyTask = fun () ->
            task {
                return res
            }
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>, exn> lazyTask
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask effect always succeeds with fiber when the task fails`` (runtime: FRuntime, res: int, exnMsg: string) =
        let lazyTask = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>, exn> lazyTask
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask fiber always fails when the task fails`` (runtime: FRuntime, res: int, exnMsg: string) =
        let lazyTask = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>, exn> lazyTask
        
        let actual = (error <| result (runtime.Run eff)).Message
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``Fork always succeeds when the effect succeeds`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Fork()
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, obj>>
        
        actual = expected
    
    [<Property>]
    member _.``Fork always succeeds when the effect fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Fork()
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<obj, int>>
        
        actual = expected
    
    [<Property>]
    member _.``Fork fiber always succeeds when the effect succeeds`` (runtime:FRuntime, res: int) =
        let eff = (FIO.Succeed res).Fork()
        
        let actual = result (result <| runtime.Run eff)
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``Fork fiber always fails when the effect fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Fork()
        
        let actual = error (result <| runtime.Run eff)
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Bind always succeeds when the initial effect succeeds and continuation succeeds`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Bind(FIO.Succeed)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``Bind always fails when the initial effect fails and continuation fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Bind(FIO.Fail)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Bind always fails when the initial effect fails and continuation succeeds`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Bind(FIO.Succeed)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Bind always fails when the initial effect succeeds and continuation fails`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Bind(FIO.Fail)
        
        let actual = error <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``BindError always succeeds when the initial effect succeeds and continuation fails`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).BindError(FIO.Fail)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``BindError always succeeds when the initial effect fails and continuation succeeds`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).BindError(FIO.Succeed)
        
        let actual = result <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``BindError always succeeds with the initial effect when the initial effect succeeds and continuation succeeds`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).BindError(fun r -> FIO.Succeed <| r + 1)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``BindError always fails with the continuation when the initial effect fails and continuation fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).BindError(fun e -> FIO.Fail <| e + 1)
        
        let actual = error <| runtime.Run eff
        let expected = err + 1
        
        actual = expected
    
    [<Property>]
    member _.``Map always succeeds when the effect succeeds and transforms result`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Map(string)
        
        let actual = result <| runtime.Run eff
        let expected = string res
        
        actual = expected
    
    [<Property>]
    member _.``Map always fails when the effect fails and does not transform result`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Map(string) 
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``MapError always succeeds when the effect succeeds and does not transform result`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).MapError(string) 
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``MapError always fails when the effect fails and transforms result`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).MapError(string) 
        
        let actual = error <| runtime.Run eff
        let expected = string err
        
        actual = expected
    
    [<Property>]
    member _.``Then always succeeds with the second effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).Then(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = res2
        
        actual = expected
    
    [<Property>]
    member _.``Then always fails with the initial effect when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).Then(FIO.Succeed res)

        let actual = error <| runtime.Run eff
        let expected = err

        actual = expected
    
    [<Property>]
    member _.``Then always fails with the second effect when the initial effect succeeds and second effect fails`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Succeed res).Then(FIO.Fail err)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Then always fails with the initial effect when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).Then(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err1
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always succeeds with the initial effect when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).ThenError(FIO.Fail err)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always succeeds with the second effect when the initial effect fails and second effect succeeds`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Fail err).ThenError(FIO.Succeed res)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always succeeds with the initial effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).ThenError(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = res1
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always fails with the second effect when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).ThenError(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err2
        
        actual = expected

    [<Property>]
    member _.``Apply always succeeds when the initial effect succeeds and function effects succeeds and transforms result`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Apply(FIO.Succeed <| string)
        
        let actual = result <| runtime.Run eff
        let expected = string res
        
        actual = expected

    [<Property>]
    member _.``Apply always fails when the initial effect fails and function effects succeeds`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Apply(FIO.Succeed <| string)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected

    [<Property>]
    member _.``Apply always fails when the initial effect succeeds and function effects fails`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Succeed res).Apply(FIO.Fail err)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected

    [<Property>]
    member _.``Apply always fails with the function effect when the initial effect fails and function effects fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).Apply(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err2
        
        actual = expected

    [<Property>]
    member _.``ApplyError always fails when the initial effect fails and function effects fails and transforms error`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).ApplyError(FIO.Fail <| string)
        
        let actual = error <| runtime.Run eff
        let expected = string err
        
        actual = expected

    [<Property>]
    member _.``ApplyError always succeeds when the initial effect fails and function effects succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).ApplyError(FIO.Succeed res)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ApplyError always succeeds when the initial effect succeeds and function effects fails`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).ApplyError(FIO.Fail <| string)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected

    [<Property>]
    member _.``ApplyError always succeeds with the function effect when the initial effect succeeds and function effects succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).ApplyError(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = res2
        
        actual = expected
    
    [<Property>]
    member _.``Zip always succeeds when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).Zip(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = (res1, res2)
        
        actual = expected
        
    [<Property>]
    member _.``Zip always fails when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).Zip(FIO.Succeed res)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
        
    [<Property>]
    member _.``Zip always fails when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).Zip(FIO.Fail err)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
        
    [<Property>]
    member _.``Zip always fails when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).Zip(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err1
        
        actual = expected
        
    [<Property>]
    member _.``ZipError always fails when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).ZipError(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = (err1, err2)
        
        actual = expected
        
    [<Property>]
    member _.``ZipError always succeeds when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).ZipError(FIO.Succeed res)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ZipError always succeeds when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).ZipError(FIO.Fail err)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ZipError always succeeds with the initial effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).ZipError(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = res1
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always succeeds when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).Parallel(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = (res1, res2)
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always fails when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).Parallel(FIO.Succeed res)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always fails when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).Parallel(FIO.Fail err)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always fails when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).Parallel(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err1
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always fails when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).ParallelError(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = (err1, err2)
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always succeeds when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).ParallelError(FIO.Succeed res)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always succeeds when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).ParallelError(FIO.Fail err)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always succeeds with the initial effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).ParallelError(FIO.Succeed res2)

        let actual = result <| runtime.Run eff
        let expected = res1

        actual = expected

    [<Property>]
    member _.``Fiber.Id returns unique identifier`` (runtime: FRuntime) =
        let fiber1 = runtime.Run (FIO.Succeed 42)
        let fiber2 = runtime.Run (FIO.Succeed 42)

        fiber1.Id <> System.Guid.Empty && fiber2.Id <> System.Guid.Empty && fiber1.Id <> fiber2.Id
