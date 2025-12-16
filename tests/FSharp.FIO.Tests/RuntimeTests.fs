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
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>> lazyTask
        
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
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>> lazyTask
        
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
        
        let eff = FIO.FromGenericTask<Fiber<int, exn>> lazyTask
        
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

    // ===== Channel Fairness Tests =====

    // [<Fact>]
    // member _.``Channel maintains FIFO with sequential operations`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let chan = Channel<int>()
    //         for i in 0..99 do
    //             do! chan <-- i |> FIO.Ignore
    //         let mutable received = []
    //         for _ in 0..99 do
    //             let! msg = !<-- chan
    //             received <- msg :: received
    //         return List.rev received
    //     }
    //     let actual = result <| runtime.Run eff
    //     let expected = [0..99]
    //     Assert.Equal<int list>(expected, actual)

    // [<Fact>]
    // member _.``Channel with single sender multiple receivers is fair`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let chan = Channel<int>()
    //         let receivedBag = System.Collections.Concurrent.ConcurrentBag<int>()

    //         let! receivers =
    //             [0..9]
    //             |> List.map (fun _ ->
    //                 fio {
    //                     for _ in 0..9 do
    //                         let! msg = !<-- chan
    //                         receivedBag.Add(msg)
    //                 } |> (!<~))
    //             |> (!+)

    //         for i in 0..99 do
    //             do! chan <-- i |> FIO.Ignore

    //         for fiber in receivers do
    //             do! !!<~~ fiber

    //         return receivedBag.ToArray() |> Array.sort
    //     }
    //     let actual = result <| runtime.Run eff
    //     let expected = [|0..99|]
    //     Assert.Equal<int[]>(expected, actual)

    // [<Fact>]
    // member _.``Channel with multiple senders maintains order per sender`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let chan = Channel<int * int>()
    //         let receivedBag = System.Collections.Concurrent.ConcurrentBag<int * int>()

    //         let! senders =
    //             [0..9]
    //             |> List.map (fun senderId ->
    //                 fio {
    //                     for i in 0..9 do
    //                         do! chan <-- (senderId, i) |> FIO.Ignore
    //                 } |> (!<~))
    //             |> (!+)

    //         let! receiver = fio {
    //             for _ in 0..99 do
    //                 let! msg = !<-- chan
    //                 receivedBag.Add(msg)
    //         } |> (!<~)

    //         for fiber in senders do
    //             do! !!<~~ fiber
    //         do! !!<~~ receiver

    //         let received = receivedBag.ToArray()
    //         let groupedBySender = received |> Array.groupBy fst

    //         return groupedBySender |> Array.forall (fun (senderId, msgs) ->
    //             let indices = msgs |> Array.map snd |> Array.sort
    //             indices = [|0..9|])
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // [<Fact>]
    // member _.``Channel Count stays consistent under concurrent access`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let chan = Channel<int>()

    //         let! senders =
    //             [0..19]
    //             |> List.map (fun i ->
    //                 fio {
    //                     for j in 0..4 do
    //                         do! chan <-- (i * 10 + j) |> FIO.Ignore
    //                 } |> (!<~))
    //             |> (!+)

    //         let! receivers =
    //             [0..19]
    //             |> List.map (fun _ ->
    //                 fio {
    //                     for _ in 0..4 do
    //                         let! _ = !<-- chan
    //                         ()
    //                 } |> (!<~))
    //             |> (!+)

    //         for fiber in senders do
    //             do! !!<~~ fiber
    //         for fiber in receivers do
    //             do! !!<~~ fiber

    //         return chan.Count
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal(0L, actual)

    // [<Fact>]
    // member _.``Empty channel blocks receiver until message arrives`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let chan = Channel<int>()
    //         let started = System.Collections.Concurrent.ConcurrentBag<bool>()

    //         let! receiver = fio {
    //             started.Add(true)
    //             let! msg = !<-- chan
    //             return msg
    //         } |> (!<~)

    //         do! !<< (fun () -> System.Threading.Thread.Sleep(100))
    //         do! chan <-- 42 |> FIO.Ignore
    //         let! receivedMsg = !<~~ receiver

    //         return receivedMsg = 42 && started.Count = 1
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // // ===== Fiber Completion & Race Tests =====

    // [<Fact>]
    // member _.``Awaiting already completed fiber succeeds immediately`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! fiber = !<~ (FIO.Succeed 42)
    //         do! !<< (fun () -> System.Threading.Thread.Sleep(100))
    //         let! res = !<~~ fiber
    //         return res
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal(42, actual)

    // [<Fact>]
    // member _.``Multiple fibers awaiting same fiber all succeed`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! targetFiber = !<~ (FIO.Succeed 42)

    //         let! awaiters =
    //             [0..49]
    //             |> List.map (fun _ ->
    //                 fio {
    //                     let! res = !<~~ targetFiber
    //                     return res
    //                 } |> (!<~))
    //             |> (!+)

    //         let mutable allResults = []
    //         for fiber in awaiters do
    //             let! res = !<~~ fiber
    //             allResults <- res :: allResults

    //         return allResults |> List.forall (fun x -> x = 42) && List.length allResults = 50
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // [<Fact>]
    // member _.``Fiber completion race with blocking registration handled`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! fastFiber = !<~ (FIO.Succeed 42)

    //         let! awaiters =
    //             [0..9]
    //             |> List.map (fun _ ->
    //                 fio {
    //                     let! res = !<~~ fastFiber
    //                     return res
    //                 } |> (!<~))
    //             |> (!+)

    //         let mutable results = []
    //         for fiber in awaiters do
    //             let! res = !<~~ fiber
    //             results <- res :: results

    //         return results |> List.forall (fun x -> x = 42)
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // [<Fact>]
    // member _.``Chain of fiber awaits completes in correct order`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! fiberD = !<~ (FIO.Succeed "D")
    //         let! fiberC = !<~ (fio {
    //             let! res = !<~~ fiberD
    //             return "C-" + res
    //         })
    //         let! fiberB = !<~ (fio {
    //             let! res = !<~~ fiberC
    //             return "B-" + res
    //         })
    //         let! fiberA = !<~ (fio {
    //             let! res = !<~~ fiberB
    //             return "A-" + res
    //         })

    //         let! finalResult = !<~~ fiberA
    //         return finalResult
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal("A-B-C-D", actual)

    // [<Fact>]
    // member _.``Fiber error propagates to all awaiters`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! failingFiber = !<~ (FIO.Fail "error")

    //         let! awaiters =
    //             [0..4]
    //             |> List.map (fun _ ->
    //                 fio {
    //                     let! res = !<~~ failingFiber
    //                     return res
    //                 } |> (!<~))
    //             |> (!+)

    //         let mutable errors = []
    //         for fiber in awaiters do
    //             try
    //                 let fiberResult = fiber.Task().GetAwaiter().GetResult()
    //                 match fiberResult with
    //                 | Ok _ -> ()
    //                 | Error err -> errors <- err :: errors
    //             with _ -> ()

    //         return errors |> List.forall (fun e -> e = "error") && List.length errors = 5
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // [<Fact>]
    // member _.``Forked fiber runs independently of parent`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let childCompleted = System.Collections.Concurrent.ConcurrentBag<bool>()

    //         let! childFiber = !<~ (fio {
    //             do! !<< (fun () -> System.Threading.Thread.Sleep(200))
    //             childCompleted.Add(true)
    //             return 42
    //         })

    //         return childCompleted.Count = 0
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // // ===== Continuation Stack Depth Tests =====

    // [<Fact>]
    // member _.``Deep bind chains (10000) do not overflow`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let rec createDeepChain depth acc =
    //         if depth <= 0 then
    //             FIO.Succeed acc
    //         else
    //             FIO.Succeed acc |> _.Bind <| (fun x -> createDeepChain (depth - 1) (x + 1))

    //     let eff = createDeepChain 10000 0
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal(10000, actual)

    // [<Fact>]
    // member _.``Deep ChainSuccess nesting succeeds`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let rec createDeepThen depth acc =
    //         if depth <= 0 then
    //             FIO.Succeed acc
    //         else
    //             (FIO.Succeed 1).Then(createDeepThen (depth - 1) (acc + 1))

    //     let eff = createDeepThen 5000 0
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal(5000, actual)

    // [<Fact>]
    // member _.``Continuation stack pooling with repeated patterns`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let pattern = fio {
    //         let! a = FIO.Succeed 1
    //         let! b = FIO.Succeed 2
    //         let! c = FIO.Succeed 3
    //         return a + b + c
    //     }

    //     let eff = fio {
    //         let mutable results = []
    //         for _ in 0..999 do
    //             let! res = pattern
    //             results <- res :: results
    //         return List.length results = 1000 && List.forall (fun x -> x = 6) results
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // [<Fact>]
    // member _.``Wide effect trees complete correctly`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let rec createBinaryTree depth =
    //         if depth <= 0 then
    //             FIO.Succeed 1
    //         else
    //             let left = createBinaryTree (depth - 1)
    //             let right = createBinaryTree (depth - 1)
    //             fio {
    //                 let! l = left
    //                 let! r = right
    //                 return l + r
    //             }

    //     let eff = createBinaryTree 10
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal(1024, actual)

    // // ===== Error Handling Edge Cases =====

    // [<Fact>]
    // member _.``Error in forked fiber does not crash parent`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! childFiber = !<~ (FIO.Fail "child error")
    //         return 42
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.Equal(42, actual)

    // [<Fact>]
    // member _.``Parallel effects handle partial failures`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let eff1 = FIO.Succeed 1
    //         let eff2 = FIO.Succeed 2
    //         let eff3 = FIO.Fail "error3"
    //         let eff4 = FIO.Succeed 4
    //         let eff5 = FIO.Fail "error5"

    //         let! _ = !~~& (eff1, eff2)
    //         ()
    //     }

    //     let eff2 = fio {
    //         let eff1 = FIO.Succeed 1
    //         let eff2 = FIO.Fail "error2"

    //         let! _ = !~~& (eff1, eff2)
    //         return 1
    //     }

    //     try
    //         let _ = error <| runtime.Run eff2
    //         Assert.True(true)
    //     with _ ->
    //         Assert.True(false)

    // [<Fact>]
    // member _.``BindError chains execute in correct order`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff =
    //         (FIO.Fail 10)
    //             .BindError(fun x -> FIO.Fail (x + 10))
    //             .BindError(fun x -> FIO.Fail (x * 2))

    //     let actual = error <| runtime.Run eff
    //     Assert.Equal(40, actual)

    // [<Fact>]
    // member _.``Exception in Bind continuation is caught`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let! _ = FIO.Succeed 42 |> _.Bind <| (fun _ -> invalidOp "test error"; FIO.Succeed 1)
    //         return 1
    //     }

    //     try
    //         let _ = error <| runtime.Run eff
    //         Assert.True(true)
    //     with _ ->
    //         Assert.True(true)

    // [<Fact>]
    // member _.``Exception in Map function is caught`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = (FIO.Succeed 42).Map(fun _ -> invalidOp "test error"; 1)

    //     try
    //         let _ = error <| runtime.Run eff
    //         Assert.True(true)
    //     with _ ->
    //         Assert.True(true)

    // // ===== Runtime-Specific Behavior Tests =====

    // [<Fact>]
    // member _.``All three runtimes produce same results`` () =
    //     let eff = fio {
    //         let chan = Channel<int>()
    //         let! fiber1 = !<~ (fio {
    //             for i in 0..9 do
    //                 do! chan <-- i |> FIO.Ignore
    //         })
    //         let! fiber2 = !<~ (fio {
    //             let mutable sum = 0
    //             for _ in 0..9 do
    //                 let! msg = !<-- chan
    //                 sum <- sum + msg
    //             return sum
    //         })
    //         do! !!<~~ fiber1
    //         let! result = !<~~ fiber2
    //         return result
    //     }

    //     let directRuntime = new Direct.Runtime()
    //     let cooperativeRuntime = new Cooperative.Runtime()
    //     let concurrentRuntime = new Concurrent.Runtime()

    //     let res1 = result <| directRuntime.Run eff
    //     let res2 = result <| cooperativeRuntime.Run eff
    //     let res3 = result <| concurrentRuntime.Run eff

    //     Assert.Equal(res1, res2)
    //     Assert.Equal(res2, res3)

    // [<Fact>]
    // member _.``ConcurrentRuntime handles high contention`` () =
    //     let runtime = new Concurrent.Runtime()
    //     let eff = fio {
    //         let chan = Channel<int>()

    //         let! receivers =
    //             [0..99]
    //             |> List.map (fun _ ->
    //                 fio {
    //                     let! msg = !<-- chan
    //                     return msg
    //                 } |> (!<~))
    //             |> (!+)

    //         do! !<< (fun () -> System.Threading.Thread.Sleep(100))

    //         for i in 0..99 do
    //             do! chan <-- i |> FIO.Ignore

    //         let mutable results = []
    //         for fiber in receivers do
    //             let! res = !<~~ fiber
    //             results <- res :: results

    //         return List.length results = 100
    //     }
    //     let actual = result <| runtime.Run eff
    //     Assert.True(actual)

    // [<Fact>]
    // member _.``DirectRuntime integrates with Task correctly`` () =
    //     let runtime = new Direct.Runtime()
    //     let eff = FIO.Succeed 42
    //     let fiber = runtime.Run eff
    //     let task = fiber.Task()

    //     Assert.True(task.IsCompleted || task.Wait(5000))
    //     match task.Result with
    //     | Ok res -> Assert.Equal(42, res)
    //     | Error _ -> Assert.True(false)
