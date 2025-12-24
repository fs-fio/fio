(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module FSharp.FIO.Tests.RuntimeTests

open FSharp.FIO.DSL
open FSharp.FIO.Runtime
open FSharp.FIO.Runtime.Direct
open FSharp.FIO.Runtime.Concurrent
open FSharp.FIO.Runtime.Cooperative

open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp

open System.Threading.Tasks
open System.Collections.Concurrent

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
            Gen.constant (new DirectRuntime())
            Gen.constant (new CooperativeRuntime())
            Gen.constant (new ConcurrentRuntime())
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
            
        let eff = FIO.Attempt<int, exn> (func, onError)
        
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
            
        let eff = FIO.Attempt<int, string> (func, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected

    [<Property>]
    member _.``FromFunc succeeds when no exception is thrown`` (runtime: FRuntime, res: int) =
        let func () =
            res
        
        let eff = FIO.Attempt func

        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected

    [<Property>]
    member _.``FromFunc fails when exception is thrown`` (runtime: FRuntime, res: int, exnMsg: string) =
        let func () =
            invalidOp exnMsg
            res

        let eff = FIO.Attempt func
        
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

        let eff = FIO.AwaitTask (t, onError)
        
        let actual = result <| runtime.Run eff
        let expected = ()

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitTask with onError always fails when the task fails and converts error`` (runtime: FRuntime, exnMsg: string) =
        let t = Task.Run(fun () ->
            invalidOp exnMsg)
        
        let onError (exn: exn) =
            exn.Message

        let eff = FIO.AwaitTask (t, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected && t.IsFaulted
    
    [<Property>]
    member _.``AwaitTask always succeeds when the task succeeds`` (runtime: FRuntime) =
        let t =
            Task.Run(fun () -> ())

        let eff = FIO.AwaitTask t
        
        let actual = result <| runtime.Run eff
        let expected = ()

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitTask always fails when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let t = Task.Run(fun () ->
            invalidOp exnMsg)
        
        let eff = FIO.AwaitTask t
        
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

        let eff = FIO.AwaitTask<int, exn> (t, onError)
        
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

        let eff = FIO.AwaitTask<int, string> (t, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected && t.IsFaulted
    
    [<Property>]
    member _.``AwaitGenericTask always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let t = task {
            return res
        }
      
        let eff = FIO.AwaitTask<int> t
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected && t.IsCompletedSuccessfully
    
    [<Property>]
    member _.``AwaitGenericTask always fails when the task fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let t = task {
            invalidOp exnMsg
            return res
        }

        let eff = FIO.AwaitTask t
        
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

        let eff = FIO.AwaitAsync (a, onError)
        
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

        let eff = FIO.AwaitAsync (a, onError)
        
        let actual = error <| runtime.Run eff
        let expected = exnMsg

        actual = expected
    
    [<Property>]
    member _.``AwaitAsync always succeeds when the computation succeeds`` (runtime: FRuntime, res: int) =
        let a = async {
            return res
        }
        
        let eff = FIO.AwaitAsync a
        
        let actual = result <| runtime.Run eff
        let expected = res

        actual = expected
    
    [<Property>]
    member _.``AwaitAsync always fails when the computation fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let a = async {
            invalidOp exnMsg
            return res
        }
        
        let eff = FIO.AwaitAsync a
        
        let actual = (error <| runtime.Run eff).Message
        let expected = exnMsg

        actual = expected
    
    [<Property>]
    member _.``FromTask with onError always succeeds when the task succeeds`` (runtime: FRuntime) =
        let taskFactory = fun () ->
            Task.Run(fun () -> ())
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromTask (taskFactory, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask effect with onError always succeeds with fiber when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let taskFactory = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromTask (taskFactory, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask fiber with onError always fails when the task fails and converts error`` (runtime: FRuntime, exnMsg: string) =
        let taskFactory = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let onError (exn: exn) =
            exn.Message
        
        let eff = FIO.FromTask (taskFactory, onError)
        
        let actual = error <| result (runtime.Run eff)
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``FromTask always succeeds when the task succeeds`` (runtime: FRuntime) =
        let taskFactory = fun () ->
            Task.Run(fun () -> ())
        
        let eff = FIO.FromTask taskFactory
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask effect always succeeds with fiber when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let taskFactory = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let eff = FIO.FromTask taskFactory
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<unit, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromTask fiber always fails when the task fails`` (runtime: FRuntime, exnMsg: string) =
        let taskFactory = fun () ->
            Task.Run(fun () ->
            invalidOp exnMsg)
        
        let eff = FIO.FromTask taskFactory
        
        let actual = (error <| result (runtime.Run eff)).Message
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask with onError always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let taskFactory = fun () ->
            task {
                return res
            }
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromTask (taskFactory, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask effect with onError always succeeds with fiber when the task fails`` (runtime: FRuntime, res: int, exnMsg: string) =
        let taskFactory = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let onError (exn: exn) =
            exn
        
        let eff = FIO.FromTask (taskFactory, onError)
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask fiber with onError always fails when the task fails and converts error`` (runtime: FRuntime, res: int, exnMsg: string) =
        let taskFactory = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let onError (exn: exn) =
            exn.Message
        
        let eff = FIO.FromTask (taskFactory, onError)
        
        let actual = error <| result (runtime.Run eff)
        let expected = exnMsg
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask always succeeds when the task succeeds`` (runtime: FRuntime, res: int) =
        let taskFactory = fun () ->
            task {
                return res
            }
        
        let eff = FIO.FromTask taskFactory
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask effect always succeeds with fiber when the task fails`` (runtime: FRuntime, res: int, exnMsg: string) =
        let taskFactory = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let eff = FIO.FromTask taskFactory
        
        let actual = (result <| runtime.Run eff).GetType()
        let expected = typeof<Fiber<int, exn>>
        
        actual = expected
    
    [<Property>]
    member _.``FromGenericTask fiber always fails when the task fails`` (runtime: FRuntime, res: int, exnMsg: string) =
        let taskFactory = fun () ->
            task {
                invalidOp exnMsg
                return res
            }
        
        let eff = FIO.FromTask taskFactory
        
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
        let eff = (FIO.Succeed res).FlatMap FIO.Succeed
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``Bind always fails when the initial effect fails and continuation fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).FlatMap FIO.Fail
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Bind always fails when the initial effect fails and continuation succeeds`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).FlatMap FIO.Succeed
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Bind always fails when the initial effect succeeds and continuation fails`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).FlatMap FIO.Fail
        
        let actual = error <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``BindError always succeeds when the initial effect succeeds and continuation fails`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).CatchAll FIO.Fail
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``BindError always succeeds when the initial effect fails and continuation succeeds`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).CatchAll FIO.Succeed
        
        let actual = result <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``BindError always succeeds with the initial effect when the initial effect succeeds and continuation succeeds`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).CatchAll(fun r -> FIO.Succeed <| r + 1)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``BindError always fails with the continuation when the initial effect fails and continuation fails`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).CatchAll(fun e -> FIO.Fail <| e + 1)
        
        let actual = error <| runtime.Run eff
        let expected = err + 1
        
        actual = expected
    
    [<Property>]
    member _.``Map always succeeds when the effect succeeds and transforms result`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).Map string
        
        let actual = result <| runtime.Run eff
        let expected = string res
        
        actual = expected
    
    [<Property>]
    member _.``Map always fails when the effect fails and does not transform result`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).Map string 
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``MapError always succeeds when the effect succeeds and does not transform result`` (runtime: FRuntime, res: int) =
        let eff = (FIO.Succeed res).MapError string 
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``MapError always fails when the effect fails and transforms result`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).MapError string 
        
        let actual = error <| runtime.Run eff
        let expected = string err
        
        actual = expected
    
    [<Property>]
    member _.``Then always succeeds with the second effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).ZipRight(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = res2
        
        actual = expected
    
    [<Property>]
    member _.``Then always fails with the initial effect when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).ZipRight(FIO.Succeed res)

        let actual = error <| runtime.Run eff
        let expected = err

        actual = expected
    
    [<Property>]
    member _.``Then always fails with the second effect when the initial effect succeeds and second effect fails`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Succeed res).ZipRight(FIO.Fail err)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
    
    [<Property>]
    member _.``Then always fails with the initial effect when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).ZipRight(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err1
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always succeeds with the initial effect when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).OrElse(FIO.Fail err)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always succeeds with the second effect when the initial effect fails and second effect succeeds`` (runtime: FRuntime, res: int, err: int) =
        let eff = (FIO.Fail err).OrElse(FIO.Succeed res)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always succeeds with the initial effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).OrElse(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = res1
        
        actual = expected
    
    [<Property>]
    member _.``ThenError always fails with the second effect when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).OrElse(FIO.Fail err2)
        
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
        let eff = (FIO.Succeed res1).ZipPar(FIO.Succeed res2)
        
        let actual = result <| runtime.Run eff
        let expected = (res1, res2)
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always fails when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).ZipPar(FIO.Succeed res)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always fails when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).ZipPar(FIO.Fail err)
        
        let actual = error <| runtime.Run eff
        let expected = err
        
        actual = expected
        
    [<Property>]
    member _.``Parallel always fails when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).ZipPar(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = err1
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always fails when the initial effect fails and second effect fails`` (runtime: FRuntime, err1: int, err2: int) =
        let eff = (FIO.Fail err1).ZipParError(FIO.Fail err2)
        
        let actual = error <| runtime.Run eff
        let expected = (err1, err2)
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always succeeds when the initial effect fails and second effect succeeds`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Fail err).ZipParError(FIO.Succeed res)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always succeeds when the initial effect succeeds and second effect fails`` (runtime: FRuntime, err: int, res: int) =
        let eff = (FIO.Succeed res).ZipParError(FIO.Fail err)
        
        let actual = result <| runtime.Run eff
        let expected = res
        
        actual = expected
        
    [<Property>]
    member _.``ParallelError always succeeds with the initial effect when the initial effect succeeds and second effect succeeds`` (runtime: FRuntime, res1: int, res2: int) =
        let eff = (FIO.Succeed res1).ZipParError(FIO.Succeed res2)

        let actual = result <| runtime.Run eff
        let expected = res1

        actual = expected

    [<Property>]
    member _.``Fiber.Id returns unique identifier`` (runtime: FRuntime) =
        let fiber1 = runtime.Run (FIO.Succeed 42)
        let fiber2 = runtime.Run (FIO.Succeed 42)

        fiber1.Id <> System.Guid.Empty && fiber2.Id <> System.Guid.Empty && fiber1.Id <> fiber2.Id

    [<Property>]
    member _.``Channel maintains FIFO with sequential operations`` (runtime: FRuntime) =
        let eff = fio {
            let chan = Channel<int>()
            for i in 0..99 do
                do! chan.Send(i).Unit()
            let mutable received = []
            for _ in 0..99 do
                let! msg = chan.Receive ()
                received <- msg :: received
            return List.rev received
        }
        let actual = result <| runtime.Run eff
        let expected = [0..99]
        actual = expected

    [<Property>]
    member _.``Channel with single sender multiple receivers is fair`` (runtime: FRuntime) =
        let eff = fio {
            let chan = Channel<int>()
            let receivedBag = ConcurrentBag<int>()

            let mutable receivers = []
            for _ in 0..9 do
                let eff = fio {
                    for _ in 0..9 do
                        let! msg = chan.Receive ()
                        do! FIO.Attempt <| fun () -> receivedBag.Add msg
                    return ()
                }
                let! fiber = eff.Fork ()
                receivers <- fiber :: receivers

            for i in 0..99 do
                do! chan.Send(i).Unit()

            for fiber in receivers do
                do! fiber.Join ()

            return receivedBag.ToArray () |> Array.sort
        }
        let actual = result <| runtime.Run eff
        let expected = [|0..99|]
        actual = expected

    [<Property>]
    member _.``Channel with multiple senders maintains order per sender`` (runtime: FRuntime) =
        let eff = fio {
            let chan = Channel<int * int>()
            let receivedBag = ConcurrentBag<int * int>()

            let mutable senders = []
            for senderId in 0..9 do
                let eff = fio {
                    for i in 0..9 do
                        do! chan.Send(senderId, i).Unit()
                    return ()
                }
                let! fiber = eff.Fork ()
                senders <- fiber :: senders

            let eff = fio {
                    for _ in 0..99 do
                        let! msg = chan.Receive ()
                        do! FIO.Attempt <| fun () -> receivedBag.Add msg
                    return ()
                }
            let! receiver = eff.Fork ()

            for fiber in senders do
                do! fiber.Join ()
            do! receiver.Join ()

            let received = receivedBag.ToArray()
            let groupedBySender = received |> Array.groupBy fst

            return groupedBySender |> Array.forall (fun (_, msgs) ->
                let indices = msgs |> Array.map snd |> Array.sort
                indices = [|0..9|])
        }
        let actual = result <| runtime.Run eff
        actual

    [<Property>]
    member _.``Channel Count stays consistent under concurrent access`` (runtime: FRuntime) =
        let eff = fio {
            let chan = Channel<int>()

            let mutable senders = []
            for i in 0..19 do
                let eff = fio {
                    for j in 0..4 do
                        do! chan.Send(i * 10 + j).Unit()
                    return ()
                }
                let! fiber = eff.Fork ()
                senders <- fiber :: senders

            let mutable receivers = []
            for _ in 0..19 do
                let eff = fio {
                    for _ in 0..4 do
                        let! _ = chan.Receive ()
                        ()
                    return ()
                }
                let! fiber = eff.Fork ()
                receivers <- fiber :: receivers

            for fiber in senders do
                do! fiber.Join ()
            for fiber in receivers do
                do! fiber.Join ()

            return chan.Count
        }
        let actual = result <| runtime.Run eff
        actual = 0L

    [<Property>]
    member _.``Empty channel blocks receiver until message arrives`` (runtime: FRuntime) =
        let eff = fio {
            let chan = Channel<int>()
            let started = ConcurrentBag<bool>()

            let receiverEff = fio {
                started.Add true
                let! msg = chan.Receive ()
                return msg
            }
            let! receiver = receiverEff.Fork ()

            do! chan.Send(42).Unit()
            let! receivedMsg = receiver.Join ()

            return receivedMsg = 42 && started.Count = 1
        }
        let actual = result <| runtime.Run eff
        actual

    [<Property>]
    member _.``Multiple fibers awaiting same fiber all succeed`` (runtime: FRuntime) =
        let eff = fio {
            let! targetFiber = (FIO.Succeed 42).Fork ()

            let mutable awaiters = []
            for _ in 0..49 do
                let eff = fio {
                    let! res = targetFiber.Join ()
                    return res
                }
                let! fiber = eff.Fork ()
                awaiters <- fiber :: awaiters

            let mutable allResults = []
            for fiber in awaiters do
                let! res = fiber.Join ()
                allResults <- res :: allResults

            return allResults |> List.forall (fun x -> x = 42) && List.length allResults = 50
        }
        let actual = result <| runtime.Run eff
        actual

    [<Property>]
    member _.``Fiber completion race with blocking registration handled`` (runtime: FRuntime) =
        let eff = fio {
            let! fastFiber = (FIO.Succeed 42).Fork ()

            let mutable awaiters = []
            for _ in 0..9 do
                let eff = fio {
                    let! res = fastFiber.Join ()
                    return res
                }
                let! fiber = eff.Fork ()
                awaiters <- fiber :: awaiters

            let mutable results = []
            for fiber in awaiters do
                let! res = fiber.Join ()
                results <- res :: results

            return results |> List.forall (fun x -> x = 42)
        }
        let actual = result <| runtime.Run eff
        actual

    [<Property>]
    member _.``Chain of fiber awaits completes in correct order`` (runtime: FRuntime) =
        let eff = fio {
            let! fiberD = (FIO.Succeed "D").Fork ()
            let effC = fio {
                let! res = fiberD.Join ()
                return "C-" + res
            }
            let! fiberC = effC.Fork ()
            let effB = fio {
                let! res = fiberC.Join ()
                return "B-" + res
            }
            let! fiberB = effB.Fork ()
            let effA = fio {
                let! res = fiberB.Join ()
                return "A-" + res
            }
            let! fiberA = effA.Fork ()

            let! finalResult = fiberA.Join ()
            return finalResult
        }
        let actual = result <| runtime.Run eff
        actual = "A-B-C-D"

    [<Property>]
    member _.``Moderate bind chains (100) do not overflow`` (runtime: FRuntime) =
        let rec createDeepChain depth acc =
            if depth <= 0 then
                FIO.Succeed acc
            else
                (FIO.Succeed acc).FlatMap(fun x -> createDeepChain (depth - 1) (x + 1))

        let eff = createDeepChain 100 0
        let actual = result <| runtime.Run eff
        actual = 100

    [<Property>]
    member _.``Moderate ChainSuccess nesting succeeds`` (runtime: FRuntime) =
        let rec createDeepThen depth acc =
            if depth <= 0 then
                FIO.Succeed acc
            else
                (FIO.Succeed 1).ZipRight(createDeepThen (depth - 1) (acc + 1))

        let eff = createDeepThen 100 0
        let actual = result <| runtime.Run eff
        actual = 100

    [<Property>]
    member _.``Continuation stack pooling with repeated patterns`` (runtime: FRuntime) =
        let pattern = fio {
            let! a = FIO.Succeed 1
            let! b = FIO.Succeed 2
            let! c = FIO.Succeed 3
            return a + b + c
        }

        let eff = fio {
            let mutable results = []
            for _ in 0..999 do
                let! res = pattern
                results <- res :: results
            return List.length results = 1000 && List.forall (fun x -> x = 6) results
        }
        let actual = result <| runtime.Run eff
        actual

    [<Property>]
    member _.``Wide effect trees complete correctly`` (runtime: FRuntime) =
        let rec createBinaryTree depth =
            if depth <= 0 then
                FIO.Succeed 1
            else
                let left = createBinaryTree (depth - 1)
                let right = createBinaryTree (depth - 1)
                fio {
                    let! l = left
                    let! r = right
                    return l + r
                }

        let eff = createBinaryTree 10
        let actual = result <| runtime.Run eff
        actual = 1024

    [<Property>]
    member _.``Error in forked fiber does not crash parent`` (runtime: FRuntime) =
        let eff = fio {
            let! childFiber = (FIO.Fail "child error").Fork ()
            return 42
        }
        let actual = result <| runtime.Run eff
        actual = 42

    [<Property>]
    member _.``Parallel effects handle partial failures`` (runtime: FRuntime) =
        let eff = fio {
            let eff1 = FIO.Succeed 1
            let eff2 = FIO.Succeed 2
            let _ = FIO.Fail "error3"
            let _ = FIO.Succeed 4
            let _ = FIO.Fail "error5"

            let! _ = eff1.ZipPar eff2
            return ()
        }

        let eff2 = fio {
            let eff1 = FIO.Succeed 1
            let eff2 = FIO.Fail "error2"

            let! _ = eff1.ZipPar eff2
            return 1
        }

        try
            let _ = error <| runtime.Run eff2
            true
        with _ ->
            false

    [<Property>]
    member _.``BindError chains execute in correct order`` (runtime: FRuntime) =
        let eff =
            (FIO.Fail 10)
                .CatchAll(fun x -> FIO.Fail (x + 10))
                .CatchAll(fun x -> FIO.Fail (x * 2))

        let actual = error <| runtime.Run eff
        actual = 40

    [<Property>]
    member _.``Runtime handles high contention`` (runtime: FRuntime) =
        let eff = fio {
            let chan = Channel<int>()

            let mutable receivers = []
            for _ in 0..99 do
                let eff = fio {
                    let! msg = chan.Receive ()
                    return msg
                }
                let! fiber = eff.Fork ()
                receivers <- fiber :: receivers

            for i in 0..99 do
                do! chan.Send(i).Unit()

            let mutable results = []
            for fiber in receivers do
                let! res = fiber.Join ()
                results <- res :: results

            return List.length results = 100
        }
        let actual = result <| runtime.Run eff
        actual

    [<Property>]
    member _.``Runtime integrates with Task correctly`` (runtime: FRuntime) =
        let eff = FIO.Succeed 42
        let fiber = runtime.Run eff
        let task = fiber.Task()

        (task.IsCompleted || task.Wait 5000) &&
        match task.Result with
        | Ok res -> res = 42
        | Error _ -> false

    [<Property>]
    member _.``Referential transparency - Same effect produces same result when run multiple times`` (runtime: FRuntime, value: int) =
        let eff = FIO.Succeed value
        let result1 = result <| runtime.Run eff
        let result2 = result <| runtime.Run eff
        let result3 = result <| runtime.Run eff
        result1 = result2 && result2 = result3

    [<Property>]
    member _.``Sequential composition equivalence - Bind then map equals map then bind`` (runtime: FRuntime, value: int) =
        let f x = x + 10
        let g x = FIO.Succeed (x * 2)

        let lhs = (FIO.Succeed value).FlatMap(g).Map f
        let rhs = (FIO.Succeed value).Map(fun x -> x * 2).FlatMap(fun x -> FIO.Succeed (f x))

        let lhsResult = result <| runtime.Run lhs
        let rhsResult = result <| runtime.Run rhs
        lhsResult = rhsResult

    [<Property>]
    member _.``Error recovery with ThenError allows continuation after failure`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).OrElse(FIO.Succeed 42)
        let actual = result <| runtime.Run eff
        actual = 42

    [<Property>]
    member _.``Channel message delivery - Message sent is exactly the message received`` (runtime: FRuntime, msg: int) =
        let eff = fio {
            let chan = Channel<int>()
            do! (chan.Send msg).FlatMap(fun _ -> FIO.Succeed ())
            let! received = chan.Receive ()
            return received
        }
        let actual = result <| runtime.Run eff
        actual = msg

    [<Property>]
    member _.``Parallel effects both execute and return results`` (runtime: FRuntime, val1: int, val2: int) =
        let eff1 = FIO.Succeed val1
        let eff2 = FIO.Succeed val2
        let parallelEff = eff1.ZipPar eff2
        let r1, r2 = result <| runtime.Run parallelEff
        r1 = val1 && r2 = val2

    [<Property>]
    member _.``Nested fio blocks compose correctly`` (runtime: FRuntime, value: int) =
        let eff = fio {
            let! x = fio {
                let! y = FIO.Succeed value
                return y + 10
            }
            return x * 2
        }
        let actual = result <| runtime.Run eff
        actual = (value + 10) * 2

    [<Property>]
    member _.``Forked fibers execute independently`` (runtime: FRuntime, val1: int, val2: int) =
        let eff = fio {
            let! fiber1 = (FIO.Succeed val1).Fork ()
            let! fiber2 = (FIO.Succeed val2).Fork ()
            let! r1 = fiber1.Join ()
            let! r2 = fiber2.Join ()
            return r1, r2
        }
        let actual1, actual2 = result <| runtime.Run eff
        actual1 = val1 && actual2 = val2

    [<Property>]
    member _.``Map preserves structure - mapping identity returns same effect`` (runtime: FRuntime, value: int) =
        let eff = FIO.Succeed value
        let mapped = eff.Map id
        let effResult = result <| runtime.Run eff
        let mappedResult = result <| runtime.Run mapped
        effResult = mappedResult

    [<Property>]
    member _.``Zip combines independent effects correctly`` (runtime: FRuntime, val1: int, val2: int) =
        let eff1 = FIO.Succeed val1
        let eff2 = FIO.Succeed val2
        let zipped = eff1.Zip eff2
        let r1, r2 = result <| runtime.Run zipped
        r1 = val1 && r2 = val2

    [<Property>]
    member _.``Error propagation - Error in bind continuation propagates`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Succeed 42).FlatMap(fun _ -> FIO.Fail err)
        let actual = error <| runtime.Run eff
        actual = err

    [<Property>]
    member _.``From Result interop - Ok values become successes`` (runtime: FRuntime, value: int) =
        let res = Ok value
        let eff = FIO.FromResult res
        let actual = result <| runtime.Run eff
        actual = value

    [<Property>]
    member _.``From Result interop - Error values become failures`` (runtime: FRuntime, err: int) =
        let res = Error err
        let eff = FIO.FromResult res
        let actual = error <| runtime.Run eff
        actual = err

    [<Property>]
    member _.``FromOption interop - Some values become successes`` (runtime: FRuntime, value: int) =
        let opt = Some value
        let eff = FIO.FromOption(opt, fun () -> "default")
        let actual = result <| runtime.Run eff
        actual = value

    [<Property>]
    member _.``FromOption interop - None values become failures`` (runtime: FRuntime) =
        let opt : int option = None
        let defaultErr = "no value"
        let eff = FIO.FromOption(opt, fun () -> defaultErr)
        let actual = error <| runtime.Run eff
        actual = defaultErr

    [<Property>]
    member _.``Sequential execution order is preserved`` (runtime: FRuntime) =
        let mutable executionOrder = []
        let eff = fio {
            do! FIO.Attempt <| fun () -> executionOrder <- 1 :: executionOrder
            do! FIO.Attempt <| fun () -> executionOrder <- 2 :: executionOrder
            do! FIO.Attempt <| fun () -> executionOrder <- 3 :: executionOrder
            return List.rev executionOrder
        }
        let actual = result <| runtime.Run eff
        actual = [1; 2; 3]

    [<Property>]
    member _.``Bind associativity holds for different nesting`` (runtime: FRuntime, value: int) =
        let f x = FIO.Succeed (x + 1)
        let g x = FIO.Succeed (x * 2)
        let h x = FIO.Succeed (x - 5)

        let lhs = (FIO.Succeed value).FlatMap(f).FlatMap(g).FlatMap h
        let rhs = (FIO.Succeed value).FlatMap(fun x -> f(x).FlatMap(fun y -> g(y).FlatMap(h)))

        let lhsResult = result <| runtime.Run lhs
        let rhsResult = result <| runtime.Run rhs
        lhsResult = rhsResult

    [<Property>]
    member _.``Channel preserves message order with multiple messages`` (runtime: FRuntime) =
        let messages = [1; 2; 3; 4; 5]
        let eff = fio {
            let chan = Channel<int>()
            for msg in messages do
                do! chan.Send(msg).Unit()
            let mutable received = []
            for _ in messages do
                let! msg = chan.Receive ()
                received <- msg :: received
            return List.rev received
        }
        let actual = result <| runtime.Run eff
        actual = messages

    [<Property>]
    member _.``Then discards first result but preserves first error`` (runtime: FRuntime, err: int) =
        let eff = (FIO.Fail err).ZipRight(FIO.Succeed 42)
        let actual = error <| runtime.Run eff
        actual = err

    [<Property>]
    member _.``Nested error handling works correctly`` (runtime: FRuntime, value: int) =
        let eff = fio {
            let! result =
                (FIO.Fail "error1")
                    .OrElse(FIO.Fail "error2")
                    .OrElse(FIO.Succeed value)
            return result
        }
        let actual = result <| runtime.Run eff
        actual = value
