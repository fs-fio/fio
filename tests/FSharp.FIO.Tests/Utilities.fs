module FSharp.FIO.Tests.Utilities

open FSharp.FIO.Runtime
open FSharp.FIO.Runtime.Direct
open FSharp.FIO.Runtime.Concurrent
open FSharp.FIO.Runtime.Cooperative

open Expecto
open FsCheck

module FsCheckProperties =

    type Generators =
        static member Runtime() =
            Gen.oneof [
                Gen.constant (new DirectRuntime() :> FIORuntime)
                Gen.constant (new CooperativeRuntime() :> FIORuntime)
                Gen.constant (new ConcurrentRuntime() :> FIORuntime)
            ] |> Arb.fromGen

    let fsCheckPropertyTestsConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ] }
