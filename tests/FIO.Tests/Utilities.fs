module FIO.Tests.Utilities

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative

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
