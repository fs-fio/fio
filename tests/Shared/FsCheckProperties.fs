module FIO.Tests.FsCheckProperties

open FIO.Tests.Utilities

open Expecto
open FsCheck.FSharp

// One pair for every property: FsCheck asks for the generator once per property and never disposes it.
let private propertyRuntimes = lazy allRuntimes ()

type Generators =

    static member Runtime() =
        Gen.elements propertyRuntimes.Value |> Arb.fromGen

let fsCheckConfig =
    { FsCheckConfig.defaultConfig with
        maxTest = 100
        arbitrary = [ typeof<Generators> ]
    }
