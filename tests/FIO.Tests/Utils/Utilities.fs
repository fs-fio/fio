/// <summary>Provides test utility types and configuration for FIO core property-based tests.</summary>
module FIO.Tests.Utilities

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Concurrent
open FIO.Runtime.Cooperative

open Expecto
open FsCheck

/// <summary>Provides FsCheck generators and configuration for property-based testing across all runtimes.</summary>
module FsCheckProperties =

    /// <summary>Provides FsCheck arbitrary instances for generating FIO runtime values in property tests.</summary>
    type Generators =
        /// <summary>Creates an FsCheck arbitrary that produces one of the three FIO runtimes (Direct, Cooperative, Concurrent) with equal probability.</summary>
        /// <returns>An arbitrary that generates FIORuntime instances for property-based testing.</returns>
        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new CooperativeRuntime() :> FIORuntime)
                    Gen.constant (new ConcurrentRuntime() :> FIORuntime)
                ]
            |> Arb.fromGen

    /// <summary>Returns the standard FsCheck configuration for FIO tests with 100 max tests and runtime generators registered.</summary>
    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }
