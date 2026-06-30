module FIO.Tests.Utilities

open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto
open FsCheck

module FsCheckProperties =

    // The worker runtimes spawn a dedicated OS thread per evaluation worker on construction, and
    // Run is single-Run-by-design (it cancels/resets the previous fiber), so a single instance can
    // NOT be shared across the parallel test run. FsCheck builds this Arb once per property and
    // reuses the four instances across that property's (serial) cases, so per-property construction
    // is safe; we only shrink the worker count from the default (~13) to 2 to avoid spawning
    // thousands of threads across ~373 properties.
    let private testConfig = { WorkerConfig.Default with EvaluationWorkers = 2 }

    type Generators =

        static member Runtime() =
            Gen.oneof
                [
                    Gen.constant (new DirectRuntime() :> FIORuntime)
                    Gen.constant (new PollingRuntime(testConfig) :> FIORuntime)
                    Gen.constant (new SignalingRuntime(testConfig) :> FIORuntime)
                    Gen.constant (new WorkStealingRuntime(testConfig) :> FIORuntime)
                ]
            |> Arb.fromGen

    /// Heavy stress/regression tests are opt-in: skipped by default (fast local runs), enabled in CI
    /// via FIO_RUN_STRESS=1.
    let stressEnabled =
        System.Environment.GetEnvironmentVariable "FIO_RUN_STRESS" = "1"

    /// A test case that runs only when FIO_RUN_STRESS=1; otherwise it is reported as pending (skipped)
    /// so the heaviest regression guards don't slow down local runs while remaining visible.
    let stressTestCase name f =
        if stressEnabled then testCase name f else ptestCase name f

    let fsCheckConfig =
        { FsCheckConfig.defaultConfig with
            maxTest = 100
            arbitrary = [ typeof<Generators> ]
        }

    /// Reduced case count for expensive property tests (fiber-forking parallel operators, real-time
    /// sleeps) where 100 random cases add seconds without meaningfully improving coverage.
    let fsCheckConfigFast =
        { fsCheckConfig with maxTest = 25 }
