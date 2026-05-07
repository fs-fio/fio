/// <summary>Provides property-based tests for clock effects.</summary>
module FIO.Tests.ClockTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL

open Expecto

open FIO.Runtime

open System
open System.Threading

module Clock = FIO.Clock

[<Tests>]
let clockTests =
    testList
        "Clock"
        [

            testPropertyWithConfig fsCheckConfig "now - returns current local time"
            <| fun (runtime: FIORuntime) ->
                let before = DateTime.Now

                let result = runtime.Run(Clock.now ()).UnsafeSuccess()

                let after = DateTime.Now
                Expect.isGreaterThanOrEqual result before "Should be >= before"
                Expect.isLessThanOrEqual result after "Should be <= after"

            testPropertyWithConfig fsCheckConfig "utcNow - returns current UTC time"
            <| fun (runtime: FIORuntime) ->
                let before = DateTime.UtcNow

                let result = runtime.Run(Clock.utcNow ()).UnsafeSuccess()

                let after = DateTime.UtcNow
                Expect.isGreaterThanOrEqual result before "Should be >= before"
                Expect.isLessThanOrEqual result after "Should be <= after"

            testPropertyWithConfig fsCheckConfig "today - returns date without time component"
            <| fun (runtime: FIORuntime) ->
                let result = runtime.Run(Clock.today ()).UnsafeSuccess()

                Expect.equal result.Hour 0 "Hour should be 0"
                Expect.equal result.Minute 0 "Minute should be 0"
                Expect.equal result.Second 0 "Second should be 0"
                Expect.equal result.Date DateTime.Today "Date should be today"

            testPropertyWithConfig fsCheckConfig "nowOffset - returns current local DateTimeOffset"
            <| fun (runtime: FIORuntime) ->
                let before = DateTimeOffset.Now

                let result = runtime.Run(Clock.nowOffset ()).UnsafeSuccess()

                let after = DateTimeOffset.Now
                Expect.isGreaterThanOrEqual result before "Should be >= before"
                Expect.isLessThanOrEqual result after "Should be <= after"

            testPropertyWithConfig fsCheckConfig "utcNowOffset - returns current UTC DateTimeOffset"
            <| fun (runtime: FIORuntime) ->
                let before = DateTimeOffset.UtcNow

                let result = runtime.Run(Clock.utcNowOffset ()).UnsafeSuccess()

                let after = DateTimeOffset.UtcNow
                Expect.isGreaterThanOrEqual result before "Should be >= before"
                Expect.isLessThanOrEqual result after "Should be <= after"
                Expect.equal result.Offset TimeSpan.Zero "Offset should be zero for UTC"

            testPropertyWithConfig fsCheckConfig "timestamp - returns Unix seconds since epoch"
            <| fun (runtime: FIORuntime) ->
                let before = DateTimeOffset.UtcNow.ToUnixTimeSeconds()

                let result = runtime.Run(Clock.timestamp ()).UnsafeSuccess()

                let after = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
                Expect.isGreaterThanOrEqual result before "Should be >= before"
                Expect.isLessThanOrEqual result after "Should be <= after"

            testPropertyWithConfig fsCheckConfig "timestampMillis - returns Unix milliseconds since epoch"
            <| fun (runtime: FIORuntime) ->
                let before = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

                let result = runtime.Run(Clock.timestampMillis ()).UnsafeSuccess()

                let after = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
                Expect.isGreaterThanOrEqual result before "Should be >= before"
                Expect.isLessThanOrEqual result after "Should be <= after"

            testPropertyWithConfig fsCheckConfig "getTimestamp - returns high-resolution timestamp"
            <| fun (runtime: FIORuntime) ->
                let result = runtime.Run(Clock.getTimestamp ()).UnsafeSuccess()

                Expect.isGreaterThan result 0L "Timestamp should be positive"

            testPropertyWithConfig fsCheckConfig "getElapsedTime - calculates elapsed time between timestamps"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! start = Clock.getTimestamp ()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 10.0, id)
                        let! end_ = Clock.getTimestamp ()
                        let! elapsed = Clock.getElapsedTime (start, end_)
                        return elapsed
                    }

                let elapsed = runtime.Run(eff).UnsafeSuccess()

                Expect.isGreaterThanOrEqual elapsed.TotalMilliseconds 5.0 "Elapsed should be >= 5ms"

            testPropertyWithConfig fsCheckConfig "timed - measures execution time of effect"
            <| fun (runtime: FIORuntime) ->
                let eff = Clock.timed (FIO.sleep (TimeSpan.FromMilliseconds 20.0, id))

                let _, duration = runtime.Run(eff).UnsafeSuccess()

                Expect.isGreaterThanOrEqual duration.TotalMilliseconds 15.0 "Duration should be >= 15ms"

            testPropertyWithConfig fsCheckConfig "timed - preserves result of measured effect"
            <| fun (runtime: FIORuntime, value: int) ->
                let eff = Clock.timed (FIO.succeed value)

                let result, duration = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result value "Should return inner result"
                Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Duration should be >= 0"

            testPropertyWithConfig fsCheckConfig "toTimestamp - converts DateTime to Unix seconds"
            <| fun (runtime: FIORuntime) ->
                let epoch = DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Utc)

                let result = runtime.Run(Clock.toTimestamp epoch).UnsafeSuccess()

                Expect.equal result 946684800L "2000-01-01 should be 946684800 Unix seconds"

            testPropertyWithConfig fsCheckConfig "fromTimestamp - converts Unix seconds to DateTime"
            <| fun (runtime: FIORuntime) ->
                let result = runtime.Run(Clock.fromTimestamp 0L).UnsafeSuccess()

                Expect.equal result (DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)) "0 should be Unix epoch"

            testPropertyWithConfig fsCheckConfig "toTimestamp/fromTimestamp - round-trips correctly"
            <| fun (runtime: FIORuntime) ->
                let eff =
                    fio {
                        let! now = Clock.utcNow ()
                        let! ts = Clock.toTimestamp now
                        let! roundTrip = Clock.fromTimestamp ts
                        return now, roundTrip
                    }

                let original, roundTrip = runtime.Run(eff).UnsafeSuccess()

                let diff = abs (original - roundTrip).TotalSeconds
                Expect.isLessThan diff 1.0 "Round trip should be within 1 second"

            testPropertyWithConfig fsCheckConfig "fromTimestampMillis - converts Unix milliseconds to DateTime"
            <| fun (runtime: FIORuntime) ->
                let result = runtime.Run(Clock.fromTimestampMillis 0L).UnsafeSuccess()

                Expect.equal result (DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)) "0ms should be Unix epoch"

            testPropertyWithConfig
                fsCheckConfig
                "now - lazy (sleep between construction and run, result reflects run time)"
            <| fun (runtime: FIORuntime) ->
                let eff = Clock.now ()
                Thread.Sleep 30
                let before = DateTime.Now

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.isGreaterThanOrEqual
                    result
                    before
                    "Clock.now must evaluate at run time; a value captured at construction would be < before"

            testPropertyWithConfig fsCheckConfig "timed - lazy (reusable; stopwatch restarts on each run)"
            <| fun (runtime: FIORuntime) ->
                let eff = Clock.timed (FIO.succeed ())

                let _, first = runtime.Run(eff).UnsafeSuccess()
                Thread.Sleep 50
                let _, second = runtime.Run(eff).UnsafeSuccess()

                Expect.isLessThan
                    second
                    (TimeSpan.FromMilliseconds 30.0)
                    "Clock.timed must start fresh on each run; a composition-time stopwatch would show >= 50ms here"
        ]
