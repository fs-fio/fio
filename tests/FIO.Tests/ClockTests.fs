module FIO.Tests.ClockTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL

open Expecto

open FIO.Runtime

open System

module Clock = FIO.Clock

[<Tests>]
let clockNowTests =
    testList "Clock.now" [

        testPropertyWithConfig fsCheckConfig "Clock.now returns current local time"
        <| fun (runtime: FIORuntime) ->
            let before = DateTime.Now
            let eff = Clock.now<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            let after = DateTime.Now
            Expect.isGreaterThanOrEqual result before "Should be >= before"
            Expect.isLessThanOrEqual result after "Should be <= after"

        testPropertyWithConfig fsCheckConfig "Clock.now increases monotonically"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! t1 = Clock.now<exn>()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 5.0)
                let! t2 = Clock.now<exn>()
                return t2 > t1
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Clock.now should increase over time"
    ]

[<Tests>]
let clockUtcNowTests =
    testList "Clock.utcNow" [

        testPropertyWithConfig fsCheckConfig "Clock.utcNow returns current UTC time"
        <| fun (runtime: FIORuntime) ->
            let before = DateTime.UtcNow
            let eff = Clock.utcNow<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            let after = DateTime.UtcNow
            Expect.isGreaterThanOrEqual result before "Should be >= before"
            Expect.isLessThanOrEqual result after "Should be <= after"

        testPropertyWithConfig fsCheckConfig "Clock.utcNow increases monotonically"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! t1 = Clock.utcNow<exn>()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 5.0)
                let! t2 = Clock.utcNow<exn>()
                return t2 > t1
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Clock.utcNow should increase over time"
    ]

[<Tests>]
let clockTimestampTests =
    testList "Clock.timestamp" [

        testPropertyWithConfig fsCheckConfig "Clock.timestamp returns ticks"
        <| fun (runtime: FIORuntime) ->
            let before = DateTime.UtcNow.Ticks
            let eff = Clock.timestamp<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            let after = DateTime.UtcNow.Ticks
            Expect.isGreaterThanOrEqual result before "Should be >= before"
            Expect.isLessThanOrEqual result after "Should be <= after"

        testPropertyWithConfig fsCheckConfig "Clock.timestamp increases monotonically"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! t1 = Clock.timestamp<exn>()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 5.0)
                let! t2 = Clock.timestamp<exn>()
                return t2 > t1
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Clock.timestamp should increase over time"
    ]

[<Tests>]
let clockTimestampMillisTests =
    testList "Clock.timestampMillis" [

        testPropertyWithConfig fsCheckConfig "Clock.timestampMillis returns milliseconds"
        <| fun (runtime: FIORuntime) ->
            let before = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            let eff = Clock.timestampMillis<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            let after = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
            Expect.isGreaterThanOrEqual result before "Should be >= before"
            Expect.isLessThanOrEqual result after "Should be <= after"

        testPropertyWithConfig fsCheckConfig "Clock.timestampMillis increases monotonically"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! t1 = Clock.timestampMillis<exn>()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 5.0)
                let! t2 = Clock.timestampMillis<exn>()
                return t2 > t1
            }
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.isTrue result "Clock.timestampMillis should increase over time"
    ]

[<Tests>]
let clockTimedTests =
    testList "Clock.timed" [

        testPropertyWithConfig fsCheckConfig "Clock.timed measures execution time"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let innerEff = fio {
                    do! FIO.sleepExn(TimeSpan.FromMilliseconds 20.0)
                    return res
                }
                let! (result, duration) = Clock.timed innerEff
                return (result, duration)
            }
            let (result, duration) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Should return inner result"
            Expect.isGreaterThanOrEqual duration.TotalMilliseconds 15.0 "Duration should be >= 15ms"

        testPropertyWithConfig fsCheckConfig "Clock.timed measures fast execution"
        <| fun (runtime: FIORuntime, res: int) ->
            let eff = fio {
                let innerEff = FIO.succeed res
                let! (result, duration) = Clock.timed innerEff
                return (result, duration)
            }
            let (result, duration) = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result res "Should return inner result"
            Expect.isGreaterThanOrEqual duration TimeSpan.Zero "Duration should be >= 0"
    ]

[<Tests>]
let clockConversionTests =
    testList "Clock Conversions" [

        testPropertyWithConfig fsCheckConfig "toTimestamp and fromTimestamp are inverses"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! now = Clock.utcNow<exn>()
                let! timestamp = Clock.toTimestamp<exn> now
                let! roundTrip = Clock.fromTimestamp<exn> timestamp
                return (now, roundTrip)
            }
            let (original, roundTrip) = runtime.Run(eff).UnsafeSuccess()
            let diff = abs (original.Ticks - roundTrip.Ticks)
            Expect.isLessThan diff 10000L "Round trip should be close to original"

        testPropertyWithConfig fsCheckConfig "getElapsedTime calculates difference"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let! start = Clock.getTimestamp<exn>()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0)
                let! end_ = Clock.getTimestamp<exn>()
                let! elapsed = Clock.getElapsedTime<exn>(start, end_)
                return elapsed
            }
            let elapsed = runtime.Run(eff).UnsafeSuccess()
            Expect.isGreaterThanOrEqual elapsed.TotalMilliseconds 8.0 "Elapsed should be >= 8ms"

        testPropertyWithConfig fsCheckConfig "Clock.today returns date without time"
        <| fun (runtime: FIORuntime) ->
            let eff = Clock.today<exn>()
            let result = runtime.Run(eff).UnsafeSuccess()
            Expect.equal result.Hour 0 "Hour should be 0"
            Expect.equal result.Minute 0 "Minute should be 0"
            Expect.equal result.Second 0 "Second should be 0"
    ]
