/// Time and date operations as FIO effects.
[<RequireQualifiedAccess>]
module FIO.Clock

open FIO.DSL

open System
open System.Diagnostics

/// Gets the current local date and time.
let inline now<'E> () : FIO<DateTime, 'E> = FIO.succeed DateTime.Now

/// Gets the current UTC date and time.
let inline utcNow<'E> () : FIO<DateTime, 'E> = FIO.succeed DateTime.UtcNow

/// Gets the current local date (time component is midnight).
let inline today<'E> () : FIO<DateTime, 'E> = FIO.succeed DateTime.Today

/// Gets the current local date and time with timezone offset.
let inline nowOffset<'E> () : FIO<DateTimeOffset, 'E> = FIO.succeed DateTimeOffset.Now

/// Gets the current UTC date and time with timezone offset.
let inline utcNowOffset<'E> () : FIO<DateTimeOffset, 'E> = FIO.succeed DateTimeOffset.UtcNow

/// Gets the current Unix timestamp in seconds.
let inline timestamp<'E> () : FIO<int64, 'E> =
    FIO.succeed <| DateTimeOffset.UtcNow.ToUnixTimeSeconds()

/// Gets the current Unix timestamp in milliseconds.
let inline timestampMillis<'E> () : FIO<int64, 'E> =
    FIO.succeed <| DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

/// Gets a high-resolution timestamp for measuring elapsed time.
let inline getTimestamp<'E> () : FIO<int64, 'E> = FIO.succeed <| Stopwatch.GetTimestamp()

/// Calculates elapsed time between two timestamps.
/// <param name="startTimestamp">Starting timestamp from getTimestamp.</param>
/// <param name="endTimestamp">Ending timestamp from getTimestamp.</param>
let inline getElapsedTime<'E> (startTimestamp: int64, endTimestamp: int64) : FIO<TimeSpan, 'E> =
    FIO.succeed <| Stopwatch.GetElapsedTime(startTimestamp, endTimestamp)

/// Measures the execution time of an effect.
let inline timed<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R * TimeSpan, 'E> =
    FIO
        .succeed(Stopwatch.GetTimestamp())
        .FlatMap(fun startTime ->
            eff.FlatMap(fun result ->
                let elapsed = Stopwatch.GetElapsedTime startTime
                FIO.succeed (result, elapsed)))

/// Converts a DateTime to Unix timestamp in seconds.
let inline toTimestamp<'E> (dateTime: DateTime) : FIO<int64, 'E> =
    FIO.succeed <| DateTimeOffset(dateTime).ToUnixTimeSeconds()

/// Converts a Unix timestamp in seconds to a UTC DateTime.
let inline fromTimestamp<'E> (timestamp: int64) : FIO<DateTime, 'E> =
    FIO.succeed <| DateTimeOffset.FromUnixTimeSeconds(timestamp).UtcDateTime

/// Converts a Unix timestamp in milliseconds to a UTC DateTime.
let inline fromTimestampMillis<'E> (timestampMillis: int64) : FIO<DateTime, 'E> =
    FIO.succeed
    <| DateTimeOffset.FromUnixTimeMilliseconds(timestampMillis).UtcDateTime
