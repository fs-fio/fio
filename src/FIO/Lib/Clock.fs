/// <summary>
/// Time and date operations as FIO effects.
/// </summary>
[<RequireQualifiedAccess>]
module FIO.Clock

open FIO.DSL

open System
open System.Diagnostics

/// <summary>Gets the current local date and time.</summary>
/// <returns>Effect returning the current local DateTime.</returns>
let now<'E> () : FIO<DateTime, 'E> = FIO.succeed DateTime.Now

/// <summary>Gets the current UTC date and time.</summary>
/// <returns>Effect returning the current UTC DateTime.</returns>
let utcNow<'E> () : FIO<DateTime, 'E> = FIO.succeed DateTime.UtcNow

/// <summary>Gets the current local date (time component is midnight).</summary>
/// <returns>Effect returning today's date.</returns>
let today<'E> () : FIO<DateTime, 'E> = FIO.succeed DateTime.Today

/// <summary>Gets the current local date and time with timezone offset.</summary>
/// <returns>Effect returning the current local DateTimeOffset.</returns>
let nowOffset<'E> () : FIO<DateTimeOffset, 'E> = FIO.succeed DateTimeOffset.Now

/// <summary>Gets the current UTC date and time with timezone offset.</summary>
/// <returns>Effect returning the current UTC DateTimeOffset.</returns>
let utcNowOffset<'E> () : FIO<DateTimeOffset, 'E> = FIO.succeed DateTimeOffset.UtcNow

/// <summary>Gets the current Unix timestamp in seconds since 1970-01-01 00:00:00 UTC.</summary>
/// <returns>Effect returning Unix timestamp in seconds.</returns>
let timestamp<'E> () : FIO<int64, 'E> =
    FIO.succeed <| DateTimeOffset.UtcNow.ToUnixTimeSeconds()

/// <summary>Gets the current Unix timestamp in milliseconds since 1970-01-01 00:00:00 UTC.</summary>
/// <returns>Effect returning Unix timestamp in milliseconds.</returns>
let timestampMillis<'E> () : FIO<int64, 'E> =
    FIO.succeed <| DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

/// <summary>Gets high-resolution timestamp for measuring elapsed time.</summary>
/// <returns>Effect returning stopwatch timestamp ticks.</returns>
let getTimestamp<'E> () : FIO<int64, 'E> = FIO.succeed <| Stopwatch.GetTimestamp()

/// <summary>Calculates elapsed time between two timestamps.</summary>
/// <param name="startTimestamp">Starting timestamp from getTimestamp.</param>
/// <param name="endTimestamp">Ending timestamp from getTimestamp.</param>
/// <returns>Effect returning the elapsed TimeSpan.</returns>
let getElapsedTime<'E> (startTimestamp: int64, endTimestamp: int64) : FIO<TimeSpan, 'E> =
    FIO.succeed <| Stopwatch.GetElapsedTime(startTimestamp, endTimestamp)

/// <summary>Measures the execution time of an effect.</summary>
/// <param name="eff">Effect to measure.</param>
/// <returns>Effect returning a tuple of (result, elapsed time).</returns>
let timed<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R * TimeSpan, 'E> =
    FIO
        .succeed(Stopwatch.GetTimestamp())
        .FlatMap(fun startTime ->
            eff.FlatMap(fun result ->
                let elapsed = Stopwatch.GetElapsedTime startTime
                FIO.succeed (result, elapsed)))

/// <summary>Converts a DateTime to Unix timestamp in seconds.</summary>
/// <param name="dateTime">DateTime to convert (assumed UTC).</param>
/// <returns>Effect returning Unix timestamp in seconds.</returns>
let toTimestamp<'E> (dateTime: DateTime) : FIO<int64, 'E> =
    FIO.succeed <| DateTimeOffset(dateTime).ToUnixTimeSeconds()

/// <summary>Converts a Unix timestamp in seconds to DateTime.</summary>
/// <param name="timestamp">Unix timestamp in seconds.</param>
/// <returns>Effect returning UTC DateTime.</returns>
let fromTimestamp<'E> (timestamp: int64) : FIO<DateTime, 'E> =
    FIO.succeed <| DateTimeOffset.FromUnixTimeSeconds(timestamp).UtcDateTime

/// <summary>Converts a Unix timestamp in milliseconds to DateTime.</summary>
/// <param name="timestampMillis">Unix timestamp in milliseconds.</param>
/// <returns>Effect returning UTC DateTime.</returns>
let fromTimestampMillis<'E> (timestampMillis: int64) : FIO<DateTime, 'E> =
    FIO.succeed
    <| DateTimeOffset.FromUnixTimeMilliseconds(timestampMillis).UtcDateTime
