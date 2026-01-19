/// <summary>
/// Time and date operations as FIO effects.
/// </summary>
[<RequireQualifiedAccess>]
module FSharp.FIO.Clock

open FSharp.FIO.DSL

open System
open System.Diagnostics

/// <summary>Gets the current local date and time.</summary>
/// <returns>Effect returning the current local DateTime.</returns>
let Now<'E> () : FIO<DateTime, 'E> =
    FIO.Succeed DateTime.Now

/// <summary>Gets the current UTC date and time.</summary>
/// <returns>Effect returning the current UTC DateTime.</returns>
let UtcNow<'E> () : FIO<DateTime, 'E> =
    FIO.Succeed DateTime.UtcNow

/// <summary>Gets the current local date (time component is midnight).</summary>
/// <returns>Effect returning today's date.</returns>
let Today<'E> () : FIO<DateTime, 'E> =
    FIO.Succeed DateTime.Today

/// <summary>Gets the current local date and time with timezone offset.</summary>
/// <returns>Effect returning the current local DateTimeOffset.</returns>
let NowOffset<'E> () : FIO<DateTimeOffset, 'E> =
    FIO.Succeed DateTimeOffset.Now

/// <summary>Gets the current UTC date and time with timezone offset.</summary>
/// <returns>Effect returning the current UTC DateTimeOffset.</returns>
let UtcNowOffset<'E> () : FIO<DateTimeOffset, 'E> =
    FIO.Succeed DateTimeOffset.UtcNow

/// <summary>Gets the current Unix timestamp in seconds since 1970-01-01 00:00:00 UTC.</summary>
/// <returns>Effect returning Unix timestamp in seconds.</returns>
let Timestamp<'E> () : FIO<int64, 'E> =
    FIO.Succeed <| DateTimeOffset.UtcNow.ToUnixTimeSeconds()

/// <summary>Gets the current Unix timestamp in milliseconds since 1970-01-01 00:00:00 UTC.</summary>
/// <returns>Effect returning Unix timestamp in milliseconds.</returns>
let TimestampMillis<'E> () : FIO<int64, 'E> =
    FIO.Succeed <| DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

/// <summary>Gets high-resolution timestamp for measuring elapsed time.</summary>
/// <returns>Effect returning stopwatch timestamp ticks.</returns>
let GetTimestamp<'E> () : FIO<int64, 'E> =
    FIO.Succeed <| Stopwatch.GetTimestamp()

/// <summary>Calculates elapsed time between two timestamps.</summary>
/// <param name="startTimestamp">Starting timestamp from GetTimestamp.</param>
/// <param name="endTimestamp">Ending timestamp from GetTimestamp.</param>
/// <returns>Effect returning the elapsed TimeSpan.</returns>
let GetElapsedTime<'E> (startTimestamp: int64, endTimestamp: int64) : FIO<TimeSpan, 'E> =
    FIO.Succeed <| Stopwatch.GetElapsedTime(startTimestamp, endTimestamp)

/// <summary>Measures the execution time of an effect.</summary>
/// <param name="effect">Effect to measure.</param>
/// <returns>Effect returning a tuple of (result, elapsed time).</returns>
let Timed<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R * TimeSpan, 'E> =
    FIO.Succeed(Stopwatch.GetTimestamp())
        .FlatMap(fun startTime ->
            eff.FlatMap(fun result ->
                let elapsed = Stopwatch.GetElapsedTime startTime
                FIO.Succeed(result, elapsed)))

/// <summary>Converts a DateTime to Unix timestamp in seconds.</summary>
/// <param name="dateTime">DateTime to convert (assumed UTC).</param>
/// <returns>Effect returning Unix timestamp in seconds.</returns>
let ToTimestamp<'E> (dateTime: DateTime) : FIO<int64, 'E> =
    FIO.Succeed <| DateTimeOffset(dateTime).ToUnixTimeSeconds()

/// <summary>Converts a Unix timestamp in seconds to DateTime.</summary>
/// <param name="timestamp">Unix timestamp in seconds.</param>
/// <returns>Effect returning UTC DateTime.</returns>
let FromTimestamp<'E> (timestamp: int64) : FIO<DateTime, 'E> =
    FIO.Succeed <| DateTimeOffset.FromUnixTimeSeconds(timestamp).UtcDateTime

/// <summary>Converts a Unix timestamp in milliseconds to DateTime.</summary>
/// <param name="timestampMillis">Unix timestamp in milliseconds.</param>
/// <returns>Effect returning UTC DateTime.</returns>
let FromTimestampMillis<'E> (timestampMillis: int64) : FIO<DateTime, 'E> =
    FIO.Succeed <| DateTimeOffset.FromUnixTimeMilliseconds(timestampMillis).UtcDateTime
