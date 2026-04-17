/// Time and date operations as FIO effects.
[<RequireQualifiedAccess>]
module FIO.Clock

open FIO.DSL

open System
open System.Diagnostics

let inline private rethrow (ex: exn) : 'E = raise ex

/// Gets the current local date and time.
/// <returns>An effect that produces the current local DateTime.</returns>
let inline now<'E> () : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTime.Now), rethrow)

/// Gets the current UTC date and time.
/// <returns>An effect that produces the current UTC DateTime.</returns>
let inline utcNow<'E> () : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTime.UtcNow), rethrow)

/// Gets the current local date (time component is midnight).
/// <returns>An effect that produces today's date as a DateTime.</returns>
let inline today<'E> () : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTime.Today), rethrow)

/// Gets the current local date and time with timezone offset.
/// <returns>An effect that produces the current local DateTimeOffset.</returns>
let inline nowOffset<'E> () : FIO<DateTimeOffset, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.Now), rethrow)

/// Gets the current UTC date and time with timezone offset.
/// <returns>An effect that produces the current UTC DateTimeOffset.</returns>
let inline utcNowOffset<'E> () : FIO<DateTimeOffset, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.UtcNow), rethrow)

/// Gets the current Unix timestamp in seconds.
/// <returns>An effect that produces the current Unix timestamp in seconds.</returns>
let inline timestamp<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.UtcNow.ToUnixTimeSeconds()), rethrow)

/// Gets the current Unix timestamp in milliseconds.
/// <returns>An effect that produces the current Unix timestamp in milliseconds.</returns>
let inline timestampMillis<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()), rethrow)

/// Gets a high-resolution timestamp for measuring elapsed time.
/// <returns>An effect that produces a high-resolution timestamp value.</returns>
let inline getTimestamp<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Stopwatch.GetTimestamp()), rethrow)

/// Calculates elapsed time between two timestamps.
/// <param name="startTimestamp">Starting timestamp from getTimestamp.</param>
/// <param name="endTimestamp">Ending timestamp from getTimestamp.</param>
/// <returns>An effect that produces the elapsed TimeSpan between the two timestamps.</returns>
let inline getElapsedTime<'E> (startTimestamp: int64, endTimestamp: int64) : FIO<TimeSpan, 'E> =
    FIO.attempt ((fun () -> Stopwatch.GetElapsedTime(startTimestamp, endTimestamp)), rethrow)

/// Measures the execution time of an effect.
/// <param name="eff">The effect to time.</param>
/// <returns>An effect that produces the result paired with the elapsed TimeSpan.</returns>
let inline timed<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R * TimeSpan, 'E> =
    FIO
        .attempt((fun () -> Stopwatch.GetTimestamp()), rethrow)
        .FlatMap(fun startTime ->
            eff.FlatMap(fun result -> FIO.attempt ((fun () -> result, Stopwatch.GetElapsedTime startTime), rethrow)))

/// Converts a DateTime to Unix timestamp in seconds.
/// <param name="dateTime">The DateTime to convert.</param>
/// <returns>An effect that produces the Unix timestamp in seconds.</returns>
let inline toTimestamp<'E> (dateTime: DateTime) : FIO<int64, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset(dateTime).ToUnixTimeSeconds()), rethrow)

/// Converts a Unix timestamp in seconds to a UTC DateTime.
/// <param name="timestamp">The Unix timestamp in seconds.</param>
/// <returns>An effect that produces the corresponding UTC DateTime.</returns>
let inline fromTimestamp<'E> (timestamp: int64) : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.FromUnixTimeSeconds(timestamp).UtcDateTime), rethrow)

/// Converts a Unix timestamp in milliseconds to a UTC DateTime.
/// <param name="timestampMillis">The Unix timestamp in milliseconds.</param>
/// <returns>An effect that produces the corresponding UTC DateTime.</returns>
let inline fromTimestampMillis<'E> (timestampMillis: int64) : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.FromUnixTimeMilliseconds(timestampMillis).UtcDateTime), rethrow)
