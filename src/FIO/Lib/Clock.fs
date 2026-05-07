/// <summary>Provides time and date operations as FIO effects.</summary>
[<RequireQualifiedAccess>]
module FIO.Clock

open FIO.DSL

open System
open System.Diagnostics

/// <summary>Transforms an exception by re-raising it, used as the error-mapping function when side effects cannot fail with a typed error.</summary>
/// <param name="ex">The exception to re-raise.</param>
/// <returns>Never returns; always throws <paramref name="ex"/>.</returns>
let inline private rethrow (ex: exn) : 'E = raise ex

/// <summary>Returns the current local date and time as an effect.</summary>
/// <returns>An effect that completes with the current local <c>DateTime</c>.</returns>
let inline now<'E> () : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTime.Now), rethrow)

/// <summary>Returns the current UTC date and time as an effect.</summary>
/// <returns>An effect that completes with the current UTC <c>DateTime</c>.</returns>
let inline utcNow<'E> () : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTime.UtcNow), rethrow)

/// <summary>Returns today's local date with the time component at midnight.</summary>
/// <returns>An effect that completes with today's <c>DateTime</c>.</returns>
let inline today<'E> () : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTime.Today), rethrow)

/// <summary>Returns the current local date and time with timezone offset.</summary>
/// <returns>An effect that completes with the current local <c>DateTimeOffset</c>.</returns>
let inline nowOffset<'E> () : FIO<DateTimeOffset, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.Now), rethrow)

/// <summary>Returns the current UTC date and time with timezone offset.</summary>
/// <returns>An effect that completes with the current UTC <c>DateTimeOffset</c>.</returns>
let inline utcNowOffset<'E> () : FIO<DateTimeOffset, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.UtcNow), rethrow)

/// <summary>Returns the current Unix timestamp expressed in seconds.</summary>
/// <returns>An effect that completes with the seconds elapsed since the Unix epoch.</returns>
let inline timestamp<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.UtcNow.ToUnixTimeSeconds()), rethrow)

/// <summary>Returns the current Unix timestamp expressed in milliseconds.</summary>
/// <returns>An effect that completes with the milliseconds elapsed since the Unix epoch.</returns>
let inline timestampMillis<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()), rethrow)

/// <summary>Returns a high-resolution timestamp suitable for measuring elapsed time.</summary>
/// <returns>An effect that completes with a monotonic timestamp value.</returns>
let inline getTimestamp<'E> () : FIO<int64, 'E> =
    FIO.attempt ((fun () -> Stopwatch.GetTimestamp()), rethrow)

/// <summary>Returns the elapsed time between two high-resolution timestamps.</summary>
/// <param name="startTimestamp">The starting timestamp obtained from <c>getTimestamp</c>.</param>
/// <param name="endTimestamp">The ending timestamp obtained from <c>getTimestamp</c>.</param>
/// <returns>An effect that completes with the duration between the two timestamps.</returns>
let inline getElapsedTime<'E> (startTimestamp: int64, endTimestamp: int64) : FIO<TimeSpan, 'E> =
    FIO.attempt ((fun () -> Stopwatch.GetElapsedTime(startTimestamp, endTimestamp)), rethrow)

/// <summary>Combines an effect with timing instrumentation, producing both the result and the elapsed duration.</summary>
/// <param name="eff">The effect to time.</param>
/// <returns>An effect that completes with a tuple of the result and the elapsed time.</returns>
let inline timed<'R, 'E> (eff: FIO<'R, 'E>) : FIO<'R * TimeSpan, 'E> =
    FIO
        .attempt((fun () -> Stopwatch.GetTimestamp()), rethrow)
        .FlatMap(fun startTime ->
            eff.FlatMap(fun result -> FIO.attempt ((fun () -> result, Stopwatch.GetElapsedTime startTime), rethrow)))

/// <summary>Transforms a <c>DateTime</c> into its Unix timestamp expressed in seconds.</summary>
/// <param name="dateTime">The date and time to convert.</param>
/// <returns>An effect that completes with the corresponding seconds since the Unix epoch.</returns>
let inline toTimestamp<'E> (dateTime: DateTime) : FIO<int64, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset(dateTime).ToUnixTimeSeconds()), rethrow)

/// <summary>Transforms a Unix timestamp expressed in seconds into a UTC <c>DateTime</c>.</summary>
/// <param name="timestamp">The seconds since the Unix epoch.</param>
/// <returns>An effect that completes with the corresponding UTC <c>DateTime</c>.</returns>
let inline fromTimestamp<'E> (timestamp: int64) : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.FromUnixTimeSeconds(timestamp).UtcDateTime), rethrow)

/// <summary>Transforms a Unix timestamp expressed in milliseconds into a UTC <c>DateTime</c>.</summary>
/// <param name="timestampMillis">The milliseconds since the Unix epoch.</param>
/// <returns>An effect that completes with the corresponding UTC <c>DateTime</c>.</returns>
let inline fromTimestampMillis<'E> (timestampMillis: int64) : FIO<DateTime, 'E> =
    FIO.attempt ((fun () -> DateTimeOffset.FromUnixTimeMilliseconds(timestampMillis).UtcDateTime), rethrow)
