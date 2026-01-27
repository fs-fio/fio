/// <summary>
/// Exception types for fiber interruption in the FIO effect system.
/// </summary>
[<AutoOpen>]
module FSharp.FIO.DSL.Exceptions

open System

/// <summary>
/// Represents the cause or reason for fiber interruption.
/// </summary>
type InterruptionCause =
    /// <summary>
    /// Fiber was interrupted due to a timeout.
    /// </summary>
    | Timeout of durationMs: float
    /// <summary>
    /// Fiber was interrupted because its parent fiber was interrupted.
    /// </summary>
    | ParentInterrupted of parentFiberId: Guid
    /// <summary>
    /// Fiber was explicitly interrupted via Fiber.Interrupt().
    /// </summary>
    | ExplicitInterrupt
    /// <summary>
    /// Fiber was interrupted due to invalid argument or precondition violation.
    /// </summary>
    | InvalidArgument of argumentName: string * reason: string
    /// <summary>
    /// Fiber was interrupted due to resource exhaustion or system limits.
    /// </summary>
    | ResourceExhaustion of reason: string

    override this.ToString () =
        match this with
        | Timeout ms -> $"Timeout ({ms}ms)"
        | ParentInterrupted id -> $"ParentInterrupted ({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | InvalidArgument(arg, reason) -> $"InvalidArgument ({arg}: {reason})"
        | ResourceExhaustion reason -> $"ResourceExhaustion ({reason})"

/// <summary>
/// Exception thrown when a fiber is interrupted during execution.
/// </summary>
exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    override this.Message =
        $"Fiber {this.fiberId} interrupted. Cause: {this.cause}. Message: {this.message}"
