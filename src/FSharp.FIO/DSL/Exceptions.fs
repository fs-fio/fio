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
    /// Fiber was interrupted due to a timeout.
    | Timeout of durationMs: float
    /// Fiber was interrupted because its parent fiber was interrupted.
    | ParentInterrupted of parentFiberId: Guid
    /// Fiber was explicitly interrupted via Fiber.Interrupt().
    | ExplicitInterrupt
    /// Fiber was interrupted due to resource exhaustion or system limits.
    | ResourceExhaustion of reason: string

    override this.ToString() =
        match this with
        | Timeout ms -> $"Timeout ({ms}ms)"
        | ParentInterrupted id -> $"ParentInterrupted ({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | ResourceExhaustion r -> $"ResourceExhaustion ({r})"

/// <summary>
/// Exception thrown when a fiber is interrupted during execution.
/// </summary>
exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    override this.Message =
        $"Fiber {this.fiberId} interrupted. Cause: {this.cause}. Message: {this.message}"
