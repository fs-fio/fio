/// Exception types for fiber interruption.
[<AutoOpen>]
module FIO.DSL.Exceptions

open System

/// Represents the cause of fiber interruption.
type InterruptionCause =
    /// Interrupted due to a timeout.
    | Timeout of durationMs: float
    /// Interrupted because the parent fiber was interrupted.
    | ParentInterrupted of parentFiberId: Guid
    /// Explicitly interrupted via Fiber.Interrupt().
    | ExplicitInterrupt
    /// Interrupted due to invalid argument or precondition violation.
    | InvalidArgument of argumentName: string * reason: string
    /// Interrupted due to resource exhaustion.
    | ResourceExhaustion of reason: string

    /// Returns a human-readable representation of the interruption cause.
    override this.ToString() =
        match this with
        | Timeout ms -> $"Timeout ({ms}ms)"
        | ParentInterrupted id -> $"ParentInterrupted ({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | InvalidArgument(arg, reason) -> $"InvalidArgument ({arg}: {reason})"
        | ResourceExhaustion reason -> $"ResourceExhaustion ({reason})"

/// Exception thrown when a fiber is interrupted.
exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    /// Gets the formatted exception message.
    override this.Message =
        $"Fiber {this.fiberId} interrupted. Cause: {this.cause}. Message: {this.message}"
