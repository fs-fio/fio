/// <summary>Provides exception types for fiber interruption.</summary>
[<AutoOpen>]
module FIO.DSL.Exceptions

open System

/// <summary>Represents the cause of a fiber's interruption.</summary>
type InterruptionCause =
    /// <summary>Represents interruption cascaded from a parent fiber, carrying the parent's identifier.</summary>
    | ParentInterrupted of parentFiberId: Guid
    /// <summary>Represents an explicit interruption requested via <c>Fiber.Interrupt</c>.</summary>
    | ExplicitInterrupt
    /// <summary>Represents interruption caused by an invalid argument or precondition violation.</summary>
    | InvalidArgument of argumentName: string * reason: string
    /// <summary>Represents interruption caused by exhaustion of a runtime resource.</summary>
    | ResourceExhaustion of reason: string

    /// <summary>Returns a human-readable description of this cause and its associated data.</summary>
    /// <returns>A short string identifying the cause and any data it carries.</returns>
    override this.ToString() =
        match this with
        | ParentInterrupted id -> $"ParentInterrupted ({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | InvalidArgument(arg, reason) -> $"InvalidArgument ({arg}: {reason})"
        | ResourceExhaustion reason -> $"ResourceExhaustion ({reason})"

/// <summary>Represents the exception raised when a fiber is interrupted before completion.</summary>
exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    /// <summary>Returns the formatted message describing the interrupted fiber, its cause, and the supplied reason.</summary>
    /// <returns>A composed message including the fiber identifier, the cause, and the original message.</returns>
    override this.Message =
        $"Fiber {this.fiberId} interrupted. Cause: {this.cause}. Message: {this.message}"
