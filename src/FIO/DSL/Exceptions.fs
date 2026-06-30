namespace FIO.DSL

open System

/// The reason a fiber was interrupted.
type InterruptionCause =
    /// The fiber was interrupted because its parent fiber was interrupted.
    | ParentInterrupted of parentFiberId: Guid
    /// The fiber was interrupted by an explicit interrupt request.
    | ExplicitInterrupt
    /// The fiber was interrupted because an argument was invalid.
    | InvalidArgument of argumentName: string * reason: string
    /// The fiber was interrupted because a resource was exhausted.
    | ResourceExhaustion of reason: string

    override this.ToString () =
        match this with
        | ParentInterrupted id -> $"ParentInterrupted ({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | InvalidArgument(arg, reason) -> $"InvalidArgument ({arg}: {reason})"
        | ResourceExhaustion reason -> $"ResourceExhaustion ({reason})"

/// Raised inside a fiber to interrupt it, carrying the interrupted fiber's id, the cause, and a message.
exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    override this.Message =
        $"Fiber {this.fiberId} interrupted. Cause: {this.cause}. Message: {this.message}"
