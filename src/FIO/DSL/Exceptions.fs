[<AutoOpen>]
module FIO.DSL.Exceptions

open System

type InterruptionCause =
    | ParentInterrupted of parentFiberId: Guid
    | ExplicitInterrupt
    | InvalidArgument of argumentName: string * reason: string
    | ResourceExhaustion of reason: string

    override this.ToString () =
        match this with
        | ParentInterrupted id -> $"ParentInterrupted ({id})"
        | ExplicitInterrupt -> "ExplicitInterrupt"
        | InvalidArgument(arg, reason) -> $"InvalidArgument ({arg}: {reason})"
        | ResourceExhaustion reason -> $"ResourceExhaustion ({reason})"

exception FiberInterruptedException of fiberId: Guid * cause: InterruptionCause * message: string with

    override this.Message =
        $"Fiber {this.fiberId} interrupted. Cause: {this.cause}. Message: {this.message}"
