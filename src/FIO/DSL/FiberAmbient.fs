module internal FIO.DSL.FiberAmbient

open System
open System.Threading

[<Sealed; AbstractClass>]
type private Slot =
    [<DefaultValue; ThreadStatic>]
    static val mutable private current: FiberContext

    static member Get() : FiberContext = Slot.current
    static member Set(ctx: FiberContext) : unit = Slot.current <- ctx

let inline tryGet () : FiberContext voption =
    let c = Slot.Get()

    if Object.ReferenceEquals(c, null) then
        ValueNone
    else
        ValueSome c

let inline currentToken () : CancellationToken =
    let c = Slot.Get()

    if Object.ReferenceEquals(c, null) then
        CancellationToken.None
    else
        c.CancellationToken

let inline withContext (ctx: FiberContext) ([<InlineIfLambda>] body: unit -> 'a) : 'a =
    let prev = Slot.Get()
    Slot.Set ctx

    try
        body ()
    finally
        Slot.Set prev
