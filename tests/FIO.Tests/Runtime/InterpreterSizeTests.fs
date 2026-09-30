module FIO.Tests.InterpreterSizeTests

open FIO.DSL

open Expecto

open System
open System.Diagnostics
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices

// The JIT silently compiles a method of more than 2,000 basic blocks without optimization; the count is estimated
// from the IL of the Release build's state machine, so the check needs `dotnet test -c Release`.
let private blockLimit = 1_800

let private opcodes =
    let oneByte = Array.zeroCreate<OpCode> 256
    let twoByte = Array.zeroCreate<OpCode> 256

    for field in typeof<OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static) do
        let opcode = field.GetValue null :?> OpCode
        let value = uint16 opcode.Value

        if value < 0x100us then
            oneByte.[int value] <- opcode
        else
            twoByte.[int (value &&& 0xFFus)] <- opcode

    oneByte, twoByte

let private estimatedBasicBlocks (body: MethodBody) =
    let oneByte, twoByte = opcodes
    let il = body.GetILAsByteArray()
    let starts = Collections.Generic.HashSet<int> [ 0 ]
    let mutable i = 0

    while i < il.Length do
        let opcode =
            if il.[i] = 0xFEuy then
                i <- i + 1
                twoByte.[int il.[i]]
            else
                oneByte.[int il.[i]]

        i <- i + 1

        let operandSize =
            match opcode.OperandType with
            | OperandType.InlineNone -> 0
            | OperandType.ShortInlineBrTarget
            | OperandType.ShortInlineI
            | OperandType.ShortInlineVar -> 1
            | OperandType.InlineVar -> 2
            | OperandType.InlineI8
            | OperandType.InlineR -> 8
            | OperandType.InlineSwitch -> 4 + 4 * BitConverter.ToInt32(il, i)
            | _ -> 4

        let next = i + operandSize

        match opcode.OperandType with
        | OperandType.ShortInlineBrTarget ->
            starts.Add(next + int (sbyte il.[i])) |> ignore
            starts.Add next |> ignore
        | OperandType.InlineBrTarget ->
            starts.Add(next + BitConverter.ToInt32(il, i)) |> ignore
            starts.Add next |> ignore
        | OperandType.InlineSwitch ->
            for k in 0 .. BitConverter.ToInt32(il, i) - 1 do
                starts.Add(next + BitConverter.ToInt32(il, i + 4 + 4 * k)) |> ignore

            starts.Add next |> ignore
        | _ ->
            if opcode.FlowControl = FlowControl.Return
               || opcode.FlowControl = FlowControl.Throw
               || opcode.Name.StartsWith "leave"
               || opcode.Name = "endfinally" then
                starts.Add next |> ignore

        i <- next

    starts.Count + 2 * body.ExceptionHandlingClauses.Count

let private interpreterLoops () =
    typeof<FIO<obj, obj>>.Assembly.GetTypes()
    |> Array.filter (fun t -> t.FullName.Contains "InterpretAsync")
    |> Array.choose (fun t ->
        match t.GetMethod("MoveNext", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) with
        | null -> None
        | moveNext ->
            let body = moveNext.GetMethodBody()

            if body.GetILAsByteArray().Length > 1000 then
                Some(t.FullName.Split('+').[0], body)
            else
                None)

let private optimizedBuild =
    match typeof<FIO<obj, obj>>.Assembly.GetCustomAttribute<DebuggableAttribute>() with
    | null -> true
    | attribute -> not attribute.IsJITOptimizerDisabled

[<Tests>]
let interpreterSizeTests =
    testList
        "Interpreter size"
        [
            for runtime in [ "Direct"; "Polling"; "Signaling"; "WorkStealing" ] ->
                testCase $"{runtime}Runtime - interpreter loop stays under the JIT's optimization limit"
                <| fun () ->
                    if not optimizedBuild then
                        skiptest "the interpreter loop is only a state machine in optimized builds; run with -c Release"

                    let loops = interpreterLoops () |> Array.filter (fun (name, _) -> name = $"FIO.Runtime.{runtime}")
                    Expect.isNonEmpty loops $"No interpreter state machine found for {runtime}"

                    for _, body in loops do
                        let blocks = estimatedBasicBlocks body

                        Expect.isLessThan
                            blocks
                            blockLimit
                            $"The {runtime} interpreter loop has ~{blocks} basic blocks; at 2,000 the JIT stops optimizing it. Move rarely taken code out of the inlined processOutcome or handleSharedCase"

            yield testCase "Cont - stays 56 bytes"
            <| fun () -> Expect.isLessThanOrEqual (Unsafe.SizeOf<Cont>()) 56 "Cont grew"
        ]
