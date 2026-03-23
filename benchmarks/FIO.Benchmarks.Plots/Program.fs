/// <summary>
/// Entry point for the FIO benchmark plotting application.
/// </summary>
module private FIO.Benchmarks.Plots.Program

open FIO.Benchmarks.Plots

[<Literal>]
let private SuccessExitCode = 0

[<Literal>]
let private RuntimeErrorExitCode = 1

[<Literal>]
let private InvalidArgsExitCode = 2

let private showPlot plotArgs =
    PlotArgs.show plotArgs

let internal runWithArgsUsing execute args =
    match Args.parse args with
    | Args.HelpRequested usage ->
        printfn "%s" usage
        SuccessExitCode
    | Args.InvalidArgs(errorText, usage) ->
        eprintfn "%s" errorText
        eprintfn "%s" usage
        InvalidArgsExitCode
    | Args.Parsed plotArgs ->
        try
            execute plotArgs
            SuccessExitCode
        with ex ->
            eprintfn "%s" (ex.ToString())
            RuntimeErrorExitCode

let internal runWithArgs args =
    runWithArgsUsing showPlot args

/// <summary>
/// Application entry point. Parses arguments and generates the specified plot type.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>Exit code (0 for success).</returns>
[<EntryPoint>]
let main args =
    runWithArgs args
