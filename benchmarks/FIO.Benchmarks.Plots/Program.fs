/// <summary>
/// Entry point for the FIO benchmark plotting application.
/// </summary>
module private FIO.Benchmarks.Plots.Program

open FIO.Benchmarks.Plots

/// <summary>Exit code returned on successful execution.</summary>
[<Literal>]
let private SuccessExitCode = 0

/// <summary>Exit code returned when a runtime error occurs during plot generation.</summary>
[<Literal>]
let private RuntimeErrorExitCode = 1

/// <summary>Exit code returned when command-line arguments are invalid.</summary>
[<Literal>]
let private InvalidArgsExitCode = 2

/// <summary>
/// Generates and displays the plot for the given arguments.
/// </summary>
/// <param name="plotArgs">Parsed plot arguments.</param>
let private showPlot plotArgs = PlotArgs.show plotArgs

/// <summary>
/// Parses command-line arguments and generates the plot using a custom executor function.
/// </summary>
/// <param name="execute">Function to execute with parsed plot arguments.</param>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Exit code indicating success or failure.</returns>
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

/// <summary>
/// Parses command-line arguments and generates the plot.
/// </summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Exit code indicating success or failure.</returns>
let internal runWithArgs args = runWithArgsUsing showPlot args

/// <summary>
/// Application entry point. Parses arguments and generates the specified plot type.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>Exit code (0 for success).</returns>
[<EntryPoint>]
let main args = runWithArgs args
