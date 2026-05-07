/// <summary>Provides the entry point for the FIO benchmark plotting application.</summary>
module private FIO.Benchmarks.Plots.Program

open FIO.Benchmarks.Plots

/// <summary>Returns the exit code for successful execution.</summary>
/// <returns>The success exit code (0).</returns>
[<Literal>]
let private SuccessExitCode = 0

/// <summary>Returns the exit code for runtime errors during plot generation.</summary>
/// <returns>The runtime error exit code (1).</returns>
[<Literal>]
let private RuntimeErrorExitCode = 1

/// <summary>Returns the exit code for invalid command-line arguments.</summary>
/// <returns>The invalid arguments exit code (2).</returns>
[<Literal>]
let private InvalidArgsExitCode = 2

/// <summary>Creates and displays the plot for the given arguments.</summary>
/// <param name="plotArgs">The parsed plot arguments.</param>
let private showPlot plotArgs = PlotArgs.show plotArgs

/// <summary>Returns an exit code after parsing command-line arguments and executing the plot generation with a custom executor.</summary>
/// <param name="execute">The function to execute with parsed plot arguments.</param>
/// <param name="args">The command-line arguments to parse.</param>
/// <returns>An exit code indicating success or failure.</returns>
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

/// <summary>Returns an exit code after parsing command-line arguments and executing the plot generation.</summary>
/// <param name="args">The command-line arguments to parse.</param>
/// <returns>An exit code indicating success or failure.</returns>
let internal runWithArgs args = runWithArgsUsing showPlot args

/// <summary>Returns an exit code after parsing arguments and creating the specified plot type.</summary>
/// <param name="args">The command-line arguments.</param>
/// <returns>An exit code indicating success or failure.</returns>
[<EntryPoint>]
let main args = runWithArgs args
