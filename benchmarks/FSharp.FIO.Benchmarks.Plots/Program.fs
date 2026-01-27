/// <summary>
/// Entry point for the FIO benchmark plotting application.
/// </summary>
module private FSharp.FIO.Benchmarks.Plots.Program

open FSharp.FIO.Benchmarks.Plots

/// <summary>
/// Application entry point. Parses arguments and generates the specified plot type.
/// </summary>
/// <param name="args">Command-line arguments.</param>
/// <returns>Exit code (0 for success).</returns>
[<EntryPoint>]
let main args =
    Args.print args
    PlotArgs.show
    <| Args.parse args
    0
