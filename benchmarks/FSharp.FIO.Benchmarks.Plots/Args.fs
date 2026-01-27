/// <summary>
/// Command-line argument parsing for plot generation.
/// </summary>
module internal FSharp.FIO.Benchmarks.Plots.Args

open Argu
open System.IO

/// <summary>
/// Default folder name for box plot data files.
/// </summary>
let defaultBoxPlotFolder = "boxplot_data"

/// <summary>
/// Default folder name for line plot data files.
/// </summary>
let defaultLinePlotFolder = "lineplot_data"

/// <summary>
/// Discriminated union representing supported command-line arguments.
/// </summary>
type private Arguments =
    /// <summary>Generate box plot visualization.</summary>
    | [<Unique; AltCommandLine("-b")>] BoxPlot
    /// <summary>Generate line plot visualization.</summary>
    | [<Unique; AltCommandLine("-l")>] LinePlot
    /// <summary>Path to load benchmark data files from.</summary>
    | [<AltCommandLine("-p")>] LoadPath of absolutePath: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | BoxPlot -> "Generate box plot"
            | LinePlot -> "Generate line plot"
            | LoadPath _ -> "Specify path to load benchmark data files from (default is 'boxplot_data' or 'lineplot_data' in the current directory)."

let private parser =
    ArgumentParser.Create<Arguments> (programName = "FSharp.FIO.Benchmarks.Plots")

/// <summary>
/// Prints the argument parser usage information.
/// </summary>
let printUsage () =
    parser.PrintUsage ()
    |> printfn "%s"

/// <summary>
/// Prints the program name and provided arguments.
/// </summary>
/// <param name="args">Command-line arguments to print.</param>
let print args =
    printfn "%s arguments: %s" parser.ProgramName (String.concat " " args)

/// <summary>
/// Parses command-line arguments into a PlotArgs configuration.
/// </summary>
/// <param name="args">Command-line arguments to parse.</param>
/// <returns>Parsed plot arguments configuration.</returns>
let parse args =
    let results = parser.Parse args

    let plotType =
        match results.Contains BoxPlot, results.Contains LinePlot with
        | true, false -> PlotType.BoxPlot
        | false, true -> PlotType.LinePlot
        | false, false -> invalidArg  (nameof args) "You must specify one of: --boxplot or --lineplot."
        | true, true -> invalidArg  (nameof args) "Only one plot type can be specified at a time."

    let loadPath =
        match results.TryGetResult LoadPath with
        | Some path -> path
        | None ->
            let subfolder =
                match plotType with
                | PlotType.BoxPlot -> defaultBoxPlotFolder
                | PlotType.LinePlot -> defaultLinePlotFolder
            Path.Combine(Directory.GetCurrentDirectory(), subfolder)

    { PlotType = plotType; LoadPath = loadPath }
