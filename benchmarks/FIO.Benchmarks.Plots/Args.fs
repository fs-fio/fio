/// <summary>Provides command-line argument parsing for plot generation.</summary>
module internal FIO.Benchmarks.Plots.Args

open Argu

open System
open System.IO

/// <summary>Returns the default folder name for box plot data files.</summary>
/// <returns>The default box plot folder name.</returns>
let defaultBoxPlotFolder = "boxplot_data"

/// <summary>Returns the default folder name for line plot data files.</summary>
/// <returns>The default line plot folder name.</returns>
let defaultLinePlotFolder = "lineplot_data"

/// <summary>Represents the supported command-line arguments.</summary>
type private Arguments =
    /// <summary>Represents the box plot visualization option.</summary>
    | [<Unique; AltCommandLine("-b")>] BoxPlot
    /// <summary>Represents the line plot visualization option.</summary>
    | [<Unique; AltCommandLine("-l")>] LinePlot
    /// <summary>Represents the path to load benchmark data files from.</summary>
    | [<Unique; AltCommandLine("-p")>] LoadPath of absolutePath: string

    interface IArgParserTemplate with
        /// <summary>Returns the usage description for this argument.</summary>
        /// <returns>The usage text string.</returns>
        member this.Usage =
            match this with
            | BoxPlot -> "generate box plot"
            | LinePlot -> "generate line plot"
            | LoadPath _ ->
                "specify absolute path to load benchmark data files from (default uses benchmark subfolder in current directory)"

/// <summary>Represents the outcome of CLI argument parsing.</summary>
type internal ParseOutcome =
    /// <summary>Represents a successful parse with validated plot arguments.</summary>
    | Parsed of PlotArgs
    /// <summary>Represents a help request containing usage text to display.</summary>
    | HelpRequested of usageText: string
    /// <summary>Represents invalid arguments containing error and usage text.</summary>
    | InvalidArgs of errorText: string * usageText: string

/// <summary>Returns the argument parser for plot command-line arguments.</summary>
/// <returns>The configured argument parser.</returns>
let private parser =
    ArgumentParser.Create<Arguments>(programName = "FIO.Benchmarks.Plots")

/// <summary>Returns the usage text for the plot CLI.</summary>
/// <returns>A formatted usage text string.</returns>
let internal usageText () = parser.PrintUsage()

/// <summary>Creates an invalid-args parse outcome with the current usage text.</summary>
/// <param name="errorText">The error message describing the validation failure.</param>
/// <returns>An invalid-args parse outcome.</returns>
let private invalid errorText = InvalidArgs(errorText, usageText ())

/// <summary>Returns a concise error summary from an Argu parse exception message.</summary>
/// <param name="message">The raw exception message from Argu.</param>
/// <returns>The first line of the error summary, or a default message.</returns>
let private parseErrorSummary (message: string) =
    if String.IsNullOrWhiteSpace message then
        "Invalid command line arguments."
    else
        let usageIndex = message.IndexOf("USAGE:", StringComparison.OrdinalIgnoreCase)

        let rawSummary =
            if usageIndex > 0 then
                message.Substring(0, usageIndex).Trim()
            else
                message.Trim()

        if String.IsNullOrWhiteSpace rawSummary then
            "Invalid command line arguments."
        else
            rawSummary.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.head

/// <summary>Builds and validates plot arguments from parsed command-line results.</summary>
/// <param name="results">The parsed command-line arguments.</param>
/// <returns>Ok with validated plot arguments, or Error with a validation message.</returns>
let private buildPlotArgs (results: ParseResults<Arguments>) : Result<PlotArgs, string> =
    let boxPlotSelected = results.Contains BoxPlot
    let linePlotSelected = results.Contains LinePlot

    let plotTypeResult =
        match boxPlotSelected, linePlotSelected with
        | true, false -> Ok PlotType.BoxPlot
        | false, true -> Ok PlotType.LinePlot
        | false, false -> Error "Specify exactly one plot type: --boxplot or --lineplot."
        | true, true -> Error "Specify only one plot type: --boxplot or --lineplot."

    match plotTypeResult with
    | Error err -> Error err
    | Ok plotType ->
        let loadPath =
            match results.TryGetResult LoadPath with
            | Some path when not (Path.IsPathFullyQualified path) -> Error "--loadpath must be an absolute path."
            | Some path -> Ok path
            | None ->
                let subfolder =
                    match plotType with
                    | PlotType.BoxPlot -> defaultBoxPlotFolder
                    | PlotType.LinePlot -> defaultLinePlotFolder

                Ok(Path.Combine(Directory.GetCurrentDirectory(), subfolder))

        match loadPath with
        | Error err -> Error err
        | Ok absolutePath -> Ok { PlotType = plotType; LoadPath = absolutePath }

/// <summary>Returns a parse outcome from command-line arguments.</summary>
/// <param name="args">The command-line arguments to parse.</param>
/// <returns>A parse outcome indicating success, help request, or invalid arguments.</returns>
let parse args =
    try
        let results = parser.Parse(args, raiseOnUsage = false)

        if results.IsUsageRequested then
            HelpRequested(usageText ())
        else
            match buildPlotArgs results with
            | Ok plotArgs -> Parsed plotArgs
            | Error errorText -> invalid errorText
    with :? ArguParseException as ex ->
        invalid (parseErrorSummary ex.Message)
