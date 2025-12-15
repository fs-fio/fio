(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module internal FSharp.FIO.Benchmarks.Plots.ArgParser

open FSharp.FIO.Benchmarks.Plots.ChartMaker

open Argu
open System.IO

let DefaultBoxPlotFolder = "boxplot_data"
let DefaultLinePlotFolder = "lineplot_data"

type private Arguments =
    | [<Unique; AltCommandLine("-b")>] BoxPlot
    | [<Unique; AltCommandLine("-l")>] LinePlot
    | [<AltCommandLine("-p")>] LoadPath of absolutePath: string
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | BoxPlot -> "Generate box plot"
            | LinePlot -> "Generate line plot"
            | LoadPath _ -> "Specify path to load benchmark data files from (default is 'boxplot_data' or 'lineplot_data' in the current directory)."

let private parser =
    ArgumentParser.Create<Arguments> (programName = "FSharp.FIO.Benchmarks.Plots")

let printUsage () =
    parser.PrintUsage ()
    |> printfn "%s"

let printArgs args =
    printfn "%s arguments: %s" parser.ProgramName (String.concat " " args)

let parseArgs args =
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
                | PlotType.BoxPlot -> DefaultBoxPlotFolder
                | PlotType.LinePlot -> DefaultLinePlotFolder
            Path.Combine(Directory.GetCurrentDirectory(), subfolder)
    
    { PlotType = plotType; LoadPath = loadPath }
