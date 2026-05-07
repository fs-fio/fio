namespace FIO.Benchmarks.Plots

open Plotly.NET

/// <summary>Represents the supported plot types for benchmark visualization.</summary>
type PlotType =
    /// <summary>Represents a box plot showing distribution of execution times.</summary>
    | BoxPlot
    /// <summary>Represents a line plot showing execution time trends.</summary>
    | LinePlot

/// <summary>Provides conversion functions for plot types.</summary>
module internal PlotType =

    /// <summary>Returns the string representation of a plot type.</summary>
    /// <param name="plotType">The plot type to convert.</param>
    /// <returns>The name of the plot type as a string.</returns>
    let toString =
        function
        | BoxPlot -> "BoxPlot"
        | LinePlot -> "LinePlot"

/// <summary>Represents the arguments controlling plot generation including type and data path.</summary>
type PlotArgs =
    {
        /// <summary>Represents the type of plot to create.</summary>
        PlotType: PlotType
        /// <summary>Represents the path to load benchmark data files from.</summary>
        LoadPath: string
    }

/// <summary>Provides functions for creating and displaying benchmark charts.</summary>
module internal PlotArgs =

    /// <summary>Creates a list of pastel purple color variations for chart styling.</summary>
    /// <param name="count">The number of colors to create.</param>
    /// <returns>A list of RGB color strings.</returns>
    let private generatePastelPurples (count: int) : string list =
        if count <= 0 then
            []
        else
            let baseR, baseG, baseB = 147, 112, 219
            let stepSize = 15

            let generateBalancedDeltas (n: int) : int list =
                if n = 1 then
                    [ 0 ]
                else
                    let steps = [ for i in 1 .. (n / 2) -> stepSize * i ]
                    let negatives = List.map (~-) steps
                    let positives = steps
                    let middle = if n % 2 = 1 then [ 0 ] else []
                    negatives @ middle @ positives

            let deltas = generateBalancedDeltas count
            let clamp value = max 0 (min 255 value)

            let createPurpleVariant index =
                let rDelta = deltas[index % deltas.Length]
                let gDelta = deltas[(index + 1) % deltas.Length]
                let bDelta = deltas[(index + 2) % deltas.Length]

                let r = clamp (baseR + rDelta)
                let g = clamp (baseG + gDelta)
                let b = clamp (baseB + bDelta)

                $"rgb({r},{g},{b})"

            List.init count createPurpleVariant

    /// <summary>Returns predicates for ordering benchmarks in display order.</summary>
    /// <param name="enablePingpong">Whether to include the pingpong benchmark predicate.</param>
    /// <returns>A list of predicates for benchmark ordering.</returns>
    let private groupPredicates enablePingpong =
        [
            if enablePingpong then
                yield
                    fun d ->
                        d
                        |> List.head
                        |> fst
                        |> fun fm -> fm.BenchmarkName.ToLowerInvariant() = "pingpong"
            yield
                fun d ->
                    d
                    |> List.head
                    |> fst
                    |> fun fm -> fm.BenchmarkName.ToLowerInvariant() = "threadring"
            yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant() = "big"
            yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant() = "bang"
            yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant() = "fork"
        ]

    /// <summary>Returns items reordered by matching against a list of predicates.</summary>
    /// <param name="predicates">The predicates to match against in order.</param>
    /// <param name="items">The items to reorder.</param>
    /// <returns>The reordered list of items.</returns>
    let private reorderByPredicates predicates items =
        let rec loop preds remaining acc =
            match preds with
            | [] -> List.rev acc
            | pred :: rest ->
                match List.tryFind pred remaining with
                | Some matchItem ->
                    let remaining' = List.filter ((<>) matchItem) remaining
                    loop rest remaining' (matchItem :: acc)
                | None -> failwith "No matching item found for predicate"

        loop predicates items []

    /// <summary>Creates box plot charts from benchmark data files.</summary>
    /// <param name="path">The directory path containing benchmark data.</param>
    /// <param name="boxPlotWidth">The width per box in pixels.</param>
    /// <param name="plotHeight">The chart height in pixels.</param>
    /// <returns>A tuple of row count, column count, titles, and charts.</returns>
    let private generateBoxPlotCharts path boxPlotWidth plotHeight =
        let boxplotData =
            reorderByPredicates (groupPredicates true) <| CsvResults.getAll path

        let colors = generatePastelPurples boxplotData.Length
        let rowCount, colCount = boxplotData.Length + 1, 1

        let titles, charts =
            List.map
                (fun (innerList, color) ->
                    let metadata: FileMetadata = innerList |> List.head |> fst
                    let plotWidth = innerList.Length * boxPlotWidth
                    FileMetadata.title metadata, Charts.boxPlot innerList plotWidth plotHeight color)
                (List.zip boxplotData colors)
            |> List.unzip

        rowCount, colCount, titles, charts

    /// <summary>Creates line plot charts from benchmark data files.</summary>
    /// <param name="path">The directory path containing benchmark data.</param>
    /// <param name="linePlotWidth">The width per line in pixels.</param>
    /// <param name="plotHeight">The chart height in pixels.</param>
    /// <returns>A tuple of row count, column count, titles, and charts.</returns>
    let private generateLineCharts path linePlotWidth plotHeight =
        let rawLineData =
            CsvResults.getAll path
            |> List.collect id
            |> List.groupBy (fst >> _.BenchmarkName)

        let reorderedGroups =
            rawLineData
            |> List.map snd // discard benchmark name keys
            |> reorderByPredicates (groupPredicates false)

        let linePlotData =
            reorderedGroups
            |> List.map (fun group ->
                group
                |> List.groupBy (fst >> _.RuntimeName)
                |> List.map (snd >> List.sortBy (fst >> _.ActorCount)))

        let rowCount, colCount = linePlotData.Length + 1, 1

        let colors =
            let colors = generatePastelPurples linePlotData.Length
            let first = List.head colors
            let last = List.last colors
            let middle = colors.[List.length colors / 2]
            [ first; middle; last ]

        let titles, charts =
            List.map
                (fun innerList ->
                    let metadata: FileMetadata = innerList |> List.head |> List.head |> fst
                    let plotWidth = innerList.Length * linePlotWidth
                    metadata.BenchmarkName, Charts.linePlot innerList plotWidth plotHeight colors)
                linePlotData
            |> List.unzip

        rowCount, colCount, titles, charts

    /// <summary>Creates and displays benchmark charts based on the provided plot arguments.</summary>
    /// <param name="args">The plot arguments specifying type and data path.</param>
    let show (args: PlotArgs) =
        let rowCount, colCount, titles, charts =
            match args.PlotType with
            | BoxPlot -> generateBoxPlotCharts args.LoadPath 100 6000
            | LinePlot -> generateLineCharts args.LoadPath 350 1800

        Chart.Grid (rowCount, colCount, SubPlotTitles = titles) charts |> Chart.show
