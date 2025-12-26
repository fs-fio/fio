module internal FSharp.FIO.Benchmarks.Plots.ChartMaker

open FSharp.FIO.Benchmarks.Plots.Charts
open FSharp.FIO.Benchmarks.Plots.DataParser

open Plotly.NET
    
type PlotType =
    | BoxPlot
    | LinePlot
    
type PlotArgs = {
    PlotType : PlotType
    LoadPath : string
}

let private generatePastelPurples (count: int) : string list =
    if count <= 0 then []
    else
        let baseR, baseG, baseB = 147, 112, 219
        let stepSize = 15
        
        let generateBalancedDeltas (n: int) : int list =
            if n = 1 then [0]
            else
                let steps = [ for i in 1 .. (n / 2) -> stepSize * i ]
                let negatives = List.map (~-) steps
                let positives = steps
                let middle = if n % 2 = 1 then [0] else []
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

let groupPredicates enablePingpong =
    [
        if enablePingpong then
            yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant () = "pingpong"
        yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant () = "threadring"
        yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant () = "big"
        yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant () = "bang"
        yield fun d -> d |> List.head |> fst |> fun fm -> fm.BenchmarkName.ToLowerInvariant () = "fork"
    ]

let reorderByPredicates predicates items =
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

let private generateBoxPlotCharts path boxPlotWidth plotHeight =
    let boxplotData =
        reorderByPredicates (groupPredicates true)
        <| getAllCsvResults path
    let colors = generatePastelPurples boxplotData.Length
    let rowCount, colCount =
        boxplotData.Length + 1, 1
    
    let titles, charts =
        List.map (fun (innerList, color) ->
            let metadata: FileMetadata = innerList |> List.head |> fst
            let plotWidth = innerList.Length * boxPlotWidth
            metadata.Title(), createBoxPlot innerList plotWidth plotHeight color) (List.zip boxplotData colors)
            |> List.unzip
            
    rowCount, colCount, titles, charts
    
let private generateLineCharts path linePlotWidth plotHeight =
    let rawLineData =
        getAllCsvResults path
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
                                       
    let rowCount, colCount =
        linePlotData.Length + 1, 1

    let colors  =
        let colors = generatePastelPurples linePlotData.Length
        let first = List.head colors
        let last = List.last colors
        let middle = colors.[List.length colors / 2]
        [first; middle; last]

    let titles, charts =
        List.map (fun innerList ->
            let metadata: FileMetadata = innerList |> List.head |> List.head |> fst
            let plotWidth = innerList.Length * linePlotWidth
            metadata.BenchmarkName, createLinePlot innerList plotWidth plotHeight colors) linePlotData
            |> List.unzip
            
    rowCount, colCount, titles, charts
    
let createAndShowCharts args =
    let rowCount, colCount, titles, charts =
        match args.PlotType with
        | BoxPlot -> generateBoxPlotCharts args.LoadPath 100 6000
        | LinePlot -> generateLineCharts args.LoadPath 350 1800
    Chart.Grid (rowCount, colCount, SubPlotTitles = titles) charts
    |> Chart.show
