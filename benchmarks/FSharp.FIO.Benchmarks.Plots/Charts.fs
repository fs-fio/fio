/// <summary>
/// Chart generation functions using Plotly.NET for benchmark visualization.
/// </summary>
module internal FSharp.FIO.Benchmarks.Plots.Charts

open Plotly.NET
open Plotly.NET.TraceObjects
open Plotly.NET.LayoutObjects

/// <summary>
/// Default layout styling for all charts.
/// </summary>
let private defaultLayout = Layout.init (
    PlotBGColor = Color.fromHex "#F8F8F8",
    PaperBGColor = Color.fromHex "#FFFFFF"
)

/// <summary>
/// Index of the execution time column in CSV headers.
/// </summary>
let private executionTimeHeaderIndex = 1

/// <summary>
/// Generates a box plot comparing execution times across runtimes.
/// </summary>
/// <param name="data">List of file metadata and benchmark data pairs.</param>
/// <param name="width">Chart width in pixels.</param>
/// <param name="height">Chart height in pixels.</param>
/// <param name="color">Color for the box plot markers.</param>
/// <returns>Combined box plot chart.</returns>
let boxPlot (data: (FileMetadata * BenchmarkData) list) (width: int) (height: int) color =

    let createChart (data: FileMetadata * BenchmarkData) =
        let metadata, data = data
        let x = List.replicate data.ExecutionTimes.Length (metadata.ToString ())
        let y = data.ExecutionTimes
        let yAxisLabel = data.Headers[executionTimeHeaderIndex] + " (lower is better)"
        Chart.BoxPlot(
            X = x,
            Y = y,
            ShowLegend = false,
            MarkerColor = Color.fromString color,
            BoxPoints = StyleParam.BoxPoints.SuspectedOutliers)
        |> Chart.withTraceInfo (metadata.ToString ())
        |> Chart.withSize (width, height)
        |> Chart.withXAxis (LinearAxis.init(TickAngle = 65))
        |> Chart.withYAxisStyle yAxisLabel
        |> Chart.withLayout defaultLayout

    Chart.combine
    <| List.map createChart data

/// <summary>
/// Generates a line plot showing execution time trends across actor counts.
/// </summary>
/// <param name="data">Nested list of file metadata and benchmark data grouped by runtime.</param>
/// <param name="width">Chart width in pixels.</param>
/// <param name="height">Chart height in pixels.</param>
/// <param name="colors">List of colors for each line.</param>
/// <returns>Combined line plot chart.</returns>
let linePlot (data: (FileMetadata * BenchmarkData) list list) (width: int) (height: int) colors =

    let createChart (data: (FileMetadata * BenchmarkData) list) color dash =
        let x = List.map (fun x -> (fst x).ActorCount) data
        let y = List.map (fun x -> (snd x).ExecutionTimes |> List.averageBy float) data
        let yAxisLabel = (snd data.Head).Headers[executionTimeHeaderIndex] + " (lower is better)"
        Chart.Scatter(
            x,
            y,
            StyleParam.Mode.Lines_Markers,
            Marker = Marker.init (Color = Color.fromString color),
            Line = Line.init (Color = Color.fromString color, Dash = dash),
            ShowLegend = true)
        |> Chart.withTraceInfo ((fst data.Head).ToString ())
        |> Chart.withSize (width, height)
        |> Chart.withXAxisStyle "Forked Fibers"
        |> Chart.withYAxisStyle yAxisLabel
        |> Chart.withLayout defaultLayout
        |> Chart.withLegend (
            Legend.init (
                Orientation = StyleParam.Orientation.Vertical,
                X = -0.05,
                Y = 1.2,
                XAnchor = StyleParam.XAnchorPosition.Left,
                YAnchor = StyleParam.YAnchorPosition.Top))

    let dashes = [StyleParam.DrawingStyle.Solid; StyleParam.DrawingStyle.LongDash; StyleParam.DrawingStyle.LongDashDot]
    Chart.combine
    <| List.map (fun (data, color, style) -> createChart data color style) (List.zip3 data colors dashes)
