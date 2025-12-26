module private FSharp.FIO.Benchmarks.Plots.Program

open FSharp.FIO.Benchmarks.Plots.ArgParser
open FSharp.FIO.Benchmarks.Plots.ChartMaker

[<EntryPoint>]
let main args =
    printArgs args
    createAndShowCharts
    <| parseArgs args
    0
