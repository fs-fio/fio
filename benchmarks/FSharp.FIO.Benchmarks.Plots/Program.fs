(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

module private FSharp.FIO.Benchmarks.Plots.Program

open FSharp.FIO.Benchmarks.Plots.ArgParser
open FSharp.FIO.Benchmarks.Plots.ChartMaker

[<EntryPoint>]
let main args =
    printArgs args
    createAndShowCharts
    <| parseArgs args
    0
