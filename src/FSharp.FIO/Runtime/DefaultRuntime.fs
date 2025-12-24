(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

namespace FSharp.FIO.Runtime.Default

/// <summary>
/// The default and recommended runtime for FIO applications.
/// This is an alias for <see cref="FSharp.FIO.Runtime.Concurrent.ConcurrentRuntime"/>,
/// which provides the best performance for highly concurrent workloads with constant-time
/// blocked fiber handling using an event-driven architecture.
/// </summary>
/// <remarks>
/// Use this runtime unless you have specific requirements for DirectRuntime (simpler .NET Task-based)
/// or CooperativeRuntime (linear-time blocked fiber handling).
/// </remarks>
type DefaultRuntime = FSharp.FIO.Runtime.Concurrent.ConcurrentRuntime
