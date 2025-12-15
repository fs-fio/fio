(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional, effectful, and type-safe abstractions over .NET TCP sockets for FIO.
/// Includes the FSocket type and related combinators for connecting, sending, receiving, and managing socket state in a purely functional style.
/// </summary>
module FSharp.FIO.Lib.Net.Sockets

open FSharp.FIO.DSL

open System
open System.IO
open System.Net
open System.Text.Json
open System.Net.Sockets

/// <summary>
/// Represents a functional, effectful, and type-safe abstraction over a .NET TCP socket.
/// <para>
/// Provides FIO-based methods for connecting, sending and receiving typed messages (with JSON serialization), disconnecting, closing,
/// and querying socket state and endpoints.
/// </para>
/// <para>
/// All operations are asynchronous, non-blocking, and return FIO effects, enabling composable and safe TCP networking in a purely functional style.
/// </para>
/// </summary>
type FSocket<'S, 'R, 'E> private (socket: Socket, reader: StreamReader, writer: StreamWriter, networkStream: NetworkStream, onError: exn -> 'E, options: JsonSerializerOptions) =

    let mutable disposed = false

    // Partially applied function as it is the same
    // onError function used everywhere in the type
    let ( !<<< ) (func: unit -> 'T) : FIO<'T, 'E> =
        !<<< func onError

    /// <summary>
    /// Disposes the socket and all associated resources.
    /// </summary>
    member _.Dispose() =
        if not disposed then
            disposed <- true
            try reader.Dispose() with _ -> ()
            try writer.Dispose() with _ -> ()
            try networkStream.Dispose() with _ -> ()
            try socket.Dispose() with _ -> ()

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()

    /// <summary>
    /// Disposes the socket and all associated resources as an FIO effect.
    /// </summary>
    /// <returns>An FIO effect that disposes the socket.</returns>
    member this.DisposeAsync<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> this.Dispose())
        }

    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, with custom error handling and JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! networkStream = !<<< (fun () -> new NetworkStream (socket)) onError
            let! writer = !<<< (fun () -> new StreamWriter (networkStream)) onError
            let! reader = !<<< (fun () -> new StreamReader (networkStream)) onError
            do! !<<< (fun () -> writer.AutoFlush <- true) onError
            return new FSocket<'S, 'R, 'E> (socket, reader, writer, networkStream, onError, options)
        }

    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, using default error handling and JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, id, options)

    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, with custom error handling and default JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, onError: exn -> 'E) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        FSocket.Create<'S, 'R, 'E> (socket, onError, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, using default error handling and default JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, id, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, connecting to the specified host and port, with custom error handling and JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int, onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        fio {
            do! !<<< (fun () -> socket.Connect(host, port)) onError
            return! FSocket.Create<'S, 'R, 'E> (socket, onError, options)
        }
        
    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, connecting to the specified host and port, using default error handling and JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="options">The JSON serializer options to use for message serialization.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, host, port, id, options)
        
    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, connecting to the specified host and port, with custom error handling and default JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="onError">A function to map exceptions to the error type 'E.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int, onError: exn -> 'E) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        FSocket.Create<'S, 'R, 'E> (socket, host, port, onError, JsonSerializerOptions())
        
    /// <summary>
    /// Creates a new functional socket abstraction from an existing .NET Socket, connecting to the specified host and port, using default error handling and default JSON serialization options.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <returns>A new FSocket instance wrapped in an FIO effect.</returns>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, host, port, id, JsonSerializerOptions())

    /// <summary>
    /// Sends a value of type 'S over the socket as a JSON-serialized string.
    /// </summary>
    /// <typeparam name="S">The type of the message to send.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="msg">The value to send.</param>
    /// <returns>An FIO effect that sends the value over the socket or returns an error of type 'E.</returns>
    member _.Send<'S, 'E> (msg: 'S) : FIO<unit, 'E> =
        fio {
            let! json = !<<< (fun () -> JsonSerializer.Serialize(msg, options))
            do! !<<< (fun () -> writer.WriteLine json)
            do! !<<< writer.Flush
        }

    /// <summary>
    /// Receives a value of type 'R from the socket, deserializing from JSON.
    /// </summary>
    /// <typeparam name="R">The type of the message to receive.</typeparam>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that receives a value from the socket or returns an error of type 'E.</returns>
    member _.Receive<'R, 'E> () : FIO<'R, 'E> =
        fio {
            let! json = !<<< reader.ReadLine
            match json with
            | null ->
                return! FIO.Fail (onError (InvalidOperationException "Connection closed: ReadLine returned null"))
            | jsonStr ->
                let! msg = !<<< (fun () -> JsonSerializer.Deserialize<'R>(jsonStr, options))
                return msg
        }
    
    /// <summary>
    /// Disconnects the socket, optionally allowing reuse.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="reuseSocket">Whether to reuse the socket after disconnecting.</param>
    /// <returns>An FIO effect that disconnects the socket or returns an error of type 'E.</returns>
    member _.Disconnect<'E> (reuseSocket: bool) : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> socket.Disconnect reuseSocket)
        }

    /// <summary>
    /// Disconnects the socket and allows reuse by default.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that disconnects the socket or returns an error of type 'E.</returns>
    member this.Disconnect<'E> () : FIO<unit, 'E> =
        this.Disconnect<'E> true

    /// <summary>
    /// Closes the socket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that closes the socket or returns an error of type 'E.</returns>
    member _.Close<'E> () : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> socket.Close())
        }
    
    /// <summary>
    /// Gets the remote endpoint of the socket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the remote endpoint or returns an error of type 'E.</returns>
    member _.RemoteEndPoint<'E> () : FIO<EndPoint, 'E> =
        fio {
            return! !<<< (fun () -> socket.RemoteEndPoint)
        }
    
    /// <summary>
    /// Gets the address family of the socket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the address family or returns an error of type 'E.</returns>
    member _.AddressFamily : FIO<AddressFamily, 'E> =
        fio {
            return! !<<< (fun () -> socket.AddressFamily)
        }

    /// <summary>
    /// Returns whether the socket is currently connected.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that returns whether the socket is connected or returns an error of type 'E.</returns>
    member _.Connected<'E> () : FIO<bool, 'E> =
        fio {
            return! !<<< (fun () -> socket.Connected)
        }

    /// <summary>
    /// Returns the local endpoint of the socket.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <returns>An FIO effect that gets the local endpoint or returns an error of type 'E.</returns>
    member _.LocalEndPoint<'E> () : FIO<EndPoint, 'E> =
        fio {
            return! !<<< (fun () -> socket.LocalEndPoint)
        }

    /// <summary>
    /// Shuts down the socket for sending, receiving, or both.
    /// </summary>
    /// <typeparam name="E">The error type.</typeparam>
    /// <param name="how">The type of shutdown to perform.</param>
    /// <returns>An FIO effect that shuts down the socket or returns an error of type 'E.</returns>
    member _.Shutdown<'E> (how: SocketShutdown) : FIO<unit, 'E> =
        fio {
            do! !<<< (fun () -> socket.Shutdown how)
        }
