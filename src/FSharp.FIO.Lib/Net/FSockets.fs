(*********************************************************************************************)
(* FIO - A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#     *)
(* Copyright (c) 2022-2026 - Daniel Larsen and Technical University of Denmark (DTU)         *)
(* All rights reserved                                                                       *)
(*********************************************************************************************)

/// <summary>
/// Provides functional, effectful, and type-safe abstractions over .NET TCP Sockets for FIO.
/// </summary>
module FSharp.FIO.Lib.Net.FSockets

open FSharp.FIO.DSL

open System
open System.IO
open System.Net
open System.Text.Json
open System.Net.Sockets

/// <summary>
/// A functional, effectful, and type-safe abstraction over a .NET TCP Socket.
/// </summary>
type FSocket<'S, 'R, 'E> private (socket: Socket, reader: StreamReader, writer: StreamWriter, networkStream: NetworkStream, onError: exn -> 'E, options: JsonSerializerOptions) =

    let mutable disposed = false

    // Partially applied function as it is the same
    // onError function used everywhere in the type
    let fromFunc (func: unit -> 'T) : FIO<'T, 'E> =
        FIO.FromFunc (func, onError)

    /// <summary>
    /// Disposes the socket and all associated resources.
    /// </summary>
    member _.Dispose () =
        if not disposed then
            disposed <- true
            try reader.Dispose () with _ -> ()
            try writer.Dispose () with _ -> ()
            try networkStream.Dispose () with _ -> ()
            try socket.Dispose () with _ -> ()

    interface IDisposable with
        member this.Dispose () = this.Dispose ()

    /// <summary>
    /// Disposes the socket and all associated resources as an FIO effect.
    /// </summary>
    member this.DisposeAsync<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> this.Dispose ()
        }

    /// <summary>
    /// Creates a new functional socket from an existing .NET Socket with custom error handling and JSON options.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        fio {
            let! networkStream = FIO.FromFunc ((fun () -> new NetworkStream (socket)), onError)
            let! writer = FIO.FromFunc ((fun _ -> new StreamWriter (networkStream)), onError)
            let! reader = FIO.FromFunc ((fun () -> new StreamReader (networkStream)), onError)
            do! FIO.FromFunc ((fun () -> writer.AutoFlush <- true), onError)
            return new FSocket<'S, 'R, 'E> (socket, reader, writer, networkStream, onError, options)
        }

    /// <summary>
    /// Creates a new functional socket from an existing .NET Socket with default error handling.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, id, options)

    /// <summary>
    /// Creates a new functional socket from an existing .NET Socket with custom error handling.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, onError: exn -> 'E) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        FSocket.Create<'S, 'R, 'E> (socket, onError, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional socket from an existing .NET Socket with default settings.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    static member Create<'S, 'R, 'E> (socket: Socket) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, id, JsonSerializerOptions())

    /// <summary>
    /// Creates a new functional socket and connects to the specified host and port.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int, onError: exn -> 'E, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        fio {
            do! FIO.FromFunc ((fun () -> socket.Connect(host, port)), onError)
            return! FSocket.Create<'S, 'R, 'E> (socket, onError, options)
        }
        
    /// <summary>
    /// Creates a new functional socket and connects to the specified host and port with default error handling.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="options">The JSON serializer options.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int, options: JsonSerializerOptions) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, host, port, id, options)
        
    /// <summary>
    /// Creates a new functional socket and connects to the specified host and port with custom error handling.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    /// <param name="onError">A function to map exceptions to the error type.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int, onError: exn -> 'E) : FIO<FSocket<'S, 'R, 'E>, 'E> =
        FSocket.Create<'S, 'R, 'E> (socket, host, port, onError, JsonSerializerOptions())
        
    /// <summary>
    /// Creates a new functional socket and connects to the specified host and port with default settings.
    /// </summary>
    /// <param name="socket">The underlying .NET Socket instance.</param>
    /// <param name="host">The remote host to connect to.</param>
    /// <param name="port">The remote port to connect to.</param>
    static member Create<'S, 'R, 'E> (socket: Socket, host: string, port: int) : FIO<FSocket<'S, 'R, exn>, exn> =
        FSocket.Create<'S, 'R, exn> (socket, host, port, id, JsonSerializerOptions())

    /// <summary>
    /// Sends a value over the socket as a JSON-serialized string.
    /// </summary>
    /// <param name="msg">The value to send.</param>
    member _.Send<'S, 'E> (msg: 'S) : FIO<unit, 'E> =
        fio {
            let! json = fromFunc <| fun () -> JsonSerializer.Serialize(msg, options)
            do! fromFunc <| fun () -> writer.WriteLine json
            do! fromFunc writer.Flush
        }

    /// <summary>
    /// Receives a value from the socket, deserializing from JSON.
    /// </summary>
    member _.Receive<'R, 'E> () : FIO<'R, 'E> =
        fio {
            let! json = fromFunc reader.ReadLine
            match json with
            | null ->
                return! FIO.Fail (onError (InvalidOperationException "Connection closed: ReadLine returned null"))
            | jsonStr ->
                let! msg = fromFunc <| fun () -> JsonSerializer.Deserialize<'R>(jsonStr, options)
                return msg
        }
    
    /// <summary>
    /// Disconnects the socket, optionally allowing reuse.
    /// </summary>
    /// <param name="reuseSocket">Whether to reuse the socket after disconnecting.</param>
    member _.Disconnect<'E> (reuseSocket: bool) : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> socket.Disconnect reuseSocket
        }

    /// <summary>
    /// Disconnects the socket and allows reuse by default.
    /// </summary>
    member this.Disconnect<'E> () : FIO<unit, 'E> =
        this.Disconnect true

    /// <summary>
    /// Closes the socket.
    /// </summary>
    member _.Close<'E> () : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> socket.Close()
        }
    
    /// <summary>
    /// Gets the remote endpoint of the socket.
    /// </summary>
    member _.RemoteEndPoint<'E> () : FIO<EndPoint, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.RemoteEndPoint
        }
    
    /// <summary>
    /// Gets the address family of the socket.
    /// </summary>
    member _.AddressFamily : FIO<AddressFamily, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.AddressFamily
        }

    /// <summary>
    /// Checks whether the socket is currently connected.
    /// </summary>
    member _.Connected<'E> () : FIO<bool, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.Connected
        }

    /// <summary>
    /// Gets the local endpoint of the socket.
    /// </summary>
    member _.LocalEndPoint<'E> () : FIO<EndPoint, 'E> =
        fio {
            return! fromFunc <| fun () -> socket.LocalEndPoint
        }

    /// <summary>
    /// Shuts down the socket for sending, receiving, or both.
    /// </summary>
    /// <param name="how">The type of shutdown to perform.</param>
    member _.Shutdown<'E> (how: SocketShutdown) : FIO<unit, 'E> =
        fio {
            do! fromFunc <| fun () -> socket.Shutdown how
        }
