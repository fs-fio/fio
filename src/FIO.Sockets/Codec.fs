namespace FIO.Sockets

open FIO.DSL

open System
open System.Net
open System.Text
open System.Text.Json

type SocketCodec<'A> =
    {
        Encode: 'A -> FIO<byte[], SocketError>
        Decode: byte[] -> FIO<'A, SocketError>
    }

[<RequireQualifiedAccess>]
module Codec =

    let private writeLengthPrefix (length: int) =
        BitConverter.GetBytes(IPAddress.HostToNetworkOrder length)

    let private readLengthPrefix (bytes: byte[]) (offset: int) =
        IPAddress.NetworkToHostOrder(BitConverter.ToInt32(bytes, offset))

    let bytes =
        {
            Encode = fun bytes -> FIO.succeed bytes
            Decode = fun bytes -> FIO.succeed bytes
        }

    let string: SocketCodec<string> =
        {
            Encode = fun str ->
                FIO.attempt
                    (fun () -> Encoding.UTF8.GetBytes str)
                    (fun ex -> CodecError("UTF-8 encoding failed", ex))
            Decode = fun bytes ->
                FIO.attempt
                    (fun () -> Encoding.UTF8.GetString bytes)
                    (fun ex -> CodecError("UTF-8 decoding failed", ex))
        }

    let line =
        {
            Encode = fun str ->
                fio {
                    let! encoded = string.Encode(str + "\n")
                    return encoded
                }
            Decode = fun bytes ->
                fio {
                    let! decoded = string.Decode bytes
                    return decoded.TrimEnd('\n', '\r')
                }
        }

    let jsonWithOptions<'A> (options: JsonSerializerOptions) =
        {
            Encode = fun value ->
                FIO.attempt
                    (fun () ->
                        let json = JsonSerializer.Serialize(value, options)
                        Encoding.UTF8.GetBytes json)
                    (fun ex -> CodecError("JSON encoding failed", ex))
            Decode = fun bytes ->
                FIO.attempt
                    (fun () ->
                        let json = Encoding.UTF8.GetString bytes
                        JsonSerializer.Deserialize<'A>(json, options))
                    (fun ex -> CodecError("JSON decoding failed", ex))
        }

    let json<'A> = jsonWithOptions<'A> (JsonSerializerOptions())

    let jsonLine<'A> options =
        let opts = defaultArg options (JsonSerializerOptions())
        {
            Encode = fun value ->
                FIO.attempt
                    (fun () ->
                        let json = JsonSerializer.Serialize(value, opts)
                        Encoding.UTF8.GetBytes(json + "\n"))
                    (fun ex -> CodecError("JSON line encoding failed", ex))
            Decode = fun bytes ->
                FIO.attempt
                    (fun () ->
                        let json = Encoding.UTF8.GetString(bytes).TrimEnd('\n', '\r')
                        JsonSerializer.Deserialize<'A>(json, opts))
                    (fun ex -> CodecError("JSON line decoding failed", ex))
        }

    let map (forward: 'A -> 'A1) (backward: 'A1 -> 'A) (codec: SocketCodec<'A>) =
        {
            Encode = fun value -> codec.Encode(backward value)
            Decode = fun bytes ->
                fio {
                    let! decoded = codec.Decode bytes
                    return forward decoded
                }
        }

    let compose (codec: SocketCodec<'A>) (codec': SocketCodec<'A1>) =
        {
            Encode = fun (first, second) ->
                fio {
                    let! bytes1 = codec.Encode first
                    let! bytes2 = codec'.Encode second
                    return Array.concat
                        [ writeLengthPrefix bytes1.Length
                          bytes1
                          writeLengthPrefix bytes2.Length
                          bytes2 ]
                }
            Decode = fun bytes ->
                fio {
                    if isNull bytes || bytes.Length < 8 then
                        return! FIO.fail (CodecError(
                            "Insufficient bytes for pair decoding (need at least 8 bytes for length prefixes)",
                            ArgumentException "bytes"))
                    else
                        let len1 = readLengthPrefix bytes 0
                        if len1 < 0 then
                            return! FIO.fail (CodecError(
                                $"Negative first payload length: {len1}",
                                ArgumentOutOfRangeException "len1"))
                        elif int64 bytes.Length < 4L + int64 len1 + 4L then
                            return!
                                FIO.fail (CodecError(
                                    $"Incomplete first payload: expected {len1} bytes plus second length prefix",
                                    ArgumentException "bytes"))
                        else
                            let bytes1 = bytes[4 .. 4 + len1 - 1]
                            let len2 = readLengthPrefix bytes (4 + len1)
                            if len2 < 0 then
                                return! FIO.fail (CodecError(
                                    $"Negative second payload length: {len2}",
                                    ArgumentOutOfRangeException "len2"))
                            elif int64 bytes.Length < 4L + int64 len1 + 4L + int64 len2 then
                                return!
                                    FIO.fail (CodecError(
                                        $"Incomplete second payload: expected {len2} bytes",
                                        ArgumentException "bytes"))
                            else
                                let bytes2 = bytes[4 + len1 + 4 .. 4 + len1 + 4 + len2 - 1]
                                let! first = codec.Decode bytes1
                                let! second = codec'.Decode bytes2
                                return first, second
                }
        }

    let lengthPrefixed<'A> (innerCodec: SocketCodec<'A>) =
        {
            Encode = fun value ->
                fio {
                    let! payload = innerCodec.Encode value
                    return Array.append (writeLengthPrefix payload.Length) payload
                }
            Decode = fun bytes ->
                fio {
                    if isNull bytes || bytes.Length < 4 then
                        return! FIO.fail (CodecError(
                            "Insufficient bytes for length prefix",
                            ArgumentException "bytes"))
                    else
                        let length = readLengthPrefix bytes 0
                        if length < 0 then
                            return! FIO.fail (CodecError(
                                $"Negative payload length: {length}",
                                ArgumentOutOfRangeException "length"))
                        elif int64 length > int64 bytes.Length - 4L then
                            return! FIO.fail (CodecError(
                                $"Incomplete payload: expected {length}, got {bytes.Length - 4}",
                                ArgumentException "bytes"))
                        else
                            return! innerCodec.Decode bytes[4 .. 4 + length - 1]
                }
        }

    let create (encode: 'A -> FIO<byte[], SocketError>) (decode: byte[] -> FIO<'A, SocketError>) =
        { Encode = encode; Decode = decode }

    let createPure (encode: 'A -> byte[]) (decode: byte[] -> 'A) =
        {
            Encode = fun value ->
                FIO.attempt
                    (fun () -> encode value)
                    (fun ex -> CodecError("Encoding failed", ex))
            Decode = fun bytes ->
                FIO.attempt
                    (fun () -> decode bytes)
                    (fun ex -> CodecError("Decoding failed", ex))
        }
