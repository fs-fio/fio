namespace FIO.Sockets

open FIO.DSL

open System.Text.Json

module SocketExtensions =

    type Socket with

        /// Sends a value as JSON over this socket, optionally using the given serializer options.
        member this.SendJson<'A> (value: 'A, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'A> opts
                do! this.Send(codec, value)
            }

        /// Receives a JSON value from this socket, optionally using the given serializer options.
        member this.ReceiveJson<'A> (maxBytes: int, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'A> opts
                return! this.Receive(codec, maxBytes)
            }

        /// Sends a value as a newline-terminated JSON message, optionally using the given serializer options.
        member this.SendJsonLine<'A> (value: 'A, ?options) =
            fio {
                let codec = Codec.jsonLine<'A> options
                do! this.Send(codec, value)
            }

        /// Receives a newline-terminated JSON value, optionally using the given serializer options.
        member this.ReceiveJsonLine<'A> (maxBytes: int, ?options) =
            fio {
                let codec = Codec.jsonLine<'A> options
                return! this.Receive(codec, maxBytes)
            }
