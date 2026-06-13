namespace FIO.Sockets

open FIO.DSL

open System.Text.Json

module SocketExtensions =

    type Socket with

        member this.SendJson<'A> (value: 'A, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'A> opts
                do! this.Send(codec, value)
            }

        member this.ReceiveJson<'A> (maxBytes: int, ?options) =
            fio {
                let opts = defaultArg options (JsonSerializerOptions())
                let codec = Codec.jsonWithOptions<'A> opts
                return! this.Receive(codec, maxBytes)
            }

        member this.SendJsonLine<'A> (value: 'A, ?options) =
            fio {
                let codec = Codec.jsonLine<'A> options
                do! this.Send(codec, value)
            }

        member this.ReceiveJsonLine<'A> (maxBytes: int, ?options) =
            fio {
                let codec = Codec.jsonLine<'A> options
                return! this.Receive(codec, maxBytes)
            }
