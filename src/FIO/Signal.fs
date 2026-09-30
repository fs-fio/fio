namespace FIO.Signal

open FIO.DSL

open System.Threading
open System.Runtime.InteropServices

/// A subscription to POSIX signals, open while the body given to Signal.subscribe runs.
[<Sealed>]
type SignalSubscription<'E> internal (signals: Channels.ChannelReader<PosixSignal>, onError: exn -> 'E) =

    /// Returns an effect that waits for the next signal, in arrival order. The fiber can be interrupted while waiting.
    member _.Next () : FIO<PosixSignal, 'E> =
        FIO.cancellationToken().FlatMap <| fun cancellationToken ->
            FIO.awaitTask (signals.ReadAsync(cancellationToken).AsTask()) onError

[<RequireQualifiedAccess>]
module Signal =

    // A registration made before one that throws would otherwise never be disposed.
    let private register (signals: PosixSignal list) (writer: Channels.ChannelWriter<PosixSignal>) =
        let registrations = ResizeArray<PosixSignalRegistration>()

        try
            for signal in signals do
                registrations.Add(PosixSignalRegistration.Create(signal, fun context -> writer.TryWrite context.Signal |> ignore))

            registrations
        with _ ->
            for registration in registrations do
                registration.Dispose()

            reraise ()

    /// Runs body with a subscription to the given signals for as long as body runs; a terminating signal still ends
    /// the process unless another registration cancels its default action. Windows: SIGINT, SIGQUIT, SIGTERM, SIGHUP only.
    let subscribe<'A, 'E> (signals: PosixSignal list) (onError: exn -> 'E) (body: SignalSubscription<'E> -> FIO<'A, 'E>) : FIO<'A, 'E> =
        let acquire =
            FIO.attempt
                (fun () ->
                    let channel = Channels.Channel.CreateUnbounded<PosixSignal>()
                    struct (channel.Reader, channel.Writer, register signals channel.Writer))
                onError

        let release struct (_, writer: Channels.ChannelWriter<PosixSignal>, registrations: ResizeArray<PosixSignalRegistration>) =
            FIO.succeedWith (fun () ->
                for registration in registrations do
                    registration.Dispose()

                writer.TryComplete() |> ignore)

        FIO.acquireReleaseWith acquire release (fun struct (reader, _, _) -> body (SignalSubscription(reader, onError)))
