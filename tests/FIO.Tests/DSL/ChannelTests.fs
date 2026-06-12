/// <summary>Provides property-based tests for channel send and receive operations.</summary>
module FIO.Tests.ChannelTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Direct
open FIO.Runtime.Cooperative
open FIO.Runtime.Concurrent

open Expecto

open System

let private runtimes () =
    [
        new DirectRuntime() :> FIORuntime
        new CooperativeRuntime() :> FIORuntime
        new ConcurrentRuntime() :> FIORuntime
    ]

let private testAllRuntimes name (f: FIORuntime -> unit) =
    testList name [ for rt in runtimes () -> testCase (rt.GetType().Name) (fun () -> f rt) ]

[<Tests>]
let channelTests =
    testList
        "Channel"
        [
            // ─── Constructor ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Constructor - creates channel with zero count"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        return chan.Count
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 0 "New channel should have count 0"

            testAllRuntimes "Constructor - creates independent channel instances" (fun runtime ->
                let effect =
                    fio {
                        let chan1 = Channel<int>()
                        let chan2 = Channel<int>()
                        do! chan1.Write(42).Unit()
                        return chan1.Count, chan2.Count
                    }

                let c1, c2 =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal c1 1 "Channel with message should have count 1"
                Expect.equal c2 0 "Other channel should remain at count 0")

            // ─── Id ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Id - each channel has unique id"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let chan1 = Channel<int>()
                        let chan2 = Channel<int>()
                        return chan1.Id, chan2.Id
                    }

                let id1, id2 =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.notEqual id1 id2 "Each channel should have a unique id"
                Expect.notEqual id1 Guid.Empty "Channel id should not be empty"

            testPropertyWithConfig fsCheckConfig "Id - is stable across repeated access"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let id1 = chan.Id
                        let id2 = chan.Id
                        return id1, id2
                    }

                let id1, id2 =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal id1 id2 "Id should return the same value on repeated access"

            // ─── Count ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Count - increments after send"
            <| fun (runtime: FIORuntime, msg: int) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let before = chan.Count
                        do! chan.Write(msg).Unit()
                        let after = chan.Count
                        return before, after
                    }

                let before, after =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal before 0 "Count should be 0 before send"
                Expect.equal after 1 "Count should be 1 after send"

            testPropertyWithConfig fsCheckConfig "Count - decrements after receive"
            <| fun (runtime: FIORuntime, msg: int) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Write(msg).Unit()
                        let before = chan.Count
                        let! _ = chan.Read()
                        let after = chan.Count
                        return before, after
                    }

                let before, after =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal before 1 "Count should be 1 before receive"
                Expect.equal after 0 "Count should be 0 after receive"

            testAllRuntimes "Count - tracks multiple messages accurately" (fun runtime ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Write(1).Unit()
                        do! chan.Write(2).Unit()
                        do! chan.Write(3).Unit()
                        let afterThreeSends = chan.Count
                        let! _ = chan.Read()
                        let afterOneReceive = chan.Count
                        let! _ = chan.Read()
                        let afterTwoReceives = chan.Count
                        return afterThreeSends, afterOneReceive, afterTwoReceives
                    }

                let c3, c2, c1 =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal c3 3 "Count should be 3 after three sends"
                Expect.equal c2 2 "Count should be 2 after one receive"
                Expect.equal c1 1 "Count should be 1 after two receives")

            // ─── Send ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Send - returns the sent message"
            <| fun (runtime: FIORuntime, msg: int) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let! sent = chan.Write msg
                        return sent
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result msg "Send should return the sent message"

            testPropertyWithConfig fsCheckConfig "Send - with string type"
            <| fun (runtime: FIORuntime, msg: string) ->
                let effect =
                    fio {
                        let chan = Channel<string>()
                        let! sent = chan.Write msg
                        return sent
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result msg "Send should return the sent string"

            testPropertyWithConfig fsCheckConfig "Send - with tuple type"
            <| fun (runtime: FIORuntime, a: int, b: string) ->
                let effect =
                    fio {
                        let chan = Channel<int * string>()
                        let! sent = chan.Write(a, b)
                        return sent
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result (a, b) "Send should return the sent tuple"

            // ─── Receive ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Receive - returns sent message"
            <| fun (runtime: FIORuntime, msg: int) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Write(msg).Unit()
                        let! received = chan.Read()
                        return received
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result msg "Receive should return the sent message"

            testPropertyWithConfig fsCheckConfig "Receive - works with different message types"
            <| fun (runtime: FIORuntime, msg: string) ->
                let effect =
                    fio {
                        let chan = Channel<string>()
                        do! chan.Write(msg).Unit()
                        let! received = chan.Read()
                        return received
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result msg "Receive should work with string type"

            testPropertyWithConfig fsCheckConfig "Receive - preserves FIFO order"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Write(1).Unit()
                        do! chan.Write(2).Unit()
                        do! chan.Write(3).Unit()
                        let! r1 = chan.Read()
                        let! r2 = chan.Read()
                        let! r3 = chan.Read()
                        return [ r1; r2; r3 ]
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "Messages should be received in FIFO order"

            testPropertyWithConfig fsCheckConfig "Receive - alternating send and receive"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        do! chan.Write(10).Unit()
                        let! r1 = chan.Read()
                        do! chan.Write(20).Unit()
                        let! r2 = chan.Read()
                        do! chan.Write(30).Unit()
                        let! r3 = chan.Read()
                        return [ r1; r2; r3 ]
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 10; 20; 30 ] "Alternating send/receive should work correctly"

            testAllRuntimes "Receive - blocks until message is sent" (fun runtime ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let! receiverFiber = (chan.Read()).Fork()
                        do! chan.Write(42).Unit()
                        let! received = receiverFiber.Join()
                        return received
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result 42 "Blocked receiver should get message once sent")

            // ─── Concurrent ─────────────────────────────────────────

            testAllRuntimes "Concurrent - multiple receivers get all messages" (fun runtime ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let! r1 = chan.Read().Fork()
                        let! r2 = chan.Read().Fork()
                        let! r3 = chan.Read().Fork()
                        do! chan.Write(1).Unit()
                        do! chan.Write(2).Unit()
                        do! chan.Write(3).Unit()
                        let! v1 = r1.Join()
                        let! v2 = r2.Join()
                        let! v3 = r3.Join()
                        return [ v1; v2; v3 ] |> List.sort
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "All receivers should get messages")

            testAllRuntimes "Concurrent - multiple senders" (fun runtime ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let! s1 = chan.Write(1).Unit().Fork()
                        let! s2 = chan.Write(2).Unit().Fork()
                        let! s3 = chan.Write(3).Unit().Fork()
                        do! s1.Join()
                        do! s2.Join()
                        do! s3.Join()
                        let! v1 = chan.Read()
                        let! v2 = chan.Read()
                        let! v3 = chan.Read()
                        return [ v1; v2; v3 ] |> List.sort
                    }

                let result =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal result [ 1; 2; 3 ] "All sent messages should be receivable")

            // ─── Isolation ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Isolation - messages don't cross channels"
            <| fun (runtime: FIORuntime) ->
                let effect =
                    fio {
                        let chan1 = Channel<int>()
                        let chan2 = Channel<int>()
                        do! chan1.Write(42).Unit()
                        let c1 = chan1.Count
                        let c2 = chan2.Count
                        let! received = chan1.Read()
                        return c1, c2, received
                    }

                let c1, c2, received =
                    runtime.Run(effect).UnsafeSuccess()

                Expect.equal c1 1 "Source channel should have 1 message"
                Expect.equal c2 0 "Other channel should be empty"
                Expect.equal received 42 "Should receive from correct channel"

            // ─── Interruption ─────────────────────────────────────────

            testAllRuntimes "Interruption - blocked receiver can be interrupted" (fun runtime ->
                let effect =
                    fio {
                        let chan = Channel<int>()
                        let! receiverFiber = (chan.Read()).Fork()
                        do! FIO.sleep (TimeSpan.FromMilliseconds 10.0) id
                        do! receiverFiber.InterruptNow ()
                        return receiverFiber
                    }

                let fiber =
                    runtime.Run(effect).UnsafeSuccess()

                match fiber.UnsafeResult() with
                | Interrupted _ -> ()
                | other -> failtest $"Expected Interrupted but got: {other}")

            // ─── Stress ─────────────────────────────────────────

            testPropertyWithConfig fsCheckConfig "Stress - 1000 sequential messages preserve FIFO order"
            <| fun (runtime: FIORuntime) ->
                let messages = [ 1..1000 ]

                let effect =
                    fio {
                        let chan = Channel<int>()

                        for msg in messages do
                            do! chan.Write(msg).Unit()

                        let mutable received = []

                        for _ in messages do
                            let! msg = chan.Read()
                            received <- received @ [ msg ]

                        return received
                    }

                let result = runtime.Run(effect).UnsafeSuccess()

                Expect.equal result messages "FIFO order should be preserved for 1000 messages"

            // Stress test targeting the signal protocol re-check in ConcurrentRuntime's
            // processBlockingChannel. Many blocked receivers + many
            // concurrent senders maximises the chance of TryBeginSignalProcessing failures
            // that rely on the finally-block re-check to avoid lost signals.
            testCase "Stress - concurrent senders with many blocked receivers (signal protocol)"
            <| fun () ->
                let receiverCount = 50
                let iterations = 20

                for _ in 1..iterations do
                    use runtime = new ConcurrentRuntime()

                    let effect =
                        fio {
                            let chan = Channel<int>()

                            let! receiverFibers =
                                FIO.forEach [ 1..receiverCount ] (fun _ ->
                                    chan.Read().Fork())

                            let! senderFibers =
                                FIO.forEach [ 1..receiverCount ] (fun i ->
                                    chan.Write(i).Unit().Fork())

                            do! FIO.forEachDiscard senderFibers (fun sf -> sf.Join())

                            let! results =
                                FIO.forEach receiverFibers (fun rf -> rf.Join())

                            return results |> List.sort
                        }

                    let result =
                        runtime.Run(effect).UnsafeSuccess()

                    Expect.equal
                        result
                        [ 1..receiverCount ]
                        "All blocked receivers must be rescheduled (no lost signals)"
        ]
