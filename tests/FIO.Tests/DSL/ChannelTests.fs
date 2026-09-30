module FIO.Tests.ChannelTests

open FIO.Tests.Utilities
open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime
open FIO.Runtime.Polling
open FIO.Runtime.Signaling
open FIO.Runtime.WorkStealing

open Expecto

open System
open System.Collections.Generic

[<Tests>]
let channelTests =
    testList
        "Channel"
        [
            testList
                "Constructor"
                [
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
                ]

            testList
                "Id"
                [
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
                ]

            testList
                "Count"
                [
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
                ]

            testList
                "Send"
                [
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
                ]

            testList
                "Receive"
                [
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
                ]

            testList
                "Concurrent"
                [
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
                ]

            testList
                "Isolation"
                [
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
                ]

            testList
                "Interruption"
                [
                    testAllRuntimes "Interruption - blocked receiver can be interrupted" (fun runtime ->
                        let effect =
                            fio {
                                let chan = Channel<int>()
                                let! receiverFiber = (chan.Read()).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 10.0)
                                do! receiverFiber.InterruptNow ()
                                return receiverFiber
                            }

                        let fiber =
                            runtime.Run(effect).UnsafeSuccess()

                        match fiber.UnsafeResult() with
                        | Interrupted _ -> ()
                        | other -> failtest $"Expected Interrupted but got: {other}")
                ]

            testSequenced (
                testList
                    "Stress"
                    [
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

                        testCase "Stress - concurrent senders with many blocked receivers (signal protocol)"
                        <| fun () ->
                            let receiverCount = 50
                            let iterations = 20

                            for _ in 1..iterations do
                                use runtime = new WorkStealingRuntime()

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

                        stressTestCase "Stress - bounded-buffer pattern at high iteration count (lost-wakeup regression)"
                        <| fun () ->
                            let producerCount = 4
                            let consumerCount = 4
                            let capacity = 10
                            let itemsPerProducer = 100_000
                            let totalItems = producerCount * itemsPerProducer

                            use runtime = new WorkStealingRuntime()

                            let effect =
                                fio {
                                    let hub = Channel<Choice<int * Channel<unit>, Channel<int>>>()
                                    let items = Queue<int>()
                                    let waitingProducers = Queue<int * Channel<unit>>()
                                    let waitingConsumers = Queue<Channel<int>>()
                                    let mutable delivered = 0

                                    let bufferActor =
                                        let rec loop () =
                                            fio {
                                                if delivered < totalItems then
                                                    match! hub.Read() with
                                                    | Choice1Of2 (item, ack) ->
                                                        if waitingConsumers.Count > 0 then
                                                            let reply = waitingConsumers.Dequeue()
                                                            delivered <- delivered + 1
                                                            do! reply.Write(item).Unit()
                                                            do! ack.Write(()).Unit()
                                                        elif items.Count < capacity then
                                                            items.Enqueue item
                                                            do! ack.Write(()).Unit()
                                                        else
                                                            waitingProducers.Enqueue(item, ack)
                                                    | Choice2Of2 reply ->
                                                        if items.Count > 0 then
                                                            let item = items.Dequeue()
                                                            delivered <- delivered + 1
                                                            do! reply.Write(item).Unit()
                                                            if waitingProducers.Count > 0 then
                                                                let parkedItem, parkedAck = waitingProducers.Dequeue()
                                                                items.Enqueue parkedItem
                                                                do! parkedAck.Write(()).Unit()
                                                        else
                                                            waitingConsumers.Enqueue reply
                                                    return! loop ()
                                            }
                                        loop ()

                                    let producer =
                                        fio {
                                            let ack = Channel<unit>()
                                            for i in 1..itemsPerProducer do
                                                do! hub.Write(Choice1Of2(i, ack)).Unit()
                                                do! ack.Read().Unit()
                                        }

                                    let consumerCounts =
                                        [ for index in 0 .. consumerCount - 1 ->
                                            let baseCount = totalItems / consumerCount
                                            let remainder = totalItems % consumerCount
                                            if index < remainder then baseCount + 1 else baseCount ]

                                    let consumer count =
                                        fio {
                                            let reply = Channel<int>()
                                            for _ in 1..count do
                                                do! hub.Write(Choice2Of2 reply).Unit()
                                                do! reply.Read().Unit()
                                        }

                                    let producers = [ for _ in 1..producerCount -> producer ]
                                    let consumers = [ for c in consumerCounts -> consumer c ]
                                    do! FIO.collectAllParDiscard (bufferActor :: (producers @ consumers))
                                }

                            let task = runtime.Run(effect).Task()

                            if not (task.Wait(TimeSpan.FromSeconds 120.0)) then
                                failwith "Deadlock detected: signal-protocol lost-wakeup race regressed"

                        stressTestCase "Stress - bounded-buffer pattern at high iteration count (Polling-BWC=1)"
                        <| fun () ->
                            let producerCount = 4
                            let consumerCount = 4
                            let capacity = 10
                            let itemsPerProducer = 100_000
                            let totalItems = producerCount * itemsPerProducer

                            use runtime =
                                new PollingRuntime
                                    { EvaluationWorkers = 12
                                      EvaluationSteps = 200
                                      BlockingWorkers = 1 }

                            let effect =
                                fio {
                                    let hub = Channel<Choice<int * Channel<unit>, Channel<int>>>()
                                    let items = Queue<int>()
                                    let waitingProducers = Queue<int * Channel<unit>>()
                                    let waitingConsumers = Queue<Channel<int>>()
                                    let mutable delivered = 0

                                    let bufferActor =
                                        let rec loop () =
                                            fio {
                                                if delivered < totalItems then
                                                    match! hub.Read() with
                                                    | Choice1Of2 (item, ack) ->
                                                        if waitingConsumers.Count > 0 then
                                                            let reply = waitingConsumers.Dequeue()
                                                            delivered <- delivered + 1
                                                            do! reply.Write(item).Unit()
                                                            do! ack.Write(()).Unit()
                                                        elif items.Count < capacity then
                                                            items.Enqueue item
                                                            do! ack.Write(()).Unit()
                                                        else
                                                            waitingProducers.Enqueue(item, ack)
                                                    | Choice2Of2 reply ->
                                                        if items.Count > 0 then
                                                            let item = items.Dequeue()
                                                            delivered <- delivered + 1
                                                            do! reply.Write(item).Unit()
                                                            if waitingProducers.Count > 0 then
                                                                let parkedItem, parkedAck = waitingProducers.Dequeue()
                                                                items.Enqueue parkedItem
                                                                do! parkedAck.Write(()).Unit()
                                                        else
                                                            waitingConsumers.Enqueue reply
                                                    return! loop ()
                                            }
                                        loop ()

                                    let producer =
                                        fio {
                                            let ack = Channel<unit>()
                                            for i in 1..itemsPerProducer do
                                                do! hub.Write(Choice1Of2(i, ack)).Unit()
                                                do! ack.Read().Unit()
                                        }

                                    let consumerCounts =
                                        [ for index in 0 .. consumerCount - 1 ->
                                            let baseCount = totalItems / consumerCount
                                            let remainder = totalItems % consumerCount
                                            if index < remainder then baseCount + 1 else baseCount ]

                                    let consumer count =
                                        fio {
                                            let reply = Channel<int>()
                                            for _ in 1..count do
                                                do! hub.Write(Choice2Of2 reply).Unit()
                                                do! reply.Read().Unit()
                                        }

                                    let producers = [ for _ in 1..producerCount -> producer ]
                                    let consumers = [ for c in consumerCounts -> consumer c ]
                                    do! FIO.collectAllParDiscard (bufferActor :: (producers @ consumers))
                                }

                            let task = runtime.Run(effect).Task()

                            if not (task.Wait(TimeSpan.FromSeconds 120.0)) then
                                failwith "PollingRuntime BWC=1 hung on bounded-buffer pattern"

                        stressTestCase "Stress - bounded-buffer pattern at high iteration count (Signaling lost-wakeup regression)"
                        <| fun () ->
                            let producerCount = 4
                            let consumerCount = 4
                            let capacity = 10
                            let itemsPerProducer = 100_000
                            let totalItems = producerCount * itemsPerProducer

                            let buildEffect () =
                                fio {
                                    let hub = Channel<Choice<int * Channel<unit>, Channel<int>>>()
                                    let items = Queue<int>()
                                    let waitingProducers = Queue<int * Channel<unit>>()
                                    let waitingConsumers = Queue<Channel<int>>()
                                    let mutable delivered = 0

                                    let bufferActor =
                                        let rec loop () =
                                            fio {
                                                if delivered < totalItems then
                                                    match! hub.Read() with
                                                    | Choice1Of2 (item, ack) ->
                                                        if waitingConsumers.Count > 0 then
                                                            let reply = waitingConsumers.Dequeue()
                                                            delivered <- delivered + 1
                                                            do! reply.Write(item).Unit()
                                                            do! ack.Write(()).Unit()
                                                        elif items.Count < capacity then
                                                            items.Enqueue item
                                                            do! ack.Write(()).Unit()
                                                        else
                                                            waitingProducers.Enqueue(item, ack)
                                                    | Choice2Of2 reply ->
                                                        if items.Count > 0 then
                                                            let item = items.Dequeue()
                                                            delivered <- delivered + 1
                                                            do! reply.Write(item).Unit()
                                                            if waitingProducers.Count > 0 then
                                                                let parkedItem, parkedAck = waitingProducers.Dequeue()
                                                                items.Enqueue parkedItem
                                                                do! parkedAck.Write(()).Unit()
                                                        else
                                                            waitingConsumers.Enqueue reply
                                                    return! loop ()
                                            }
                                        loop ()

                                    let producer =
                                        fio {
                                            let ack = Channel<unit>()
                                            for i in 1..itemsPerProducer do
                                                do! hub.Write(Choice1Of2(i, ack)).Unit()
                                                do! ack.Read().Unit()
                                        }

                                    let consumerCounts =
                                        [ for index in 0 .. consumerCount - 1 ->
                                            let baseCount = totalItems / consumerCount
                                            let remainder = totalItems % consumerCount
                                            if index < remainder then baseCount + 1 else baseCount ]

                                    let consumer count =
                                        fio {
                                            let reply = Channel<int>()
                                            for _ in 1..count do
                                                do! hub.Write(Choice2Of2 reply).Unit()
                                                do! reply.Read().Unit()
                                        }

                                    let producers = [ for _ in 1..producerCount -> producer ]
                                    let consumers = [ for c in consumerCounts -> consumer c ]
                                    do! FIO.collectAllParDiscard (bufferActor :: (producers @ consumers))
                                }

                            use runtime =
                                new SignalingRuntime
                                    { EvaluationWorkers = 12
                                      EvaluationSteps = 200
                                      BlockingWorkers = 1 }

                            for iteration in 1..10 do
                                let task = runtime.Run(buildEffect ()).Task()

                                if not (task.Wait(TimeSpan.FromSeconds 120.0)) then
                                    failwith $"SignalingRuntime hung on bounded-buffer pattern (iteration {iteration}): lost-wakeup race regressed"
                    ])

            testList
                "TryWrite"
                [
                    testAllRuntimes "TryWrite - an unbounded channel always accepts" (fun runtime ->
                        let chan = Channel<int>()
                        let accepted = runtime.Run(chan.TryWrite 1 : FIO<bool, exn>).UnsafeSuccess()

                        Expect.isTrue accepted "An unbounded channel should accept the message"
                        Expect.equal chan.Count 1 "The message should be buffered")

                    testCase "Channel members take the element type, never obj" <| fun () ->
                        let untyped =
                            typeof<Channel<int>>.GetMethods(
                                Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.DeclaredOnly)
                            |> Array.filter (fun m -> m.GetParameters() |> Array.exists (fun p -> p.ParameterType = typeof<obj>))
                            |> Array.map _.Name

                        Expect.isEmpty untyped "A member whose parameter was inferred as obj accepts messages of any type"
                ]

            testList
                "Bounded, dropping and sliding channels"
                [
                    testCase "Bounded, Dropping and Sliding - reject a capacity below 1" <| fun () ->
                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Channel<int>.Bounded 0 |> ignore) "Bounded 0"
                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Channel<int>.Dropping 0 |> ignore) "Dropping 0"
                        Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Channel<int>.Sliding 0 |> ignore) "Sliding 0"

                    testAllRuntimes "Bounded - a write to a full channel waits until a message is read" (fun runtime ->
                        let chan = Channel<int>.Bounded 1
                        let writing = ref false

                        let effect : FIO<int list * bool * int, exn> =
                            fio {
                                do! chan.Write(1).Unit()

                                let! writer =
                                    (FIO.succeedWith(fun () -> writing.Value <- true).FlatMap(fun () -> chan.Write(2).Unit())).Fork()

                                do! FIO.succeedWith (fun () -> waitForFlag writing |> ignore)
                                do! FIO.sleep (TimeSpan.FromMilliseconds 100.0)
                                let! pending = writer.Poll()
                                let countWhileWaiting = chan.Count
                                let! first = chan.Read()
                                do! writer.Join()
                                let! second = chan.Read()
                                return [ first; second ], pending.IsNone, countWhileWaiting
                            }

                        let values, waited, countWhileWaiting = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue waited "The second write, already started, should still be waiting while the channel is full"
                        Expect.equal countWhileWaiting 1 "The waiting write should not have added its message"
                        Expect.equal values [ 1; 2 ] "Both messages should arrive, in order")

                    testAllRuntimes "Bounded - an uninterruptible waiting writer still writes after an interruption" (fun runtime ->
                        let chan = Channel<int>.Bounded 1
                        let writing = ref false

                        let effect : FIO<int * int * bool, exn> =
                            fio {
                                do! chan.Write(1).Unit()

                                let! writer =
                                    (FIO.uninterruptible (
                                        FIO.succeedWith(fun () -> writing.Value <- true).FlatMap(fun () -> chan.Write(2).Unit()))).Fork()

                                do! FIO.succeedWith (fun () -> waitForFlag writing |> ignore)
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! writer.InterruptNow()
                                let! first = chan.Read()
                                let! outcome = writer.Await()
                                let! second = chan.Read()

                                let interrupted =
                                    match outcome with
                                    | Interrupted _ -> true
                                    | _ -> false

                                return first, second, interrupted
                            }

                        let first, second, interrupted = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal (first, second) (1, 2) "The uninterruptible write should land once there is room"
                        Expect.isTrue interrupted "The writer should still report its interruption")

                    testAllRuntimes "Bounded - an interrupted waiting writer never writes" (fun runtime ->
                        let chan = Channel<int>.Bounded 1

                        let effect : FIO<int * int, exn> =
                            fio {
                                do! chan.Write(1).Unit()
                                let! writer = (chan.Write(2).Unit()).Fork()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                do! writer.InterruptNow()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                let! first = chan.Read()
                                do! FIO.sleep (TimeSpan.FromMilliseconds 50.0)
                                return first, chan.Count
                            }

                        let first, left = runtime.Run(effect).UnsafeSuccess()

                        Expect.equal first 1 "The first message should be read"
                        Expect.equal left 0 "The interrupted write should never land")

                    testAllRuntimes "Bounded - TryWrite on a full channel yields false without waiting or writing" (fun runtime ->
                        let chan = Channel<int>.Bounded 1

                        let effect : FIO<bool * bool * int * int, exn> =
                            fio {
                                let! first = chan.TryWrite 1
                                let! second = chan.TryWrite 2
                                let! value = chan.Read()
                                return first, second, value, chan.Count
                            }

                        let first, second, value, left = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue first "A channel with room should accept"
                        Expect.isFalse second "A full bounded channel should refuse without suspending"
                        Expect.equal value 1 "The channel should keep its original message"
                        Expect.equal left 0 "The refused message should never be buffered")

                    testAllRuntimes "Dropping - a full channel refuses new messages and keeps its own" (fun runtime ->
                        let chan = Channel<int>.Dropping 1

                        let effect : FIO<bool * bool * int * int, exn> =
                            fio {
                                let! first = chan.TryWrite 1
                                let! second = chan.TryWrite 2
                                do! chan.Write(3).Unit()
                                let! value = chan.Read()
                                return first, second, value, chan.Count
                            }

                        let first, second, value, left = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue first "The first offer should be accepted"
                        Expect.isFalse second "An offer to a full dropping channel should be refused"
                        Expect.equal value 1 "The channel should keep its original message"
                        Expect.equal left 0 "Refused messages should never be buffered")

                    testAllRuntimes "Sliding - a full channel drops its oldest message" (fun runtime ->
                        let chan = Channel<int>.Sliding 2

                        let effect : FIO<bool * int list * int, exn> =
                            fio {
                                do! chan.Write(1).Unit()
                                do! chan.Write(2).Unit()
                                let! accepted = chan.TryWrite 3
                                let! a = chan.Read()
                                let! b = chan.Read()
                                return accepted, [ a; b ], chan.Count
                            }

                        let accepted, values, left = runtime.Run(effect).UnsafeSuccess()

                        Expect.isTrue accepted "A sliding channel should always accept"
                        Expect.equal values [ 2; 3 ] "The oldest message should have been dropped"
                        Expect.equal left 0 "Nothing else should be buffered")

                    testAllRuntimes "Bounded - many producers and consumers deliver every message exactly once" (fun runtime ->
                        let chan = Channel<int>.Bounded 4
                        let producers, perProducer, consumers = 4, 250, 4
                        let total = producers * perProducer

                        let produce p : FIO<unit, exn> =
                            FIO.forEachDiscard [ 1..perProducer ] (fun i -> chan.Write(p * perProducer + i).Unit())

                        let consume () : FIO<int list, exn> =
                            FIO.forEach [ 1 .. total / consumers ] (fun _ -> chan.Read())

                        let effect : FIO<int list, exn> =
                            fio {
                                let! producerFibers = FIO.forEach [ 0 .. producers - 1 ] (fun p -> (produce p).Fork())
                                let! consumerFibers = FIO.forEach [ 1..consumers ] (fun _ -> (consume ()).Fork())
                                let! received = FIO.forEach consumerFibers (fun fiber -> fiber.Join())
                                do! FIO.forEachDiscard producerFibers (fun fiber -> fiber.Join())
                                return List.concat received
                            }

                        let task = runtime.Run(effect).Task()
                        Expect.isTrue (task.Wait(TimeSpan.FromSeconds 60.0)) "Producers and consumers should finish without a lost wakeup"

                        match task.Result with
                        | Succeeded received ->
                            Expect.equal (List.sort received) [ 1..total ] "Every message should arrive exactly once"
                        | other -> failtest $"Expected Succeeded, got {other}")
                ]
        ]
