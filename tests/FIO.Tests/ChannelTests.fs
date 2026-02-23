module FIO.Tests.ChannelTests

open FIO.Tests.Utilities.FsCheckProperties

open FIO.DSL
open FIO.Runtime

open Expecto

open System

[<Tests>]
let channelCreationTests =
    testList "Channel Creation" [

        testPropertyWithConfig fsCheckConfig "Channel constructor creates channel"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int>()
                return chan.Count = 0
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Channel should be created with Count = 0"

        testPropertyWithConfig fsCheckConfig "Channel.Id is unique for each channel"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan1 = Channel<int>()
                let chan2 = Channel<int>()
                let chan3 = Channel<string>()
                return chan1.Id <> chan2.Id && chan2.Id <> chan3.Id && chan1.Id <> chan3.Id
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Channel IDs should be unique"

        testPropertyWithConfig fsCheckConfig "Channel.Id is not empty GUID"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int>()
                return chan.Id <> Guid.Empty
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Channel ID should not be empty GUID"
    ]

[<Tests>]
let channelSendReceiveTests =
    testList "Channel Send/Receive" [

        testPropertyWithConfig fsCheckConfig "Send then Receive returns same message"
        <| fun (runtime: FIORuntime, msg: int) ->
            let eff = fio {
                let chan = Channel<int>()
                let! sentMsg = chan.Send msg
                let! receivedMsg = chan.Receive()
                return sentMsg = receivedMsg && receivedMsg = msg
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Send then Receive should return same message"

        testPropertyWithConfig fsCheckConfig "Send returns the sent message"
        <| fun (runtime: FIORuntime, msg: string) ->
            let eff = fio {
                let chan = Channel<string>()
                let! sentMsg = chan.Send msg
                return sentMsg
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result msg "Send should return the sent message"

        testPropertyWithConfig fsCheckConfig "Multiple sends then receives in order"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(1).Unit()
                do! chan.Send(2).Unit()
                do! chan.Send(3).Unit()
                let! msg1 = chan.Receive()
                let! msg2 = chan.Receive()
                let! msg3 = chan.Receive()
                return [msg1; msg2; msg3]
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "Messages should be received in FIFO order"
    ]

[<Tests>]
let channelCountTests =
    testList "Channel Count" [

        testPropertyWithConfig fsCheckConfig "Count increments after send"
        <| fun (runtime: FIORuntime, msg: int) ->
            let eff = fio {
                let chan = Channel<int>()
                let countBefore = chan.Count
                do! chan.Send(msg).Unit()
                let countAfter = chan.Count
                return countBefore, countAfter
            }

            let before, after = runtime.Run(eff).UnsafeSuccess()

            Expect.equal before 0 "Count should be 0 before send"
            Expect.equal after 1 "Count should be 1 after send"

        testPropertyWithConfig fsCheckConfig "Count decrements after receive"
        <| fun (runtime: FIORuntime, msg: int) ->
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(msg).Unit()
                let countBefore = chan.Count
                let! _ = chan.Receive()
                let countAfter = chan.Count
                return countBefore, countAfter
            }

            let before, after = runtime.Run(eff).UnsafeSuccess()

            Expect.equal before 1 "Count should be 1 before receive"
            Expect.equal after 0 "Count should be 0 after receive"

        testPropertyWithConfig fsCheckConfig "Count reflects multiple messages"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int>()
                do! chan.Send(1).Unit()
                do! chan.Send(2).Unit()
                do! chan.Send(3).Unit()
                let countAfterSend = chan.Count
                let! _ = chan.Receive()
                let countAfterOneReceive = chan.Count
                let! _ = chan.Receive()
                let! _ = chan.Receive()
                let countAfterAllReceive = chan.Count
                return countAfterSend, countAfterOneReceive, countAfterAllReceive
            }

            let afterSend, afterOne, afterAll = runtime.Run(eff).UnsafeSuccess()
           
            Expect.equal afterSend 3 "Count should be 3 after three sends"
            Expect.equal afterOne 2 "Count should be 2 after one receive"
            Expect.equal afterAll 0 "Count should be 0 after all receives"
    ]

[<Tests>]
let channelFifoTests =
    testList "Channel FIFO Order" [

        testPropertyWithConfig fsCheckConfig "FIFO order preserved with sequential sends"
        <| fun (runtime: FIORuntime, messages: int list) ->
            if List.isEmpty messages then ()
            else
                let eff = fio {
                    let chan = Channel<int>()
                    for msg in messages do
                        do! chan.Send(msg).Unit()
                    let mutable received = []
                    for _ in messages do
                        let! msg = chan.Receive()
                        received <- received @ [msg]
                    return received
                }

                let result = runtime.Run(eff).UnsafeSuccess()

                Expect.equal result messages "FIFO order should be preserved"

        testPropertyWithConfig fsCheckConfig "FIFO order preserved with 100 messages"
        <| fun (runtime: FIORuntime) ->
            let messages = [1..100]
            let eff = fio {
                let chan = Channel<int>()
                for msg in messages do
                    do! chan.Send(msg).Unit()
                let mutable received = []
                for _ in messages do
                    let! msg = chan.Receive()
                    received <- received @ [msg]
                return received
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result messages "FIFO order should be preserved for 100 messages"
    ]

[<Tests>]
let channelConcurrentTests =
    testList "Channel Concurrent Access" [

        testPropertyWithConfig fsCheckConfig "Concurrent sender and receiver"
        <| fun (runtime: FIORuntime, msg: int) ->
            let eff = fio {
                let chan = Channel<int>()
                let! receiverFiber = (chan.Receive()).Fork()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0)
                do! chan.Send(msg).Unit()
                let! received = receiverFiber.Join()
                return received
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result msg "Concurrent receiver should get sent message"

//         testPropertyWithConfig fsCheckConfig "Multiple concurrent receivers"
//         <| fun (runtime: FIORuntime) ->
//             let eff = fio {
//                 let chan = Channel<int>()
//                 let! r1 = chan.Receive().Fork()
//                 let! r2 = chan.Receive().Fork()
//                 let! r3 = chan.Receive().Fork()
//                 do! FIO.sleepExn(TimeSpan.FromMilliseconds 10.0)
//                 do! chan.Send(1).Unit()
//                 do! chan.Send(2).Unit()
//                 let! v1 = r1.Join()
//                 let! v2 = r2.Join()
//                 let! v3 = r3.Join()
//                 return [v1; v2; v3] |> List.sort
//             }

//             let result = runtime.Run(eff).UnsafeSuccess()

//             Expect.equal result [1; 2; 3] "All receivers should get messages"

        testPropertyWithConfig fsCheckConfig "Multiple concurrent senders"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int>()
                let! s1 = chan.Send(1).Unit().Fork()
                let! s2 = chan.Send(2).Unit().Fork()
                let! s3 = chan.Send(3).Unit().Fork()
                do! s1.Join()
                do! s2.Join()
                do! s3.Join()
                let! v1 = chan.Receive()
                let! v2 = chan.Receive()
                let! v3 = chan.Receive()
                return [v1; v2; v3] |> List.sort
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result [1; 2; 3] "All sent messages should be receivable"
    ]

[<Tests>]
let channelTypeTests =
    testList "Channel Type Safety" [

        testPropertyWithConfig fsCheckConfig "Channel with string type"
        <| fun (runtime: FIORuntime, msg: string) ->
            let eff = fio {
                let chan = Channel<string>()
                let! sent = chan.Send msg
                let! received = chan.Receive()
                return sent = received
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "String channel should work"

        testPropertyWithConfig fsCheckConfig "Channel with tuple type"
        <| fun (runtime: FIORuntime, a: int, b: string) ->
            let eff = fio {
                let chan = Channel<int * string>()
                let! sent = chan.Send(a, b)
                let! received = chan.Receive()
                return sent = received
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "Tuple channel should work"

        testPropertyWithConfig fsCheckConfig "Channel with list type"
        <| fun (runtime: FIORuntime, items: int list) ->
            let eff = fio {
                let chan = Channel<int list>()
                let! sent = chan.Send items
                let! received = chan.Receive()
                return sent = received
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.isTrue result "List channel should work"

        testPropertyWithConfig fsCheckConfig "Channel with option type"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int option>()
                do! chan.Send(Some 42).Unit()
                do! chan.Send(None).Unit()
                let! r1 = chan.Receive()
                let! r2 = chan.Receive()
                return r1, r2
            }

            let r1, r2 = runtime.Run(eff).UnsafeSuccess()

            Expect.equal r1 (Some 42) "Should receive Some 42"
            Expect.equal r2 None "Should receive None"

//         testPropertyWithConfig fsCheckConfig "Channel with record type"
//         <| fun (runtime: FIORuntime) ->
//             let eff = fio {
//                 let chan = Channel<{| Name: string; Value: int |}>()
//                 let msg = {| Name = "test"; Value = 123 |}
//                 do! chan.Send(msg).Unit()
//                 let! received = chan.Receive()
//                 return received
//             }

//             let result = runtime.Run(eff).UnsafeSuccess()

//             Expect.equal result.Name "test" "Should receive correct Name"
//             Expect.equal result.Value 123 "Should receive correct Value"

        testPropertyWithConfig fsCheckConfig "Channel with unit type"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<unit>()
                do! chan.Send(()).Unit()
                let! received = chan.Receive()
                return received
            }

            let result = runtime.Run(eff).UnsafeSuccess()

            Expect.equal result () "Should receive unit"
    ]

[<Tests>]
let channelIsolationTests =
    testList "Channel Isolation" [

        testPropertyWithConfig fsCheckConfig "Messages sent to one channel don't appear in another"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan1 = Channel<int>()
                let chan2 = Channel<int>()
                do! chan1.Send(42).Unit()
                let chan1Count = chan1.Count
                let chan2Count = chan2.Count
                let! received = chan1.Receive()
                return chan1Count, chan2Count, received
            }

            let c1, c2, received = runtime.Run(eff).UnsafeSuccess()

            Expect.equal c1 1 "chan1 should have 1 message"
            Expect.equal c2 0 "chan2 should stay empty"
            Expect.equal received 42 "chan1 receive should succeed"
    ]

[<Tests>]
let channelInterruptionTests =
    testList "Channel Interruption" [

        testPropertyWithConfig fsCheckConfig "Receiver fiber can be interrupted while blocked"
        <| fun (runtime: FIORuntime) ->
            let eff = fio {
                let chan = Channel<int>()
                let! receiverFiber = (chan.Receive()).Fork()
                do! FIO.sleepExn(TimeSpan.FromMilliseconds 5.0)
                do! receiverFiber.Interrupt()
                return receiverFiber
            }
            let fiber = runtime.Run(eff).UnsafeSuccess()

            match fiber.UnsafeResult() with
            | Interrupted _ -> ()
            | _ -> failwith "Fiber should be interrupted"

        // testPropertyWithConfig fsCheckConfig "Channel remains usable after receiver interruption"
        // <| fun (runtime: FIORuntime) ->
        //     let eff = fio {
        //         let chan = Channel<int>()
        //         let! receiverFiber = (chan.Receive()).Fork()
        //         do! FIO.sleepExn(System.TimeSpan.FromMilliseconds 5.0)
        //         do! receiverFiber.Interrupt()
        //         let! _ = receiverFiber.Join().OrElse(FIO.succeed -1)
        //         do! chan.Send(99).Unit()
        //         let! received = chan.Receive()
        //         return received
        //     }

        //     let result = runtime.Run(eff).UnsafeSuccess()

        //     Expect.equal result 99 "Channel should still work after interruption"
    ]

// [<Tests>]
// let channelStressTests =
//     testList "Channel Stress" [

//         testPropertyWithConfig fsCheckConfig "Multiple concurrent senders and receivers"
//         <| fun (runtime: FIORuntime) ->
//             let eff = fio {
//                 let chan = Channel<int>()
//                 let! s1 = chan.Send(1).Unit().Fork()
//                 let! s2 = chan.Send(2).Unit().Fork()
//                 let! s3 = chan.Send(3).Unit().Fork()
//                 let! s4 = chan.Send(4).Unit().Fork()
//                 let! s5 = chan.Send(5).Unit().Fork()
//                 let! r1 = chan.Receive().Fork()
//                 let! r2 = chan.Receive().Fork()
//                 let! r3 = chan.Receive().Fork()
//                 let! r4 = chan.Receive().Fork()
//                 let! r5 = chan.Receive().Fork()
//                 do! s1.Join()
//                 do! s2.Join()
//                 do! s3.Join()
//                 do! s4.Join()
//                 do! s5.Join()
//                 let! v1 = r1.Join()
//                 let! v2 = r2.Join()
//                 let! v3 = r3.Join()
//                 let! v4 = r4.Join()
//                 let! v5 = r5.Join()
//                 return [v1; v2; v3; v4; v5] |> List.sort
//             }

//             let result = runtime.Run(eff).UnsafeSuccess()

//             Expect.equal result [1; 2; 3; 4; 5] "All 5 values should be received"

//         testPropertyWithConfig fsCheckConfig "High volume sequential send/receive"
//         <| fun (runtime: FIORuntime) ->
//             let messages = [1..10000]
//             let eff = fio {
//                 let chan = Channel<int>()
//                 for msg in messages do
//                     do! chan.Send(msg).Unit()
//                 let mutable received = []
//                 for _ in messages do
//                     let! msg = chan.Receive()
//                     received <- received @ [msg]
//                 return received
//             }

//             let result = runtime.Run(eff).UnsafeSuccess()

//             Expect.equal result messages "FIFO order should be preserved for 10000 messages"
//    ]

// [<Tests>]
// let channelInterleavedTests =
//     testList "Channel Interleaved Operations" [

//         testPropertyWithConfig fsCheckConfig "Alternating send and receive"
//         <| fun (runtime: FIORuntime) ->
//             let eff = fio {
//                 let chan = Channel<int>()
//                 do! chan.Send(1).Unit()
//                 let! r1 = chan.Receive()
//                 do! chan.Send(2).Unit()
//                 let! r2 = chan.Receive()
//                 do! chan.Send(3).Unit()
//                 let! r3 = chan.Receive()
//                 return [r1; r2; r3]
//             }

//             let result = runtime.Run(eff).UnsafeSuccess()

//             Expect.equal result [1; 2; 3] "Alternating send/receive should work correctly"
//   ]
