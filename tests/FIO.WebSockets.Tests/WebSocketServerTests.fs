module FIO.WebSockets.Tests.WebSocketServerTests

open FIO.WebSockets.Tests.Utilities

open FIO.DSL

open Expecto

open FIO.WebSockets

[<Tests>]
let webSocketServerTests =
    testList "WebSocketServer" [

        testAllRuntimes "start creates listening server" (fun runtime ->
            let port = findAvailablePort ()
            let url = $"http://localhost:{port}/"

            let eff = fio {
                let! listener = WebSocketServer.start url
                do! WebSocketServer.close listener
            }

            runtime.Run(eff).UnsafeSuccess())

        testAllRuntimes "close stops the listener" (fun runtime ->
            let port = findAvailablePort ()
            let url = $"http://localhost:{port}/"

            let eff = fio {
                let! listener = WebSocketServer.start url
                do! WebSocketServer.close listener
                let! listener2 = WebSocketServer.start url
                do! WebSocketServer.close listener2
            }

            runtime.Run(eff).UnsafeSuccess())

        testAllRuntimes "accept receives client connection" (fun runtime ->
            withTestServer
                (fun ws -> fio {
                    do! ws.SendText "from server"
                })
                (fun port -> fio {
                    let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                    let! msg = ws.ReceiveMessage()

                    match msg with
                    | Frame(Text s) -> Expect.equal s "from server" "Should receive server message"
                    | other -> failtest $"Expected text frame but got {other}"

                    do! ws.Close()
                })
                runtime)

        testAllRuntimes "acceptLoop handles multiple connections" (fun runtime ->
            withTestEchoServer (fun port ->
                fio {
                    for i in 1..3 do
                        let! ws = WebSocketClient.connectDefault $"ws://localhost:{port}/"
                        let msg = $"msg{i}"
                        do! ws.SendText msg
                        let! received = ws.ReceiveMessage()

                        match received with
                        | Frame(Text s) -> Expect.equal s msg $"Echo {i} should match"
                        | other -> failtest $"Expected text frame but got {other}"

                        do! ws.Close()
                }) runtime)

        testAllRuntimes "startDefault is alias for start" (fun runtime ->
            let port = findAvailablePort ()
            let url = $"http://localhost:{port}/"

            let eff = fio {
                let! listener = WebSocketServer.startDefault url
                do! WebSocketServer.close listener
            }
            
            runtime.Run(eff).UnsafeSuccess())
    ]
