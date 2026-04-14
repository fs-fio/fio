module FIO.Http.Tests.ServerTests

open FIO.Http.Tests.Utilities

open FIO.DSL
open FIO.Http

open System.Text

open Expecto

[<Tests>]
let serverTests =
    testSequenced (
        testList
            "Server"
            [

                testCase "starts and responds to GET request"
                <| fun () ->
                    let routes = get "/" (HttpHandler.text "hello")

                    withTestHttpServer routes (fun port ->
                        use client = new System.Net.Http.HttpClient()

                        let resp = client.GetAsync($"http://127.0.0.1:{port}/").Result
                        Expect.equal (int resp.StatusCode) 200 "200"

                        let body = resp.Content.ReadAsStringAsync().Result
                        Expect.equal body "hello" "Body")

                testCase "routes to correct handler"
                <| fun () ->
                    let routes =
                        get "/a" (HttpHandler.text "A")
                        |> Routes.combine (get "/b" (HttpHandler.text "B"))

                    withTestHttpServer routes (fun port ->
                        use client = new System.Net.Http.HttpClient()

                        let bodyA = client.GetStringAsync($"http://127.0.0.1:{port}/a").Result
                        let bodyB = client.GetStringAsync($"http://127.0.0.1:{port}/b").Result

                        Expect.equal bodyA "A" "Route A"
                        Expect.equal bodyB "B" "Route B")

                testCase "returns 404 for unknown path"
                <| fun () ->
                    let routes = get "/known" (HttpHandler.text "known")

                    withTestHttpServer routes (fun port ->
                        use client = new System.Net.Http.HttpClient()

                        let resp = client.GetAsync($"http://127.0.0.1:{port}/unknown").Result

                        Expect.equal (int resp.StatusCode) 404 "404")

                testCase "handles JSON response body"
                <| fun () ->
                    let routes = get "/json" (HttpHandler.okJson {| message = "hello" |})

                    withTestHttpServer routes (fun port ->
                        use client = new System.Net.Http.HttpClient()
                        let resp = client.GetAsync($"http://127.0.0.1:{port}/json").Result
                        Expect.equal (int resp.StatusCode) 200 "200"

                        let ct = resp.Content.Headers.ContentType.ToString()
                        Expect.stringContains ct "application/json" "JSON content-type"

                        let body = resp.Content.ReadAsStringAsync().Result
                        Expect.stringContains body "hello" "JSON body")

                testCase "handles POST with request body"
                <| fun () ->
                    let routes =
                        post "/echo" (fun req -> FIO.succeed (Response.okText (req.Body.AsString())))

                    withTestHttpServer routes (fun port ->
                        use client = new System.Net.Http.HttpClient()

                        let content =
                            new System.Net.Http.StringContent("test payload", Encoding.UTF8, "text/plain")

                        let resp = client.PostAsync($"http://127.0.0.1:{port}/echo", content).Result
                        Expect.equal (int resp.StatusCode) 200 "200"

                        let body = resp.Content.ReadAsStringAsync().Result
                        Expect.equal body "test payload" "Echoed body")

                testCase "handles text response body"
                <| fun () ->
                    let routes = get "/text" (HttpHandler.text "plain text")

                    withTestHttpServer routes (fun port ->
                        use client = new System.Net.Http.HttpClient()

                        let body = client.GetStringAsync($"http://127.0.0.1:{port}/text").Result
                        Expect.equal body "plain text" "Text body")
            ]
    )
