<a id="readme-top"></a>

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![NuGet][nuget-shield]][nuget-url]



<br />
<div align="center">
  <a href="https://github.com/fio-fsharp/fio">
    <img src="assets/images/fio_logo_wide.png" width="auto" height="300" alt="FIO Logo">
  </a>

  <h3 align="center">🪻 A Type-Safe, Purely Functional Effect System for Asynchronous and Concurrent F#</h3>

  <p align="center">
    <a href="https://itsdaniel.dk/projects/fio/">View Project Post</a>
    &middot;
    <a href="https://github.com/fs-fio/fio/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    &middot;
    <a href="https://github.com/fs-fio/fio/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>



<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-fio">About FIO</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#quick-start">Quick Start</a></li>
      </ul>
    </li>
    <li>
      <a href="#core-concepts">Core Concepts</a>
      <ul>
        <li><a href="#effect-composition">Effect Composition</a></li>
      </ul>
    </li>
    <li>
      <a href="#usage">Usage</a>
      <ul>
        <li><a href="#direct-usage">Direct Usage</a></li>
        <li><a href="#using-fioapp-recommended">Using FIOApp (Recommended)</a></li>
        <li><a href="#alternative-dsl-only-style">Alternative: DSL-Only Style</a></li>
      </ul>
    </li>
    <li>
      <a href="#extension-packages">Extension Packages</a>
      <ul>
        <li><a href="#http-server">HTTP Server</a></li>
        <li><a href="#postgresql">PostgreSQL</a></li>
        <li><a href="#tcp-sockets">TCP Sockets</a></li>
        <li><a href="#websockets">WebSockets</a></li>
      </ul>
    </li>
    <li>
      <a href="#benchmarks">Benchmarks</a>
      <ul>
        <li><a href="#benchmark-overview">Benchmark Overview</a></li>
        <li><a href="#running-benchmarks">Running Benchmarks</a></li>
        <li><a href="#example">Example</a></li>
        <li><a href="#experimental-flags">Experimental Flags</a></li>
      </ul>
    </li>
    <li>
      <a href="#performance">Performance</a>
      <ul>
        <li><a href="#execution-time">Execution Time</a></li>
        <li><a href="#scalability">Scalability</a></li>
      </ul>
    </li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li>
      <a href="#contributing">Contributing</a>
      <ul>
        <li><a href="#quick-start">Quick Start</a></li>
        <li><a href="#top-contributors">Top Contributors</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



## About FIO

<!-- [![Product Name Screen Shot][product-screenshot]](https://example.com) !-->

**FIO** is a type-safe, purely functional effect system for [**F#**](https://fsharp.org/), designed for building **highly concurrent** and **asynchronous** applications. It provides a lightweight [**DSL**](https://martinfowler.com/dsl.html) for writing composable programs using **functional effects**.

Inspired by [**ZIO**](https://zio.dev/) and [**Cats Effect**](https://typelevel.org/cats-effect/), **FIO** features:

- An **IO monad** for managing side effects  
- **Fibers** (green threads) for scalable concurrency  
- A focus on **purity**, **type safety**, and **performance**

**FIO** was developed as part of a master’s thesis in Computer Science at [**DTU**](https://www.dtu.dk/english).

> **Note:** FIO is under active development. Contributions, feedback, and questions are welcome.
> Please report bugs and request features through [GitHub Issues](https://github.com/fio-fsharp/fio/issues), or contact the maintainer at [hey@itsdaniel.dk](mailto:hey@itsdaniel.dk).


<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

### Installation

**FIO** is distributed as five NuGet packages:

#### Core Package (Required)

```bash
dotnet add package FSharp.FIO
```

The core package includes:
- **Core effect system** - FIO monad, fibers, channels, and three runtime implementations
- **App framework** - FIOApp base classes for simplified application entry points
- **Console I/O** - Functional console operations

#### Optional Extension Packages

##### TCP Sockets

```bash
dotnet add package FSharp.FIO.Sockets
```

Provides TCP socket operations with client, server, and connection pooling functionality.

##### WebSockets

```bash
dotnet add package FSharp.FIO.WebSockets
```

Provides WebSocket client and server functionality with connection pooling.

##### HTTP Server (experimental)

```bash
dotnet add package FSharp.FIO.Http
```

Provides composable HTTP server functionality built on ASP.NET Core Kestrel, including routes, handlers, and middleware.

##### PostgreSQL Database (experimental)

```bash
dotnet add package FSharp.FIO.PostgreSQL
```

Provides PostgreSQL database operations built on Npgsql, including connection pooling, query execution, and transactions.

### Quick Start

To get started with **FIO**:

1. Install [**.NET**](https://dotnet.microsoft.com/en-us/)
2. Use an editor like [**VS Code**](https://code.visualstudio.com/), [**Visual Studio**](https://visualstudio.microsoft.com/downloads/), [**Rider**](https://www.jetbrains.com/rider/download/), or any F#-compatible editor
3. Install the FSharp.FIO package (see above)
4. Explore the [**FSharp.FIO.Examples**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples) project or create your own F# file

## Core Concepts

### Effect Composition

**FIO** effects are composable values that can be combined using operators, similar to how `Result` or `Option` types can be composed in functional programming. These operators enable you to build complex concurrent programs from simple building blocks.

#### Sequential Composition

**Bind Operator (>>=)**

The bind operator chains effects sequentially, giving you access to previous results:

```fsharp
// Using computation expression
let getUserData userId =
    fio {
        let! user = fetchUser userId
        let! orders = fetchOrders user.Id
        return (user, orders)
    }

// Equivalent using >>= operator
let getUserData userId =
    fetchUser userId >>= fun user ->
    fetchOrders user.Id >>= fun orders ->
    FIO.Succeed (user, orders)
```

**Zip Operators (<*>, *>, <*)**

Combine effects sequentially with different result handling:

```fsharp
// <*> - Combine results into tuple
let combined = fetchUser() <*> fetchSettings()
// Returns: FIO<User * Settings, 'E>

// *> - Run both, keep second result
let compute =
    Console.PrintLine "Starting computation..."
    *> calculateResult()
// Returns: FIO<CalculationResult, 'E>

// <* - Run both, keep first result
let withLogging =
    performAction()
    <* Console.PrintLine "Action completed!"
// Returns: FIO<ActionResult, 'E>
```

#### Parallel Composition

**Parallel Zip (<&>)**

Execute effects concurrently and wait for both to complete:

```fsharp
// Execute two effects in parallel
let fetchUserAndOrders userId =
    fetchUser userId <&> fetchOrders userId
    <!> fun (user, orders) ->
        { User = user; Orders = orders }
// Both fetches run concurrently

// Multiple parallel API calls
let dashboard =
    getProfile() <&> getNotifications() <&> getActivity()
    <!> fun ((profile, notifications), activity) ->
        { Profile = profile
          Notifications = notifications
          Activity = activity }
```

**Parallel Operators (&>, <&, <&&>)**

```fsharp
// &> - Run in parallel, keep second result
let warmupThenCompute = warmupCache() &> calculateResult()

// <& - Run in parallel, keep first result
let computeWithBackground = calculateResult() <& logMetrics()

// <&&> - Run in parallel, discard both results
let fireAndForget =
    sendEmail() <&&> updateMetrics() <&&> logActivity()
    // All run concurrently, returns FIO<unit, 'E>
```

#### Error Handling

**OrElse Operator (<|>)**

Try the first effect, fallback to the second if it fails:

```fsharp
// Simple fallback
let fetchData = fetchFromCache() <|> fetchFromDatabase()

// Chain multiple fallbacks
let robustFetch =
    fetchFromPrimary()
    <|> fetchFromSecondary()
    <|> fetchFromCache()
    <|> FIO.Succeed(defaultValue)
```

**Map Operator (<!>)**

Transform the success value without affecting errors:

```fsharp
// Transform a single value
let doubled = computeValue() <!> fun x -> x * 2

// Chain transformations
let pipeline =
    fetchNumber()
    <!> fun n -> n * 2
    <!> fun n -> n + 10
    <!> fun n -> $"Result: {n}"
```

#### Computation Expressions vs Operators

**Use computation expressions** for complex logic with branching, loops, or multiple let bindings:

```fsharp
let processOrders = fio {
    let! orders = fetchOrders()
    let mutable processed = 0

    for order in orders do
        if order.Total > 100.0 then
            do! applyDiscount order

        do! processOrder order
        processed <- processed + 1

    return processed
}
```

**Use operators** for simple pipelines and functional composition:

```fsharp
let pipeline =
    fetchData()
    <!> validateData
    >>= enrichData
    >>= saveData
    <* Console.PrintLine "Pipeline completed!"
```

#### Operator Quick Reference

| Operator | Execution | Returns | Use Case |
|----------|-----------|---------|----------|
| `>>=` | Sequential | Result of function | Chain with access to previous value |
| `<*>` | Sequential | Tuple | Combine two results |
| `*>` | Sequential | Second | Side effect then main result |
| `<*` | Sequential | First | Main result then side effect |
| `<&>` | Parallel | Tuple | Concurrent execution |
| `&>` | Parallel | Second | Concurrent, prefer second |
| `<&` | Parallel | First | Concurrent, prefer first |
| `<&&>` | Parallel | Unit | Fire-and-forget parallel tasks |
| `<\|>` | Sequential | First success | Error recovery with fallback |
| `<!>` | N/A | Transformed | Map over success value |

For more examples of operator usage, see [**FSharp.FIO.Examples**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples).



## Usage

You can use **FIO** in two ways:  
- Directly by creating and running effects manually (examples in [**FSharp.FIO.Examples**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples))
- Via `FIOApp`, which simplifies setup and runtime management (examples in [**FSharp.FIO.Examples.App**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples.App))

#### Direct Usage

Create a new F# file and open the DSL, IO and Concurrent runtime modules:

```fsharp
module DirectUsage

open FSharp.FIO.DSL
open FSharp.FIO.Console
open FSharp.FIO.Runtime.Default

[<EntryPoint>]
let main _ =
    let askForName = fio {
        do! Console.PrintLine "Hello! What is your name?"
        let! name = Console.ReadLine
        do! Console.PrintLine $"Hello, {name}! Welcome to FIO! 🪻💜"
    }

    let fiber = (new DefaultRuntime()).Run askForName
    fiber.UnsafePrintResult()
    0
```

Run it with:

```
$ dotnet run
```

And you'll see the following output:

```
Hello! What is your name?
Daniel
Hello, Daniel! Welcome to FIO! 🪻💜
Ok ()
```

#### Using FIOApp (Recommended)

FIOApp provides a high-level framework for running FIO effects with lifecycle management, shutdown hooks, and automatic exit codes. It comes in three variants to suit different needs.

##### FIOApp Variants

**FIOApp** comes in three variants to suit different needs:

**1. SimpleFIOApp**

`SimpleFIOApp` is a type alias for `FIOApp<unit, exn>`, used when your effect performs actions without returning a meaningful result.

```fsharp
// SimpleFIOApp = FIOApp<unit, exn>
type WelcomeApp() =
    inherit SimpleFIOApp()

    override _.effect = fio {
        do! Console.PrintLine "Hello! What is your name?"
        let! name = Console.ReadLine
        do! Console.PrintLine $"Hello, {name}! Welcome to FIO! 🪻💜"
    }
```

**2. DefaultFIOApp<'R>**

`DefaultFIOApp<'R>` is a type alias for `FIOApp<'R, exn>`, used when you need to return a specific value but use standard exceptions for errors.

```fsharp
// DefaultFIOApp<'R> = FIOApp<'R, exn>
type RandomNumberApp() =
    inherit DefaultFIOApp<int>()

    override _.effect = fio {
        let! randomNumber = FIO.Attempt(fun () -> Random().Next(1, 100))
        do! Console.PrintLine $"Generated: {randomNumber}"
        return randomNumber  // Returns int
    }
```

**3. FIOApp<'R, 'E>**

The generic `FIOApp<'R, 'E>` base class provides full control over both result and error types, enabling type-safe domain-specific error handling.

```fsharp
type AppError =
    | ValidationError of string
    | NotFound
    | DatabaseError of string

type UserLookupApp() =
    inherit FIOApp<string, AppError>()

    override _.effect = fio {
        do! Console.Print "Enter user ID: "
        let! userId = Console.ReadLine

        if userId = "" then
            return! FIO.Fail(ValidationError "User ID cannot be empty")
        else
            return $"User: {userId}"
    }

    // Optional: Override exit code mappings
    override _.exitCodeError = function
        | ValidationError _ -> 2
        | NotFound -> 3
        | DatabaseError _ -> 4
```

##### Choosing Your Variant

- **Use SimpleFIOApp** when your effect returns `unit` and uses standard exceptions (most CLI apps)
- **Use DefaultFIOApp<'R>** when you need a custom result type but standard exceptions
- **Use FIOApp<'R, 'E>** when you need custom error types for domain-specific error handling

For more examples, see [**FSharp.FIO.Examples.App**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples.App).

#### Alternative: DSL-Only Style

Prefer DSL chaining? Use bind (>>=) directly:

```fsharp
module DSLOnly

open FSharp.FIO.DSL
open FSharp.FIO.Console
open FSharp.FIO.Runtime.Default

let askForName =
    Console.PrintLine "Hello! What is your name?" >>= fun _ ->
    Console.ReadLine >>= fun name ->
    Console.PrintLine $"Hello, {name}! Welcome to FIO! 🪻💜"

[<EntryPoint>]
let main _ =
    let fiber = (new DefaultRuntime()).Run askForName
    fiber.UnsafePrintResult()
    0
```

## Extension Packages

FIO provides optional packages for common scenarios:

### HTTP Server

Build HTTP servers with composable routes and middleware:

```fsharp
open FSharp.FIO.Http
open FSharp.FIO.Http.RoutesOperators
open FSharp.FIO.Http.MiddlewareOperators

// Define handlers
let helloHandler : HttpHandler<exn> =
    HttpHandler.text "Hello from FIO!"

let jsonHandler : HttpHandler<exn> =
    HttpHandler.okJson {| message = "JSON response" |}

// Compose routes
let routes =
    GET "/" helloHandler
    ++ GET "/json" jsonHandler

// Run server
let config = ServerConfig.defaultConfig  // localhost:8080
Server.runServer config routes
```

For complete examples, see [**FSharp.FIO.Examples.Http**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples.Http).

### PostgreSQL

Connect to PostgreSQL databases with connection pooling:

```fsharp
open FSharp.FIO.PostgreSQL

// Configure connection pool
let config = {
    ConnectionString = "Host=localhost;Database=mydb;Username=user;Password=pass"
    MinPoolSize = 5
    MaxPoolSize = 20
    ConnectionLifetime = 300
    CommandTimeout = 30
}

let pool = Pool.create config

// Query with parameters
let getUserById id = fio {
    let sql = "SELECT id, name, email FROM users WHERE id = @id"
    let parameters = ["id" @= id]
    return! Dsl.queryFirstWithParams sql parameters userMapper pool
}
```

For complete examples, see [**FSharp.FIO.Examples.PostgreSQL**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples.PostgreSQL).

### TCP Sockets

TCP socket client and server functionality. See [**FSharp.FIO.Sockets**](https://www.nuget.org/packages/FSharp.FIO.Sockets) for documentation.

### WebSockets

WebSocket client and server functionality. See [**FSharp.FIO.WebSockets**](https://www.nuget.org/packages/FSharp.FIO.WebSockets) for documentation.



## Benchmarks

This repository includes five benchmarks, each designed to evaluate a specific aspect of concurrent computation. All benchmarks are adapted from the [**Savina – An Actor Benchmark Suite**](http://soft.vub.ac.be/AGERE14/papers/ageresplash2014_submission_19.pdf).

### Benchmark Overview

- `Pingpong` – Message sending and retrieval between two actors  
- `Threadring` – Message passing with frequent fiber context switching  
- `Big` – Many-to-many message passing with high channel contention  
- `Bang` – Many-to-one messaging, stressing a single receiver  
- `Fork` – Measures fiber spawning overhead

### Running Benchmarks

The benchmarks accept a variety of command-line options:

```
USAGE: FSharp.FIO.Benchmarks [--help]
                             [--direct-runtime]
                             [--cooperative-runtime <ewc> <ews> <bwc>]
                             [--concurrent-runtime <ewc> <ews> <bwc>]
                             [--runs <runs>]
                             [--actor-increment <actorInc> <times>]
                             [--round-increment <roundInc> <times>]
                             [--pingpong <roundCount>]
                             [--threadring <actorCount> <roundCount>]
                             [--big <actorCount> <roundCount>]
                             [--bang <actorCount> <roundCount>]
                             [--fork <actorCount>]
                             [--save <saveToCsv>]
                             [--savepath <absolutePath>]

OPTIONS:

    --direct-runtime      specify Direct runtime
    --cooperative-runtime <ewc> <ews> <bwc>
                          specify Cooperative runtime with ewc, ews and bwc
    --concurrent-runtime <ewc> <ews> <bwc>
                          specify Concurrent runtime with ewc, ews and bwc
    --runs <runs>         specify number of runs for each benchmark
    --actor-increment <actorInc> <times>
                          specify the value of actor increment and the number of times
    --round-increment <roundInc> <times>
                          specify the value of round increment and the number of times
    --pingpong <roundCount>
                          specify number of rounds for Pingpong benchmark
    --threadring <actorCount> <roundCount>
                          specify number of actors and rounds for Threadring benchmark
    --big <actorCount> <roundCount>
                          specify number of actors and rounds for Big benchmark
    --bang <actorCount> <roundCount>
                          specify number of actors and rounds for Bang benchmark
    --fork <actorCount>   specify number of actors for Fork benchmark
    --save <saveToCsv>    should save benchmark results to csv file
    --savepath <absolutePath>
                          specify absolute path to save the benchmark results csv file
    --help                display this list of options.
```

### Example

To run each benchmark 30 times using the concurrent runtime (39 evaluation workers, 200 evaluation steps, 1 blocking worker):

```bash
--concurrent-runtime 39 200 1 --runs 30 --pingpong 150000 --threadring 10000 10 --big 250 10 --bang 10000 10 --fork 20000
```

### Experimental Flags

**FIO** also supports optional compile-time flags:

- `DETECT_DEADLOCK` – Enables a simple thread that attempts to detect deadlocks during execution

- `MONITOR` – Starts a monitoring thread that prints internal runtime structure state during execution

> **Note:** These features are experimental and may behave unpredictably.



## Performance

The following plots illustrate the **execution time** (measured in milliseconds) and **scalability** of the available runtime systems across benchmarks.

The runtimes differ in how they manage fibers and blocked operations:

- **Direct** – .NET tasks with waiting for blocked fibers
- **Cooperative** – Fibers with linear-time handling of blocked fibers
- **Concurrent** – Fibers with constant-time handling of blocked fibers

### Execution Time

The boxplots show the measured execution time for each benchmark with the shown benchmark and runtime configurations.

<img src="assets/images/boxplot.png" alt="Boxplot" />


### Scalability

The lineplots show for each benchmark, how each runtime scales when the amount of fibers increases.

<img src="assets/images/lineplot.png" alt="Lineplot" />



<!-- ROADMAP -->
## Roadmap

See the [**open issues**](https://github.com/fio-fsharp/fio/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

We welcome contributions! To contribute:
- Star the repository
- Open an issue (tag it with `enhancement`)
- Fork the project and submit a pull request

### Contributing Guide

1. Fork the repository
2. Create a branch: `git checkout -b feature/AmazingFeature`
3. Commit your changes: `git commit -m 'Add AmazingFeature'`
4. Push the branch: `git push origin feature/AmazingFeature`
5. Open a pull request

### Top contributors

<a href="https://github.com/fio-fsharp/fio/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=fio-fsharp/fio" alt="Contributors Image" />
</a>

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See [**LICENSE.md**](LICENSE.md) for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Daniel Larsen ([**itsdaniel.dk**](https://itsdaniel.dk))

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

Alceste Scalas ([**people.compute.dtu.dk**](https://people.compute.dtu.dk/alcsc/))

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/fio-fsharp/fio.svg?style=for-the-badge
[contributors-url]: https://github.com/fio-fsharp/fio/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/fio-fsharp/fio.svg?style=for-the-badge
[forks-url]: https://github.com/fio-fsharp/fio/network/members
[stars-shield]: https://img.shields.io/github/stars/fio-fsharp/fio.svg?style=for-the-badge
[stars-url]: https://github.com/fio-fsharp/fio/stargazers
[issues-shield]: https://img.shields.io/github/issues/fio-fsharp/fio.svg?style=for-the-badge
[issues-url]: https://github.com/fio-fsharp/fio/issues
[license-shield]: https://img.shields.io/github/license/fio-fsharp/fio.svg?style=for-the-badge
[license-url]: https://github.com/fio-fsharp/fio/blob/main/LICENSE.md
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/itsdanieldk
[product-screenshot]: images/screenshot.png
[nuget-shield]: https://img.shields.io/nuget/v/FSharp.FIO.svg?style=for-the-badge
[nuget-url]: https://www.nuget.org/packages/FSharp.FIO/0.0.38-alpha
[FSharp]: https://img.shields.io/badge/F%23-378BBA?style=for-the-badge&logo=.NET&logoColor=white
[FSharp-url]: https://fsharp.org/
[.NET]: https://img.shields.io/badge/.NET-5C2D91?style=for-the-badge&logo=.NET&logoColor=white
[.NET-url]: https://dotnet.microsoft.com/en-us/
