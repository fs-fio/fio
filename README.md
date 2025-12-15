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
    <!-- An awesome README template to jumpstart your projects! -->
    <!--
    <br />
    <a href="https://github.com/fio-fsharp/fio"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    -->
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
        <li><a href="#usage">Usage</a></li>
        <ul>
          <li><a href="#direct-usage">Direct Usage</a></li>
          <li><a href="#using-fioapp-recommended">Using FIOApp (Recommended)</a></li>
          <li><a href="#alternative-dsl-only-style">Alternative: DSL-Only Style</a></li>
        </ul>
      </ul>
    </li>
    <li>
      <a href="#Benchmarks">Benchmarks</a>
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

> **Note:** FIO is under active development. Contributions, feedback, and questions are very welcome!  
> Feel free to report bugs, request features or [**reach out**](mailto:hey@itsdaniel.dk).


<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

Getting started with **FIO** is simple:

1. Install [**.NET**](https://dotnet.microsoft.com/en-us/)
2. Use an editor like [**VS Code**](https://code.visualstudio.com/), [**Visual Studio**](https://visualstudio.microsoft.com/downloads/), or [**Rider**](https://www.jetbrains.com/rider/download/) (or even vim)
3. Clone this repository
4. Open it in your editor
5. Explore the [**FSharp.FIO.Examples**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples) project or create your own F# file

### Usage

You can use **FIO** in two ways:  
- Directly by creating and running effects manually (examples in [**FSharp.FIO.Examples**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples))
- Via `FIOApp`, which simplifies setup and runtime management (examples in [**FSharp.FIO.Examples.App**](https://github.com/fs-fio/FIO/tree/main/examples/FSharp.FIO.Examples.App))

#### Direct Usage

Create a new F# file and open the DSL, IO and Concurrent runtime modules:

```fsharp
module DirectUsage

open FSharp.FIO.DSL
open FSharp.FIO.Lib.IO
open FSharp.FIO.Runtime.Concurrent

[<EntryPoint>]
let main _ =
    let askForName = fio {
        do! FConsole.PrintLine "Hello! What is your name?"
        let! name = FConsole.ReadLine ()
        do! FConsole.PrintLine $"Hello, %s{name}! Welcome to FIO! 🪻💜"
    }
    
    Runtime().Run askForName
    |> fun fiber -> fiber.Task ()
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> printfn "%A"
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
Hello, Daniel, welcome to FIO! 🪻💜
Ok ()
```

#### Using FIOApp (Recommended)

Wrap your effect in a `FIOApp` to simplify boilerplate. Open the App module:

```fsharp
module FIOAppUsage

open FSharp.FIO.DSL
open FSharp.FIO.Lib.IO
open FSharp.FIO.App

type WelcomeApp() =
    inherit FIOApp<unit, exn> ()

    override _.effect = fio {
        do! FConsole.PrintLine "Hello! What is your name?"
        let! name = FConsole.ReadLine ()
        do! FConsole.PrintLine $"Hello, %s{name}! Welcome to FIO! 🪻💜"
    }

WelcomeApp().Run()
```

Same execution as before:

```
$ dotnet run
```

and same output as well:

```
Hello! What is your name?
Daniel
Hello, Daniel, welcome to FIO! 🪻💜
Ok ()
```

#### Alternative: DSL-Only Style

Prefer DSL chaining? Use bind (>>=) directly:

```fsharp
module DSLOnly

open FSharp.FIO.DSL
open FSharp.FIO.Lib.IO

let askForName =
    FConsole.PrintLine "Hello! What is your name?" >>= fun _ ->
    FConsole.ReadLine () >>= fun name ->
    FConsole.PrintLine $"Hello, %s{name}, welcome to FIO! 🪻💜"
```



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

Contributions are welcome and appreciated!

Got an idea or improvement? Feel free to:
- Star the repository
- Open an issue (tag it with `enhancement`)
- Fork the project and submit a pull request

### Quick Start

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

Distributed under the MIT License See [**LICENSE.md**](LICENSE.md) for more information.

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
[nuget-url]: https://www.nuget.org/packages/FSharp.FIO/0.0.30-alpha
[FSharp]: https://img.shields.io/badge/F%23-378BBA?style=for-the-badge&logo=.NET&logoColor=white
[FSharp-url]: https://fsharp.org/
[.NET]: https://img.shields.io/badge/.NET-5C2D91?style=for-the-badge&logo=.NET&logoColor=white
[.NET-url]: https://dotnet.microsoft.com/en-us/
