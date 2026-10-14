module FIO.Tests.Shared.ChildProcessRunner

open System
open System.Threading
open System.Diagnostics
open System.Collections.Concurrent

type private Marker = class end

// Re-runs the enclosing test assembly as a child process, for behaviour that needs a process of its own.
type ChildProcess(args: string list) =
    let lines = ConcurrentQueue<string>()

    let host =
        match Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
        | null
        | "" -> "dotnet"
        | path -> path

    let info =
        ProcessStartInfo(host, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)

    do
        info.ArgumentList.Add "exec"
        info.ArgumentList.Add typeof<Marker>.Assembly.Location
        info.ArgumentList.Add "--child"

        for arg in args do
            info.ArgumentList.Add arg

    let proc = Process.Start info

    do
        proc.OutputDataReceived.Add(fun args -> if not (isNull args.Data) then lines.Enqueue args.Data)
        proc.ErrorDataReceived.Add(fun args -> if not (isNull args.Data) then lines.Enqueue $"stderr: {args.Data}")
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()

    member _.Output : string list =
        List.ofSeq lines

    member _.WaitForLine (expected: string) (timeout: TimeSpan) : bool =
        let stopwatch = Stopwatch.StartNew()

        while not (lines |> Seq.contains expected) && not proc.HasExited && stopwatch.Elapsed < timeout do
            Thread.Sleep 20

        lines |> Seq.contains expected

    member _.Signal (name: string) : unit =
        use kill = Process.Start("kill", $"-{name} {proc.Id}")
        kill.WaitForExit()

    member _.WaitForExit (timeout: TimeSpan) : int option =
        if proc.WaitForExit(int timeout.TotalMilliseconds) then
            proc.WaitForExit()
            Some proc.ExitCode
        else
            None

    interface IDisposable with
        member _.Dispose () =
            if not proc.HasExited then
                try proc.Kill true with _ -> ()

            proc.Dispose()
