module GeneticLbp.Program

open System
open System.Diagnostics
open System.IO
open System.Threading

module PArray = Array.Parallel

type AppSettings = FSharp.Configuration.AppSettings<"App.config">

let private terminationRequested = new ManualResetEvent false
let private terminationCompleted = new ManualResetEvent false
// Having it outside of main seems to be necessary to prevent GC, but must be mutable then.
let mutable private onTerminationRequestDelegate : SafeNativeMethods.ConsoleCloseHandler = null

[<EntryPoint>]
let main argv =
    match CommandLine.parse argv with
    | Choice2Of2 msg ->
        eprintfn "%s" msg
        1
    | Choice1Of2 opt ->
        let outputPath =
            if String.IsNullOrWhiteSpace AppSettings.OutputFolder then
                Directory.GetCurrentDirectory()
            else
                AppSettings.OutputFolder

        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.BelowNormal

        let start = DateTime.Now
        printfn "Starting %s at %s..." <| start.ToShortDateString() <| start.ToShortTimeString()
        printfn "======================================================================\n"

        let a = [|1..10|] |> Array.map box
        let b = [|11..20|] |> Array.map box
        let universe = Universe(a, b)

        let rec periodicSaveAsync really = async {
            if really then
                printfn "Would save."
            do! Async.Sleep (AppSettings.BackupSaveIntervalInMinutes * 60000)
            do! periodicSaveAsync true
        }

        // As this is really a heavy-duty, long-running task, I'd rather have a regular thread.
        let mainComputation = Thread(ParameterizedThreadStart(universe.Simulate), 16 * 1024 * 1024)
        mainComputation.Start(terminationRequested)
        assert(not mainComputation.IsBackground)

        use backups = new CancellationTokenSource()
        Async.Start(periodicSaveAsync false, backups.Token)

        let onTerminationRequest (_:int) =
            // Signal termination
            terminationRequested.Set() |> ignore
            backups.Cancel()
            printfn "Shutting down and saving, please be patient."
            // Wait for cleanup
            terminationCompleted.WaitOne() |> ignore
            // Don't run other handlers, just exit.
            true

        // Having only a local variable here leads to it being GC'd. Don't understand but mutable "outside" variable works.
        onTerminationRequestDelegate <- SafeNativeMethods.ConsoleCloseHandler(onTerminationRequest)
        SafeNativeMethods.SetConsoleCtrlHandler(onTerminationRequestDelegate, true) |> ignore

        // Nothing left to do in main thread. When this comes to an end, it means the user quit.
        mainComputation.Join()

        let stop = DateTime.Now
        printfn "\n======================================================================"
        printfn "Stopped %s at %s..." <| stop.ToShortDateString() <| stop.ToShortTimeString()
        let span = stop - start
        printf "That was "
        if span.Days > 0 then
            printfn "%d days and " span.Days
        let span = span.Subtract(TimeSpan(span.Days, 0, 0, 0))
        printfn "%02d:%02d:%02d hours.\n" span.Hours span.Minutes span.Seconds

        Async.RunSynchronously <| universe.DumpToAsync(Path.Combine(outputPath, sprintf "./genome_%s.txt" <| stop.ToString("yyMMdd_HHmmss")))

        terminationCompleted.Set() |> ignore
        0
