open System
open System.Diagnostics
open System.Text

open Bench
open BenchmarkDotNet.Running

open FsClean.UniqueIds

let testGenerator name (generator: IIdentifierGenerator) =
    let id = generator.Generate()
    printfn "%s: %A" name id

let testCUIDv1Gen() = testGenerator "CUIDv1" CUIDv1IdentifierGenerator.Default

let printSamples() =
    testCUIDv1Gen()
    SpanGenerator.CUID.Counter.getCounter() |> printfn "Counter: %d"
    SpanGenerator.CUID.Fingerprint.getHostNameV1() |> printfn "Host name: %A"
    SpanGenerator.CUID.Fingerprint.generateV1() |> printfn "Fingerprint: %A"

let runBenchmarks() =
    BenchmarkRunner.Run<BenchCUIDv1>() |> ignore

let runProfiler() =
    let bench = BenchCUIDv1()
    bench.Iterations <- 1_000_000
    let stopwatch = Stopwatch.StartNew()
    bench.StringQuick()
    let elapsedTime = stopwatch.Elapsed
    printfn "Elapsed time: %O" elapsedTime

//printSamples()
//runBenchmarks()
runProfiler()
