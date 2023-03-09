namespace Bench

//open System
//open System.Text

//open BenchmarkDotNet
//open BenchmarkDotNet.Attributes
//open Microsoft.FSharp.NativeInterop
//open Xaevik.Cuid

//open FsClean.UniqueIds

//#nowarn "9"

//[<MemoryDiagnoser>]
//type BenchCUIDv1() =
//    let spanGenerator: ISpanGenerator = CUIDv1SpanGenerator.Default
//    let generator: IIdentifierGenerator = CUIDv1IdentifierGenerator.Default

//    let mutable dummyId = ""

//    [<Params(1_000_000)>]
//    member val Iterations: int = 0 with get, set

//    [<Benchmark()>]
//    member this.SpanGenerator() =
//        let last = this.Iterations - 1
//        let nativeBytes = NativePtr.stackalloc<byte> 25
//        let nativePtr = NativePtr.toVoidPtr nativeBytes
//        let bytesSpan = Span<byte>(nativePtr, 25)
//        for _ = last downto 0 do
//            spanGenerator.Generate(bytesSpan)

//    [<Benchmark()>]
//    member this.SpanQuick() =
//        let last = this.Iterations - 1
//        let nativeBytes = NativePtr.stackalloc<byte> 25
//        let nativePtr = NativePtr.toVoidPtr nativeBytes
//        let bytesSpan = Span<byte>(nativePtr, 25)
//        for _ = last downto 0 do
//            SpanGenerator.cuidv1 bytesSpan

//    [<Benchmark(Baseline = true)>]
//    member this.StringGenerator() =
//        let last = this.Iterations - 1
//        for _ = last downto 0 do
//            dummyId <- generator.Generate()

//    [<Benchmark()>]
//    member this.StringQuick() =
//        let last = this.Iterations - 1
//        for _ = last downto 0 do
//            dummyId <- Generator.cuidv1()

//    [<Benchmark>]
//    member this.CuidNet() =
//        let last = this.Iterations - 1
//        for _ = last downto 0 do
//            dummyId <- Cuid.NewCuid().ToString()

//    [<Benchmark>]
//    member this.SystemGuid() =
//        let last = this.Iterations - 1
//        for _ = last downto 0 do
//            dummyId <- Guid.NewGuid().ToString("N")
