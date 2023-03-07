namespace FsClean.UniqueIds.Bench

open System
open System.Text

open BenchmarkDotNet.Attributes

open FsClean.UniqueIds

[<MemoryDiagnoser>]
type SpanUtilsASCIIToChars() =

    let mutable byteArray = Array.empty
    let mutable dummyString = ""

    [<Params(1)>]
    member val Seed = 1 with get, set

    [<Params(256)>]
    member val Size = 256 with get, set

    [<Params(1_000_000)>]
    member val Count = 1_000_000 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let random = Random()
        byteArray <- Array.init this.Size (fun _ -> byte (random.Next(31, 128)))

    [<Benchmark(Baseline = true)>]
    member this.LibToChar() =
        let charSpan = SpanUtils.stackAlloc<char> this.Size
        let bytesSpan = Span.op_Implicit (Span byteArray)
        let count = this.Count

        for _ = 1 to count do
            SpanUtils.ASCII.toChars bytesSpan charSpan

    [<Benchmark>]
    member this.SystemDecodingASCIIChars() =
        let charSpan = SpanUtils.stackAlloc<char> this.Size
        let bytesSpan = Span byteArray
        let count = this.Count
        let decoder = Encoding.ASCII.GetDecoder()

        for _ = 1 to count do
            decoder.GetChars(bytesSpan, charSpan, true)
            |> ignore

[<MemoryDiagnoser>]
type SpanUtilsASCIIToString() =

    let mutable byteArray = Array.empty
    let mutable dummyString = ""

    [<Params(1)>]
    member val Seed = 1 with get, set

    [<Params(256)>]
    member val Size = 256 with get, set

    [<Params(1_000_000)>]
    member val Count = 1_000_000 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let random = Random()
        byteArray <- Array.init this.Size (fun _ -> byte (random.Next(31, 128)))

    [<Benchmark(Baseline = true)>]
    member this.LibToString() =
        let bytesSpan = Span.op_Implicit(Span byteArray)
        let count = this.Count

        for _ = 1 to count do
            dummyString <- SpanUtils.ASCII.toString bytesSpan

    [<Benchmark>]
    member this.SystemDecodingASCIIString() =
        let count = this.Count

        for _ = 1 to count do
            dummyString <- Encoding.ASCII.GetString(byteArray)

[<MemoryDiagnoser>]
type SpanUtilsHexStandard() =

    let mutable byteArray = Array.empty
    let mutable dummyString = ""

    [<Params(1)>]
    member val Seed = 1 with get, set

    [<Params(256)>]
    member val Size = 256 with get, set

    [<Params(1_000_000)>]
    member val Count = 1_000_000 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let random = Random()
        byteArray <- Array.init this.Size (fun _ -> byte (random.Next(0, 256)))

    [<Benchmark(Baseline = true)>]
    member this.LibToLowerChars() =
        let charSpan = SpanUtils.stackAlloc<char> (this.Size * 2)
        let bytesSpan = Span.op_Implicit (Span byteArray)
        let count = this.Count

        for _ = 1 to count do
            SpanUtils.Hex.toLowerChars bytesSpan charSpan

    [<Benchmark>]
    member this.SystemConvertToHexString() =
        let count = this.Count

        for _ = 1 to count do
            dummyString <- Convert.ToHexString(byteArray)

    // [<Benchmark>]
    member this.SystemBitConverterToString() =
        let count = this.Count

        for _ = 1 to count do
            dummyString <- BitConverter.ToString(byteArray).Replace("-", "")

[<MemoryDiagnoser>]
type SpanUtilsBase64Standard() =

    let mutable byteArray = Array.empty
    let mutable dummyString = ""

    [<Params(1)>]
    member val Seed = 1 with get, set

    [<Params(256)>]
    member val Size = 256 with get, set

    [<Params(1_000_000)>]
    member val Count = 1_000_000 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let random = Random()
        byteArray <- Array.init this.Size (fun _ -> byte (random.Next(0, 256)))

    [<Benchmark(Baseline = true)>]
    member this.LibToLowerChars() =
        let charSpan = SpanUtils.stackAlloc<char> (this.Size * 2)
        let bytesSpan = Span.op_Implicit (Span byteArray)
        let count = this.Count

        for _ = 1 to count do
            SpanUtils.Hex.toLowerChars bytesSpan charSpan

    [<Benchmark>]
    member this.SystemConvertToHexString() =
        let count = this.Count

        for _ = 1 to count do
            dummyString <- Convert.ToHexString(byteArray)

    // [<Benchmark>]
    member this.SystemBitConverterToString() =
        let count = this.Count

        for _ = 1 to count do
            dummyString <- BitConverter.ToString(byteArray).Replace("-", "")
