module IdentityBytesGeneratorsTests

open System
open System.Diagnostics

open Xunit

open FsClean.UniqueIds

[<Theory>]
[<InlineData(0, -1)>]
[<InlineData(1, 1)>]
[<InlineData(4, 4)>]
[<InlineData(8, 8)>]
[<InlineData(9, 9)>]
let ``SessionCounterBytesGenerator constructor`` (length: int) (expected: int) =
    if expected = -1 then
        Assert.Throws<ArgumentOutOfRangeException>(fun () -> SessionCounterBytesGenerator(length) |> ignore)
        |> ignore
    else
        let generator =
            SessionCounterBytesGenerator(length) :> IIdentityBytesGenerator

        Assert.Equal(expected, generator.Length)

[<Fact>]
let ``SessionCounterBytesGenerator default length`` () =
    let generator =
        SessionCounterBytesGenerator() :> IIdentityBytesGenerator

    Assert.Equal(8, generator.Length)

[<Theory>]
[<InlineData(1, 0, "00")>]
[<InlineData(1, 1, "01")>]
[<InlineData(1, 42, "2A")>]
[<InlineData(1, 256, "00")>]
[<InlineData(3, 0, "000000")>]
[<InlineData(5, 1, "0000000001")>]
[<InlineData(8, 42, "000000000000002A")>]
[<InlineData(9, 256, "000000000000000100")>]
let ``SessionCounterBytesGenerator.Generate`` (length: int) (skipCount: int) (expected: string) =
    let generator =
        SessionCounterBytesGenerator(length) :> IIdentityBytesGenerator

    let span = SpanUtils.stackAlloc<byte> length

    for _ = 1 to skipCount do
        generator.Generate span

    generator.Generate span
    let actual = Convert.ToHexString(span)
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(0, -1)>]
[<InlineData(1, 1)>]
[<InlineData(4, 4)>]
[<InlineData(8, 8)>]
[<InlineData(9, 9)>]
let ``TimestampBytesGenerator constructor`` (length: int) (expected: int) =
    if expected = -1 then
        Assert.Throws<ArgumentOutOfRangeException>(fun () -> TimestampBytesGenerator(length) |> ignore)
        |> ignore
    else
        let generator =
            TimestampBytesGenerator(length) :> IIdentityBytesGenerator

        Assert.Equal(expected, generator.Length)

[<Fact>]
let ``TimestampBytesGenerator default length`` () =
    let generator =
        TimestampBytesGenerator() :> IIdentityBytesGenerator

    Assert.Equal(8, generator.Length)

[<Theory>]
[<InlineData(1, 5051223264702L, "BE")>]
[<InlineData(3, 5051223264702L, "5E4DBE")>]
[<InlineData(5, 5051223264702L, "98145E4DBE")>]
[<InlineData(8, 5051223264702L, "00000498145E4DBE")>]
[<InlineData(9, 5051223264702L, "0000000498145E4DBE")>]
let ``TimestampBytesGenerator.Generate`` (length: int) (timestamp: int64) (expected: string) =
    let generator =
        TimestampBytesGenerator(length, fun () -> timestamp) :> IIdentityBytesGenerator

    let span = SpanUtils.stackAlloc<byte> length

    generator.Generate span
    let actual = Convert.ToHexString(span)
    Assert.Equal(expected, actual)

[<Fact>]
let ``TimestampBytesGenerator default timestamp`` () =
    let generator =
        TimestampBytesGenerator(8) :> IIdentityBytesGenerator

    let span = SpanUtils.stackAlloc<byte> generator.Length

    let minExpected = Stopwatch.GetTimestamp().ToString("X16")
    generator.Generate span
    let maxExpected = Stopwatch.GetTimestamp().ToString("X16")

    let actual = Convert.ToHexString(span)
    Assert.True(actual >= minExpected)
    Assert.True(actual <= maxExpected)
