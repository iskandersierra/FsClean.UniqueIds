module SpanUtilsTimestampTests

open System

open Xunit

open FsClean.UniqueIds

[<Fact>]
let ``getValue`` () =
    let minExpected = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()
    let actual = SpanUtils.Timestamp.getValue ()
    let maxExpected = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

    Assert.True(actual >= minExpected)
    Assert.True(actual <= maxExpected)

[<Fact>]
let ``writeBytes`` () =
    let span = SpanUtils.stackAlloc<byte> 8
    
    SpanUtils.writeInt64Bytes (DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()) span
    let minExpected = Convert.ToHexString(span)
    
    SpanUtils.Timestamp.writeBytes span
    let actual = Convert.ToHexString(span)
    
    SpanUtils.writeInt64Bytes (DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()) span
    let maxExpected = Convert.ToHexString(span)

    Assert.True(actual >= minExpected)
    Assert.True(actual <= maxExpected)
