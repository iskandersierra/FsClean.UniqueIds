module SpanUtilsRandomTests

open System

open Xunit

open FsClean.UniqueIds

[<Fact>]
let ``writeBytes`` () =
    let span = SpanUtils.stackAlloc<byte> 8

    SpanUtils.Random.writeBytes span
    let actual = Convert.ToHexString(span)

    Assert.Equal(16, actual.Length)
    Assert.NotEqual<string>("0000000000000000", actual)
