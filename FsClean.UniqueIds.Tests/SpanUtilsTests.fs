module SpanUtilsTests

open System

open Xunit

open FsClean.UniqueIds

[<Theory>]
[<InlineData(1)>]
[<InlineData(10)>]
[<InlineData(100)>]
let ``stackAlloc<byte>`` (size: int) =
    let span = SpanUtils.stackAlloc<byte> size
    Assert.Equal(size, span.Length)
    for i = 0 to size - 1 do
        Assert.Equal(0uy, span.[i])
