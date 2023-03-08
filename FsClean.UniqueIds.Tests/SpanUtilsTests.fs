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

[<Theory>]
[<InlineData('x', "hello", 5, "hello")>]
[<InlineData('x', "hello", 3, "llo")>]
[<InlineData('x', "hello", 8, "xxxhello")>]
[<InlineData('x', "hello", 0, "")>]
let ``copyPadLeft`` (paddingValue: char) (source: string) (targetLength: int) (expected: string) =
    let target = SpanUtils.stackAlloc<char> targetLength
    SpanUtils.copyPadLeft paddingValue (source.AsSpan()) target
    Assert.Equal<char>(expected, target.ToArray())

[<Theory>]
[<InlineData('x', "hello", 5, "hello")>]
[<InlineData('x', "hello", 3, "hel")>]
[<InlineData('x', "hello", 8, "helloxxx")>]
[<InlineData('x', "hello", 0, "")>]
let ``copyPadRight`` (paddingValue: char) (source: string) (targetLength: int) (expected: string) =
    let target = SpanUtils.stackAlloc<char> targetLength
    SpanUtils.copyPadRight paddingValue (source.AsSpan()) target
    Assert.Equal<char>(expected, target.ToArray())

[<Theory>]
[<InlineData(1678275604689L, 8, "00000186C10570D1")>]
[<InlineData(1678275604689L, 5, "86C10570D1")>]
[<InlineData(1678275604689L, 11, "00000000000186C10570D1")>]
[<InlineData(1678275604689L, 0, "")>]
let ``writeInt64Bytes`` (value: int64) (targetLength: int) (expected: string) =
    let target = SpanUtils.stackAlloc<byte> targetLength
    SpanUtils.writeInt64Bytes value target
    let actual = Convert.ToHexString target
    Assert.Equal<char>(expected, actual)
