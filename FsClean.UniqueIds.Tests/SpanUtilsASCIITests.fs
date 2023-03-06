module SpanUtilsASCIITests

open System
open System.Text

open Swensen.Unquote
open Xunit

open FsClean.UniqueIds

[<Fact>]
let ``toChars`` () =
    let span = SpanUtils.stackAlloc<byte> 128
    for i = 1 to 128 do
        span.[i - 1] <- byte i
    let chars = SpanUtils.stackAlloc<char> 128
    SpanUtils.ASCII.toChars (Span.op_Implicit span) chars
    let actual = String(chars)
    let expected = Encoding.ASCII.GetString(span)
    test <@ actual = expected @>

[<Fact>]
let ``toString`` () =
    let span = SpanUtils.stackAlloc<byte> 128
    for i = 1 to 128 do
        span.[i - 1] <- byte i
    let actual = SpanUtils.ASCII.toString (Span.op_Implicit span)
    let expected = Encoding.ASCII.GetString(span)
    test <@ actual = expected @>
    