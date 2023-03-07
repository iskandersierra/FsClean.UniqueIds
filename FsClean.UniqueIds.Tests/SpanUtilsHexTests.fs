module SpanUtilsHexTests

open System
open System.Text

open Swensen.Unquote
open Xunit

open FsClean.UniqueIds

let genRandomBytes minLen maxLen =
    let random = Random()
    let size = random.Next(minLen, maxLen)
    Array.init size (fun _ -> byte (random.Next(0, 256)))

[<Theory>]
[<InlineData(-1, -1)>]
[<InlineData(0, 0)>]
[<InlineData(1, 2)>]
[<InlineData(10, 20)>]
[<InlineData(234, 468)>]
let ``getEncodedLength`` (sourceLength: int) (expected: int) =
    if expected < 0 then
        Assert.Throws<ArgumentException> (fun () ->
            SpanUtils.Hex.getEncodedLength sourceLength
            |> ignore)
        |> ignore
    else
        let actual =
            SpanUtils.Hex.getEncodedLength sourceLength

        Assert.Equal(expected, actual)

let sampleVocabulary =
    [| 'a' .. 'p' |]

let encodeFromVocabularyData () =
    seq {
        yield [||], [||]

        yield
            [| 0uy .. 255uy |],
            [| for i = 0uy to 255uy do
                'a' + char (i >>> 4)
                'a' + char (i &&& 0x0Fuy) |]
    }
    |> Seq.map (fun (bytes, expected) -> [| bytes :> obj; expected |])

[<Theory>]
[<MemberData(nameof (encodeFromVocabularyData))>]
let ``encodeFromVocabulary`` (bytes: byte []) (expected: char []) =
    let hexSize =
        SpanUtils.Hex.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<char> hexSize

    SpanUtils.Hex.encodeFromVocabulary (ReadOnlySpan sampleVocabulary) (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<char>(expected, actual)

let toLowerBytesData () =
    seq {
        yield [||], [||]

        yield
            [| 0uy .. 15uy |],
            [| for i = 0uy to 9uy do
                byte '0'
                byte '0' + i
               for i = 0uy to 5uy do
                   byte '0'
                   byte 'a' + i |]

        let bytes = genRandomBytes 1 100

        let expected =
            BitConverter
                .ToString(bytes)
                .Replace("-", "")
                .ToLower()

        let expected = Encoding.ASCII.GetBytes(expected)
        yield bytes, expected
    }
    |> Seq.map (fun (bytes, expected) -> [| bytes :> obj; expected |])

[<Theory>]
[<MemberData(nameof (toLowerBytesData))>]
let ``toLowerBytes`` (bytes: byte []) (expected: byte []) =
    let hexSize =
        SpanUtils.Hex.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<byte> hexSize

    SpanUtils.Hex.toLowerBytes (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toLowerBytesData))>]
let ``toLowerChars`` (bytes: byte []) (expected: byte []) =
    let hexSize =
        SpanUtils.Hex.getEncodedLength bytes.Length

    let resultChars = SpanUtils.stackAlloc<char> hexSize

    SpanUtils.Hex.toLowerChars (ReadOnlySpan bytes) resultChars
    let actual = resultChars.ToArray()
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)

let toUpperBytesData () =
    seq {
        yield [||], [||]

        yield
            [| 0uy .. 15uy |],
            [| for i = 0uy to 9uy do
                byte '0'
                byte '0' + i
               for i = 0uy to 5uy do
                   byte '0'
                   byte 'A' + i |]

        let bytes = genRandomBytes 1 100

        let expected =
            BitConverter
                .ToString(bytes)
                .Replace("-", "")

        let expected = Encoding.ASCII.GetBytes(expected)
        yield bytes, expected
    }
    |> Seq.map (fun (bytes, expected) -> [| bytes :> obj; expected |])

[<Theory>]
[<MemberData(nameof (toUpperBytesData))>]
let ``toUpperBytes`` (bytes: byte []) (expected: byte []) =
    let hexSize =
        SpanUtils.Hex.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<byte> hexSize

    SpanUtils.Hex.toUpperBytes (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toUpperBytesData))>]
let ``toUpperChars`` (bytes: byte []) (expected: byte []) =
    let hexSize =
        SpanUtils.Hex.getEncodedLength bytes.Length

    let resultChars = SpanUtils.stackAlloc<char> hexSize

    SpanUtils.Hex.toUpperChars (ReadOnlySpan bytes) resultChars
    let actual = resultChars.ToArray()
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)
