module SpanUtilsBase64Tests

open System

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
[<InlineData(2, 3)>]
[<InlineData(3, 4)>]
[<InlineData(9, 12)>]
[<InlineData(31, 42)>]
[<InlineData(62, 83)>]
[<InlineData(93, 124)>]
let ``getEncodedLength`` (sourceLength: int) (expected: int) =
    if expected < 0 then
        Assert.Throws<ArgumentException> (fun () ->
            SpanUtils.Base64.getEncodedLength sourceLength
            |> ignore)
        |> ignore
    else
        let actual =
            SpanUtils.Base64.getEncodedLength sourceLength

        Assert.Equal(expected, actual)

let sampleVocabulary = [| char 32uy .. char (32uy + 63uy) |]

let encodeFromVocabularyData () =
    seq {
        yield [||], [||]
        yield [| 213uy |], [| 'U'; '0' |]
        yield [| 213uy; 87uy |], [| 'U'; '5'; '<' |]
        yield [| 213uy; 87uy; 192uy |], [| 'U'; '5'; '?'; ' ' |]
        yield [| 213uy; 87uy; 192uy; 23uy |], [| 'U'; '5'; '?'; ' '; '%'; 'P' |]
    }
    |> Seq.map (fun (bytes, expected) -> [| bytes :> obj; expected |])

[<Theory>]
[<MemberData(nameof (encodeFromVocabularyData))>]
let ``encodeFromVocabulary`` (bytes: byte []) (expected: char []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<char> base64Size

    SpanUtils.Base64.encodeFromVocabulary (ReadOnlySpan sampleVocabulary) (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<char>(expected, actual)

[<Fact>]
let ``encodeFromVocabulary with shorter vocabulary`` () =
    let vocabulary = [| 'a' .. 'o' |]
    let bytes = [| 1uy .. 20uy |]

    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    Assert.Throws<ArgumentException> (fun () ->
        let resultBytes = SpanUtils.stackAlloc<char> base64Size

        SpanUtils.Base64.encodeFromVocabulary (ReadOnlySpan vocabulary) (ReadOnlySpan bytes) resultBytes
        |> ignore)
    |> ignore

[<Fact>]
let ``encodeFromVocabulary with longer vocabulary`` () =
    let vocabulary = [| 'a' .. 'q' |]
    let bytes = [| 1uy .. 20uy |]

    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    Assert.Throws<ArgumentException> (fun () ->
        let resultBytes = SpanUtils.stackAlloc<char> base64Size

        SpanUtils.Base64.encodeFromVocabulary (ReadOnlySpan vocabulary) (ReadOnlySpan bytes) resultBytes
        |> ignore)
    |> ignore

[<Fact>]
let ``encodeFromVocabulary with shorter target`` () =
    let vocabulary = [| char 32uy .. char (32uy + 63uy) |]
    let bytes = [| 1uy .. 20uy |]

    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length - 1

    Assert.Throws<ArgumentException> (fun () ->
        let resultBytes = SpanUtils.stackAlloc<char> base64Size

        SpanUtils.Base64.encodeFromVocabulary (ReadOnlySpan vocabulary) (ReadOnlySpan bytes) resultBytes
        |> ignore)
    |> ignore

[<Theory>]
[<MemberData(nameof (encodeFromVocabularyData))>]
let ``toArrayFromVocabulary`` (bytes: byte []) (expected: char []) =
    let actual = SpanUtils.Base64.toArrayFromVocabulary (ReadOnlySpan sampleVocabulary) (ReadOnlySpan bytes)
    Assert.Equal<char>(expected, actual)

[<Theory>]
[<MemberData(nameof (encodeFromVocabularyData))>]
let ``toStringFromVocabulary`` (bytes: byte []) (expected: char []) =
    let actual = SpanUtils.Base64.toStringFromVocabulary (ReadOnlySpan sampleVocabulary) (ReadOnlySpan bytes)
    Assert.Equal<char>(expected, actual)

let toStandardBytesData () =
    seq {
        yield [||], [||]
        yield [| 213uy |], [| '1'; 'Q' |]
        yield [| 213uy; 87uy |], [| '1'; 'V'; 'c' |]
        yield [| 213uy; 87uy; 192uy |], [| '1'; 'V'; 'f'; 'A' |]
        yield [| 213uy; 87uy; 192uy; 23uy |], [| '1'; 'V'; 'f'; 'A'; 'F'; 'w' |]
        yield [| 03uy; 239uy; 193uy |], [| 'A'; '+'; '/'; 'B' |]
    }
    |> Seq.map (fun (bytes, expected) ->
        [| bytes :> obj
           expected |> Array.map byte :> obj |])

[<Theory>]
[<MemberData(nameof (toStandardBytesData))>]
let ``toStandardBytes`` (bytes: byte []) (expected: byte []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<byte> base64Size

    SpanUtils.Base64.toStandardBytes (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toStandardBytesData))>]
let ``toStandardChars`` (bytes: byte []) (expected: byte []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultChars = SpanUtils.stackAlloc<char> base64Size

    SpanUtils.Base64.toStandardChars (ReadOnlySpan bytes) resultChars
    let actual = resultChars.ToArray()
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)

[<Theory>]
[<MemberData(nameof (toStandardBytesData))>]
let ``toStandardArray`` (bytes: byte []) (expected: byte []) =
    let actual = SpanUtils.Base64.toStandardArray (ReadOnlySpan bytes)
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toStandardBytesData))>]
let ``toStandardString`` (bytes: byte []) (expected: byte []) =
    let actual = SpanUtils.Base64.toStandardString (ReadOnlySpan bytes)
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)

let toUrlSafeBytesData () =
    seq {
        yield [||], [||]
        yield [| 213uy |], [| '1'; 'Q' |]
        yield [| 213uy; 87uy |], [| '1'; 'V'; 'c' |]
        yield [| 213uy; 87uy; 192uy |], [| '1'; 'V'; 'f'; 'A' |]
        yield [| 213uy; 87uy; 192uy; 23uy |], [| '1'; 'V'; 'f'; 'A'; 'F'; 'w' |]
        yield [| 03uy; 239uy; 193uy |], [| 'A'; '-'; '_'; 'B' |]
    }
    |> Seq.map (fun (bytes, expected) ->
        [| bytes :> obj
           expected |> Array.map byte :> obj |])

[<Theory>]
[<MemberData(nameof (toUrlSafeBytesData))>]
let ``toUrlSafeBytes`` (bytes: byte []) (expected: byte []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<byte> base64Size

    SpanUtils.Base64.toUrlSafeBytes (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toUrlSafeBytesData))>]
let ``toUrlSafeChars`` (bytes: byte []) (expected: byte []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultChars = SpanUtils.stackAlloc<char> base64Size

    SpanUtils.Base64.toUrlSafeChars (ReadOnlySpan bytes) resultChars
    let actual = resultChars.ToArray()
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)

[<Theory>]
[<MemberData(nameof (toUrlSafeBytesData))>]
let ``toUrlSafeArray`` (bytes: byte []) (expected: byte []) =
    let actual = SpanUtils.Base64.toUrlSafeArray (ReadOnlySpan bytes)
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toUrlSafeBytesData))>]
let ``toUrlSafeString`` (bytes: byte []) (expected: byte []) =
    let actual = SpanUtils.Base64.toUrlSafeString (ReadOnlySpan bytes)
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)

let toIdentifierSafeBytesData () =
    seq {
        yield [||], [||]
        yield [| 213uy |], [| '1'; 'Q' |]
        yield [| 213uy; 87uy |], [| '1'; 'V'; 'c' |]
        yield [| 213uy; 87uy; 192uy |], [| '1'; 'V'; 'f'; 'A' |]
        yield [| 213uy; 87uy; 192uy; 23uy |], [| '1'; 'V'; 'f'; 'A'; 'F'; 'w' |]
        yield [| 03uy; 239uy; 193uy |], [| 'A'; 'A'; 'a'; 'B' |]
    }
    |> Seq.map (fun (bytes, expected) ->
        [| bytes :> obj
           expected |> Array.map byte :> obj |])

[<Theory>]
[<MemberData(nameof (toIdentifierSafeBytesData))>]
let ``toIdentifierSafeBytes`` (bytes: byte []) (expected: byte []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultBytes = SpanUtils.stackAlloc<byte> base64Size

    SpanUtils.Base64.toIdentifierSafeBytes (ReadOnlySpan bytes) resultBytes
    let actual = resultBytes.ToArray()
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toIdentifierSafeBytesData))>]
let ``toIdentifierSafeChars`` (bytes: byte []) (expected: byte []) =
    let base64Size =
        SpanUtils.Base64.getEncodedLength bytes.Length

    let resultChars = SpanUtils.stackAlloc<char> base64Size

    SpanUtils.Base64.toIdentifierSafeChars (ReadOnlySpan bytes) resultChars
    let actual = resultChars.ToArray()
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)

[<Theory>]
[<MemberData(nameof (toIdentifierSafeBytesData))>]
let ``toIdentifierSafeArray`` (bytes: byte []) (expected: byte []) =
    let actual = SpanUtils.Base64.toIdentifierSafeArray (ReadOnlySpan bytes)
    Assert.Equal<byte>(expected, actual)

[<Theory>]
[<MemberData(nameof (toIdentifierSafeBytesData))>]
let ``toIdentifierSafeString`` (bytes: byte []) (expected: byte []) =
    let actual = SpanUtils.Base64.toIdentifierSafeString (ReadOnlySpan bytes)
    let expected = expected |> Array.map char
    Assert.Equal<char>(expected, actual)
