[<RequireQualifiedAccess>]
module FsClean.UniqueIds.SpanUtils

open System
open System.Buffers.Binary
open System.Security.Cryptography
open System.Text
open System.Threading

open Microsoft.FSharp.NativeInterop

#nowarn "9"
#nowarn "51"
#nowarn "3391"

/// <summary>
/// Allocates a stack-allocated span of values of given type.
/// </summary>
/// <param name="size">The number of values to allocate.</param>
/// <returns>A stack-allocated span of values of given type.</returns>
/// <remarks>
/// Parameter size must be a positive integer that fits into the thread stack.
/// </remarks>
/// <see href="https://learn.microsoft.com/en-us/dotnet/fsharp/whats-new/fsharp-45#span-and-byref-like-structs"/>
let inline stackAlloc<'t when 't: unmanaged> (size: int) =
    let nativeBytes = NativePtr.stackalloc<'t> size
    let nativePtr = NativePtr.toVoidPtr nativeBytes
    let bytesSpan = Span<'t>(nativePtr, size)
    bytesSpan

let copyPadLeft<'t when 't: unmanaged> (paddingValue: 't) (source: ReadOnlySpan<'t>) (target: Span<'t>) =
    let sourceLength = source.Length
    let targetLength = target.Length

    if sourceLength >= targetLength then
        source
            .Slice(sourceLength - targetLength)
            .CopyTo(target)
    else
        let paddingLength = targetLength - sourceLength
        target.Slice(0, paddingLength).Fill paddingValue
        source.CopyTo(target.Slice(paddingLength))

let copyPadRight<'t when 't: unmanaged> (paddingValue: 't) (source: ReadOnlySpan<'t>) (target: Span<'t>) =
    let sourceLength = source.Length
    let targetLength = target.Length

    if sourceLength >= targetLength then
        source.Slice(0, targetLength).CopyTo(target)
    else
        source.CopyTo(target.Slice(0, sourceLength))
        target.Slice(sourceLength).Fill paddingValue

let writeInt64Bytes (value: int64) (span: Span<byte>) =
    let bytesSpan = stackAlloc<byte> 8
    BinaryPrimitives.WriteInt64BigEndian(bytesSpan, value)
    copyPadLeft<byte> 0uy (bytesSpan) span

[<RequireQualifiedAccess>]
module ASCII =
    /// <summary>
    /// Converts a span of bytes to a span of chars, where all characters are ASCII.
    /// </summary>
    /// <param name="source">The source span of bytes to convert.</param>
    /// <param name="target">The target span of chars to convert to.</param>
    /// <remarks>
    /// This function is intended to be performant, so it does not validate the input.
    /// </remarks>
    let inline toChars (source: ReadOnlySpan<byte>) (target: Span<char>) =
        Encoding.ASCII.GetChars(source, target) |> ignore

    /// <summary>
    /// Converts a span of bytes to a string where all characters are ASCII.
    /// </summary>
    /// <param name="source">The source span of bytes to convert.</param>
    /// <returns>A string where all characters are ASCII or invalid character '?'.</returns>
    /// <remarks>
    /// This function is intended to be performant, so it does not validate the input.
    /// </remarks>
    let inline toString (source: ReadOnlySpan<byte>) = Encoding.ASCII.GetString(source)

[<RequireQualifiedAccess>]
module Hex =
    /// <summary>
    /// Computes the length of the target span for encoding a given number of bytes to HEX.
    /// </summary>
    /// <param name="sourceLength">The number of bytes to encode.</param>
    /// <returns>The length of the target span for encoding a given number of bytes to HEX.</returns>
    /// <exception cref="System.ArgumentException">Thrown when sourceLength is less than zero.</exception>
    /// <seeAlso cref="encodeFromVocabulary"/>
    let getEncodedLength (sourceLength: int) =
        if sourceLength < 0 then
            invalidArg "size" "size must not be less than zero"
        else
            sourceLength * 2

    /// <summary>
    /// Encodes a span of bytes to a span of HEX ascii characters, also as bytes.
    /// </summary>
    /// <param name="vocabulary">The vocabulary to use for encoding. MUST be 16 bytes long.</param>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be twice as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// </remarks>
    let encodeFromVocabulary<'t when 't: unmanaged>
        (vocabulary: ReadOnlySpan<'t>)
        (source: ReadOnlySpan<byte>)
        (target: Span<'t>)
        =
        if vocabulary.Length <> 16 then
            invalidArg "vocabulary" "vocabulary must be 16 bytes long"

        if source.Length <<< 1 > target.Length then
            invalidArg "target" "target must be at least twice as long as source"

        let last = source.Length - 1
        let vocabularyPtr = &&vocabulary.GetPinnableReference()
        let sourcePtr = &&source.GetPinnableReference()
        let targetPtr = &&target.GetPinnableReference()

        let mutable targetIndex = 0

        for index = 0 to last do
            let b = NativePtr.get sourcePtr index

            NativePtr.get vocabularyPtr (int (b >>> 4))
            |> NativePtr.set targetPtr targetIndex

            NativePtr.get vocabularyPtr (int (b &&& 0x0Fuy))
            |> NativePtr.set targetPtr (targetIndex + 1)

            targetIndex <- targetIndex + 2

    /// <summary>
    /// Encodes a span of bytes to an array of HEX ascii characters.
    /// </summary>
    /// <param name="vocabulary">The vocabulary to use for encoding. MUST be 16 bytes long.</param>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// </remarks>
    let toArrayFromVocabulary<'t when 't: unmanaged> (vocabulary: ReadOnlySpan<'t>) (source: ReadOnlySpan<byte>) =
        let targetLength = getEncodedLength source.Length
        let target = stackAlloc<'t> targetLength
        encodeFromVocabulary vocabulary source target
        target.ToArray()

    /// <summary>
    /// Encodes a span of bytes to a string of HEX ascii characters.
    /// </summary>
    /// <param name="vocabulary">The vocabulary to use for encoding. MUST be 16 bytes long.</param>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// </remarks>
    let toStringFromVocabulary (vocabulary: ReadOnlySpan<char>) (source: ReadOnlySpan<byte>) =
        let targetLength = getEncodedLength source.Length
        let target = stackAlloc<char> targetLength
        encodeFromVocabulary vocabulary source target
        String(target)

    let private LowerCaseChars =
        [| for ch = '0' to '9' do
            yield ch
           for ch = 'a' to 'f' do
               yield ch |]

    let private UpperCaseChars =
        [| for ch = '0' to '9' do
            yield ch
           for ch = 'A' to 'F' do
               yield ch |]

    let private LowerCaseBytes =
        [| for ch in LowerCaseChars do
               yield byte (int ch) |]

    let private UpperCaseBytes =
        [| for ch in UpperCaseChars do
               yield byte (int ch) |]

    /// <summary>
    /// Encodes a span of bytes to a span of lower-case HEX ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be twice as long as the source.</param>
    let toLowerBytes (source: ReadOnlySpan<byte>) (target: Span<byte>) =
        let vocabulary = ReadOnlySpan<_>(LowerCaseBytes)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to a span of upper-case HEX ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be twice as long as the source.</param>
    let toUpperBytes (source: ReadOnlySpan<byte>) (target: Span<byte>) =
        let vocabulary = ReadOnlySpan<_>(UpperCaseBytes)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to a span of lower-case HEX ascii characters, but as chars.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of chars to encode to. MUST be twice as long as the source.</param>
    let toLowerChars (source: ReadOnlySpan<byte>) (target: Span<char>) =
        let vocabulary = ReadOnlySpan<_>(LowerCaseChars)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to a span of upper-case HEX ascii characters, but as chars.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of chars to encode to. MUST be twice as long as the source.</param>
    let toUpperChars (source: ReadOnlySpan<byte>) (target: Span<char>) =
        let vocabulary = ReadOnlySpan<_>(UpperCaseChars)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to an array of lower-case HEX ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be twice as long as the source.</param>
    let toLowerArray (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(LowerCaseBytes)
        toArrayFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to an array of upper-case HEX ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    let toUpperArray (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(UpperCaseBytes)
        toArrayFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a string of lower-case HEX ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    let toLowerString (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(LowerCaseChars)
        toStringFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a string of upper-case HEX ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    let toUpperString (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(UpperCaseChars)
        toStringFromVocabulary vocabulary source

[<RequireQualifiedAccess>]
module Base64 =
    /// <summary>
    /// Computes the length of the target span for encoding a given number of bytes to base 64.
    /// </summary>
    /// <param name="sourceLength">The number of bytes to encode.</param>
    /// <returns>The length of the target span for encoding a given number of bytes to base 64.</returns>
    /// <exception cref="System.ArgumentException">Thrown when sourceLength is less than zero.</exception>
    /// <see href="https://en.wikipedia.org/wiki/Base64"/>
    /// <seeAlso cref="encodeToBase64From"/>
    let getEncodedLength (sourceLength: int) =
        if sourceLength < 0 then
            invalidArg "size" "size must not be less than zero"

        let extra = sourceLength % 3
        let extra = extra + sign extra

        (sourceLength / 3) * 4 + extra

    /// <summary>
    /// Encodes a span of bytes to a span of base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="vocabulary">The vocabulary to use for encoding. MUST be 64 bytes long.</param>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// </remarks>
    /// <see href="https://en.wikipedia.org/wiki/Base64"/>
    let encodeFromVocabulary<'t when 't: unmanaged>
        (vocabulary: ReadOnlySpan<'t>)
        (source: ReadOnlySpan<byte>)
        (target: Span<'t>)
        =
        let sourceLength = source.Length

        if vocabulary.Length <> 64 then
            invalidArg "vocabulary" "vocabulary must be 64 bytes long"

        if target.Length < getEncodedLength sourceLength then
            invalidArg "target" $"target must be at least {getEncodedLength sourceLength} bytes long"

        let lastWholeStep = sourceLength / 3 - 1

        let mutable fromPos = 0
        let mutable pos = 0

        for _ = 0 to lastWholeStep do
            let b1 = source.[fromPos]
            let b2 = source.[fromPos + 1]
            let b3 = source.[fromPos + 2]

            target.[pos] <- vocabulary.[int (b1 >>> 2)]
            target.[pos + 1] <- vocabulary.[int (((b1 &&& 0x03uy) <<< 4) ||| (b2 >>> 4))]
            target.[pos + 2] <- vocabulary.[int (((b2 &&& 0x0Fuy) <<< 2) ||| (b3 >>> 6))]
            target.[pos + 3] <- vocabulary.[int (b3 &&& 0x3Fuy)]

            fromPos <- fromPos + 3
            pos <- pos + 4

        let remaining = sourceLength - fromPos

        if remaining = 1 then
            let b1 = source.[fromPos]

            target.[pos] <- vocabulary.[int (b1 >>> 2)]
            target.[pos + 1] <- vocabulary.[int ((b1 &&& 0x03uy) <<< 4)]

        elif remaining = 2 then
            let b1 = source.[fromPos]
            let b2 = source.[fromPos + 1]

            target.[pos] <- vocabulary.[int (b1 >>> 2)]
            target.[pos + 1] <- vocabulary.[int (((b1 &&& 0x03uy) <<< 4) ||| (b2 >>> 4))]
            target.[pos + 2] <- vocabulary.[int ((b2 &&& 0x0Fuy) <<< 2)]

    /// <summary>
    /// Encodes a span of bytes to an array of base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="vocabulary">The vocabulary to use for encoding. MUST be 64 bytes long.</param>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// </remarks>
    /// <see href="https://en.wikipedia.org/wiki/Base64"/>
    let toArrayFromVocabulary<'t when 't: unmanaged> (vocabulary: ReadOnlySpan<'t>) (source: ReadOnlySpan<byte>) =
        let targetLength = getEncodedLength source.Length
        let target = stackAlloc<'t> targetLength
        encodeFromVocabulary vocabulary source target
        target.ToArray()

    /// <summary>
    /// Encodes a span of bytes to a string of base 64 ascii characters.
    /// </summary>
    /// <param name="vocabulary">The vocabulary to use for encoding. MUST be 64 bytes long.</param>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// </remarks>
    /// <see href="https://en.wikipedia.org/wiki/Base64"/>
    let toStringFromVocabulary (vocabulary: ReadOnlySpan<char>) (source: ReadOnlySpan<byte>) =
        let targetLength = getEncodedLength source.Length
        let target = stackAlloc<char> targetLength
        encodeFromVocabulary vocabulary source target
        target.ToArray()

    let private SafeChars =
        [| for ch = 'A' to 'Z' do
            yield ch
           for ch = 'a' to 'z' do
               yield ch
           for ch = '0' to '9' do
               yield ch |]

    let private StandardChars =
        [| yield! SafeChars
           yield '+'
           yield '/' |]

    let private UrlSafeChars =
        [| yield! SafeChars
           yield '-'
           yield '_' |]

    let private IdentifierSafeChars =
        [| yield! SafeChars
           yield 'A'
           yield 'a' |]

    let private StandardBytes =
        [| for ch in StandardChars do
               yield byte ch |]

    let private UrlSafeBytes =
        [| for ch in UrlSafeChars do
               yield byte ch |]

    let private IdentifierSafeBytes =
        [| for ch in IdentifierSafeChars do
               yield byte ch |]

    /// <summary>
    /// Encodes a span of bytes to a span of standard base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toStandardBytes (source: ReadOnlySpan<byte>) (target: Span<byte>) =
        let vocabulary = ReadOnlySpan<_>(StandardBytes)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to a span of standard base 64 ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toStandardChars (source: ReadOnlySpan<byte>) (target: Span<char>) =
        let vocabulary = ReadOnlySpan<_>(StandardChars)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to an array of standard base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toStandardArray (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(StandardBytes)
        toArrayFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a string of standard base 64 ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toStandardString (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(StandardChars)
        toStringFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a span of URL-safe base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toUrlSafeBytes (source: ReadOnlySpan<byte>) (target: Span<byte>) =
        let vocabulary = ReadOnlySpan<_>(UrlSafeBytes)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to a span of URL-safe base 64 ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toUrlSafeChars (source: ReadOnlySpan<byte>) (target: Span<char>) =
        let vocabulary = ReadOnlySpan<_>(UrlSafeChars)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to an array of URL-safe base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toUrlSafeArray (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(UrlSafeBytes)
        toArrayFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a string of URL-safe base 64 ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toUrlSafeString (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(UrlSafeChars)
        toStringFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a span of URL-safe base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toIdentifierSafeBytes (source: ReadOnlySpan<byte>) (target: Span<byte>) =
        let vocabulary = ReadOnlySpan<_>(IdentifierSafeBytes)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to a span of URL-safe base 64 ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <param name="target">The target span of bytes to encode to. MUST be 4/3 times as long as the source.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toIdentifierSafeChars (source: ReadOnlySpan<byte>) (target: Span<char>) =
        let vocabulary = ReadOnlySpan<_>(IdentifierSafeChars)
        encodeFromVocabulary vocabulary source target

    /// <summary>
    /// Encodes a span of bytes to an array of URL-safe base 64 ascii characters, also as bytes.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toIdentifierSafeArray (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(IdentifierSafeBytes)
        toArrayFromVocabulary vocabulary source

    /// <summary>
    /// Encodes a span of bytes to a string of URL-safe base 64 ascii characters.
    /// </summary>
    /// <param name="source">The source span of bytes to encode.</param>
    /// <remarks>
    /// Because it is intended to produce IDs, the vocabulary is not checked for uniqueness.
    /// For the same reason, the target span is not padded with '=' characters.
    /// Use <see cref="getEncodedLength"/> to compute the length of the target span.
    /// </remarks>
    let toIdentifierSafeString (source: ReadOnlySpan<byte>) =
        let vocabulary = ReadOnlySpan<_>(IdentifierSafeChars)
        toStringFromVocabulary vocabulary source

[<RequireQualifiedAccess>]
module Random =
    let inline writeBytes (span: Span<byte>) = RandomNumberGenerator.Fill span

[<RequireQualifiedAccess>]
module ProcessFingerprint =
    let createHostFingerprint () =
        let hostName =
            try
                Environment.MachineName
            with
            | _ -> ""

        let hostName =
            if hostName = "" then
                let bytes = stackAlloc<byte> 6
                Random.writeBytes bytes
                Hex.toLowerString bytes
            else
                hostName

        hostName.GetHashCode()

    let createProcessFingerprint () = Environment.ProcessId

    let createFingerprint () =
        let hash = HashCode()
        hash.Add(createHostFingerprint ())
        hash.Add(createProcessFingerprint ())
        hash.ToHashCode()

    let private fingerprint = createFingerprint ()

    let getValue () = fingerprint

    let inline writeBytes (span: Span<byte>) =
        writeInt64Bytes (getValue ()) span
