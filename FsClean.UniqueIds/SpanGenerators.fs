namespace FsClean.UniqueIds

open System
open System.Buffers.Binary
open System.Globalization
open System.Runtime.CompilerServices
open System.Security.Cryptography
open System.Text
open System.Threading

open FsClean.UniqueIds

open IdUtils

[<RequireQualifiedAccess>]
module SpanGenerator =
    module CUID =
        module Counter =
            let counter = ref -1

            let inline getCounter () = Interlocked.Increment(counter)

        module Fingerprint =
            let getHostNameV1 () =
                try
                    Environment.MachineName
                with
                | :? InvalidOperationException -> null
                |> ValueOption.ofObj
                |> ValueOption.defaultWith (fun () -> generateRandomString 8)

            let writeDecimalToSpan (value: int) (span: Span<char>) =
                let str =
                    value.ToString(CultureInfo.InvariantCulture)

                let diff = span.Length - str.Length
                let copyLen = Math.Min(str.Length, span.Length)

                for i = 0 to copyLen - 1 do
                    span.[span.Length - 1 - i] <- str.[str.Length - 1 - i]

                if diff > 0 then
                    for i = 0 to diff do
                        span.[i] <- '0'

            let generateV1 () =
                let hostName = getHostNameV1 ()

                let hostId =
                    hostName.Length
                    + Array.sum (hostName.ToCharArray() |> Array.map int32)

                let pId = Environment.ProcessId

                let str =
                    String.Create(
                        4,
                        (struct (pId, hostId)),
                        fun buffer (struct (pId, hostId)) ->
                            writeDecimalToSpan pId (buffer.Slice(0, 2))
                            writeDecimalToSpan hostId (buffer.Slice(2, 2))
                    )

                Encoding.UTF8.GetBytes(str)

    let private fingerprintV1 = CUID.Fingerprint.generateV1 ()

    [<Obsolete("CUIDv1 is deprecated, use cuidv2. See https://github.com/paralleldrive/cuid#cuid",
               DiagnosticId = "CUID0001")>]
    let cuidv1 (span: Span<byte>) =
        validateSize 25 25 span.Length
        let bytesSpan = stackAlloc<byte> (8)
        let mutable pos = 0

        // CUIDv1 identifier
        span.[pos] <- byte 'c'
        pos <- pos + 1

        // Unix timestamp (in milliseconds)
        let timestamp =
            DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

        BinaryPrimitives.WriteInt64BigEndian(bytesSpan, timestamp)
        encodeBytesToHex false (bytesSpan.Slice(4, 4)) (span.Slice(pos, 8))
        pos <- pos + 8

        // Session counter
        let count = CUID.Counter.getCounter ()
        BinaryPrimitives.WriteInt32BigEndian(bytesSpan.Slice(0, 4), count)
        encodeBytesToHex false (bytesSpan.Slice(2, 2)) (span.Slice(pos, 4))
        pos <- pos + 4

        // Host fingerprint
        fingerprintV1.AsSpan().CopyTo(span.Slice(pos, 4))
        pos <- pos + 4

        // Random bytes
        generateRandomSpan (bytesSpan.Slice(0, 4))
        encodeBytesToHex false (bytesSpan.Slice(0, 4)) (span.Slice(pos, 8))


[<Obsolete("CUIDv1 is deprecated, use cuidv2. See https://github.com/paralleldrive/cuid#cuid", DiagnosticId = "CUID0001")>]
type CUIDv1SpanGenerator() =
    static member val Default = CUIDv1SpanGenerator() with get

    interface ISpanGenerator with
        member __.CharCount = 25

        member __.Generate span = SpanGenerator.cuidv1 span
