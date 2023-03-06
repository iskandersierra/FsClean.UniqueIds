namespace FsClean.UniqueIds

open System
open System.Text

open FsClean.UniqueIds

open IdUtils

module Generator =
    [<Obsolete("CUIDv1 is deprecated, use cuidv2. See https://github.com/paralleldrive/cuid#cuid",
               DiagnosticId = "CUID0001")>]
    let cuidv1 () =
        let bytesSpan = stackAlloc<byte> 25
        SpanGenerator.cuidv1 bytesSpan
        Encoding.ASCII.GetString(bytesSpan)

[<Obsolete("CUIDv1 is deprecated, use cuidv2. See https://github.com/paralleldrive/cuid#cuid", DiagnosticId = "CUID0001")>]
type CUIDv1IdentifierGenerator() =
    static member val Default = CUIDv1IdentifierGenerator() with get

    interface IIdentifierGenerator with
        member this.Generate() = Generator.cuidv1 ()
