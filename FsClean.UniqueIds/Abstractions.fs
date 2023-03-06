namespace FsClean.UniqueIds

open System

type ISpanGenerator =
    abstract member CharCount : int

    abstract member Generate : span: Span<byte> -> unit

type IIdentifierGenerator =
    abstract member Generate : unit -> string
