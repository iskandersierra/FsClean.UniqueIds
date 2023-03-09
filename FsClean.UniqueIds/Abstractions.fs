namespace FsClean.UniqueIds

open System

type IIdentityBytesGenerator =
    abstract Length : int

    abstract Generate : span: Span<byte> -> unit


type ISpanDecoder =
    abstract GetEncodedLength : inputLength: int -> int

    abstract Decode : input: Span<byte> * output: Span<char> -> unit


type IIdentityCharGenerator =
    abstract Length : int

    abstract Generate : span: Span<char> -> unit


type IIdentityArrayGenerator =
    abstract Length : int

    abstract Generate : span: Span<char> -> unit


type IIdentityGenerator =
    abstract Length : int

    abstract Generate : unit -> string
