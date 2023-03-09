namespace FsClean.UniqueIds

open System
open System.Diagnostics
open System.Threading

type SessionCounterBytesGenerator(length) =
    do
        if length < 1 then
            raise (new ArgumentOutOfRangeException("length", "Must be greater than 0"))

    let mutable counter = -1L

    let getValue () = Interlocked.Increment(&counter)

    new() =
        SessionCounterBytesGenerator(8)

    member this.Length = length

    member this.Generate span = SpanUtils.writeInt64Bytes (getValue ()) span

    interface IIdentityBytesGenerator with
        member this.Length = this.Length

        member this.Generate span = this.Generate span

type TimestampBytesGenerator(length, [<InlineIfLambda>] getTimestamp: unit -> int64) =
    do
        if length < 1 then
            raise (new ArgumentOutOfRangeException("length", "Must be greater than 0"))

    new(?length: int) =
        TimestampBytesGenerator(defaultArg length 8, fun () -> Stopwatch.GetTimestamp())

    member this.Length = length

    member this.Generate span = SpanUtils.writeInt64Bytes (getTimestamp ()) span

    interface IIdentityBytesGenerator with
        member this.Length = this.Length

        member this.Generate span = this.Generate span


//type RandomBytesGenerator(length) =
//    do
//        if length < 1 then
//            raise (new ArgumentOutOfRangeException("length", "Must be greater than 0"))

//    member this.Length = length

//    member inline this.Generate span = SpanUtils.Random.writeBytes span

//    interface IIdentityBytesGenerator with
//        member this.Length = this.Length

//        member this.Generate span = this.Generate span

