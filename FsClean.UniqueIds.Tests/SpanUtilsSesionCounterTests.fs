module SpanUtilsSesionCounterTests

open System

open Xunit

open FsClean.UniqueIds

open System.Threading.Tasks
open System.Threading

let locker = new SemaphoreSlim(1)

[<Theory>]
[<InlineData(10)>]
[<InlineData(100)>]
[<InlineData(1000)>]
let ``getValue`` (count: int) =
    task {
        do! locker.WaitAsync()

        try
            SpanUtils.SessionCounter.resetValue ()

            let! result =
                Task.WhenAll(
                    seq { 1 .. count }
                    |> Seq.map (fun _ -> Task.Run(fun () -> SpanUtils.SessionCounter.getValue ()))
                )

            let resultSet = Set.ofArray result

            Assert.Equal(count, resultSet.Count)
            Assert.All(resultSet, (fun x -> Assert.True(x >= 0L && x < int64 count)))

        finally
            locker.Release() |> ignore
    }

[<Theory>]
[<InlineData(0, 1, "00")>]
[<InlineData(10, 2, "000A")>]
[<InlineData(100, 3, "000064")>]
[<InlineData(1000, 5, "00000003E8")>]
let ``writeBytes`` (skipCount: int) (size: int) (expected: string) =
    task {
        do! locker.WaitAsync()

        try
            SpanUtils.SessionCounter.resetValue ()
            for _ = 1 to skipCount do
                SpanUtils.SessionCounter.getValue () |> ignore

            let span = SpanUtils.stackAlloc<byte> size
            SpanUtils.SessionCounter.writeBytes span
            let actual = Convert.ToHexString(span)
            Assert.Equal<char>(expected, actual)

        finally
            locker.Release() |> ignore
    }
