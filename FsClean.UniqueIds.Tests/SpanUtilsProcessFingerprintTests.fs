module SpanUtilsProcessFingerprintTests

open System

open Xunit

open FsClean.UniqueIds

[<Fact>]
let ``createHostFingerprint`` () =
    let fingerprint = SpanUtils.ProcessFingerprint.createHostFingerprint()
    let actual = fingerprint.ToString("X8")

    Assert.Equal(8, actual.Length)
    Assert.NotEqual<string>("00000000", actual)

[<Fact>]
let ``createProcessFingerprint`` () =
    let fingerprint = SpanUtils.ProcessFingerprint.createProcessFingerprint()
    let actual = fingerprint.ToString("X8")

    Assert.Equal(8, actual.Length)
    Assert.NotEqual<string>("00000000", actual)

[<Fact>]
let ``createFingerprint`` () =
    let fingerprint = SpanUtils.ProcessFingerprint.createFingerprint()
    let actual = fingerprint.ToString("X8")

    Assert.Equal(8, actual.Length)
    Assert.NotEqual<string>("00000000", actual)

[<Fact>]
let ``getValue`` () =
    let fingerprint = SpanUtils.ProcessFingerprint.getValue()
    let actual = fingerprint.ToString("X8")

    Assert.Equal(8, actual.Length)
    Assert.NotEqual<string>("00000000", actual)
