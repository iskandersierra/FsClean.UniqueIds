namespace FsClean.UniqueIds

open System

open Microsoft.FSharp.NativeInterop
open System.Security.Cryptography
open System.Text

#nowarn "9"

module internal IdUtils =
    let inline stackAlloc<'t> (size: int) =
        let nativeBytes = NativePtr.stackalloc<byte> size
        let nativePtr = NativePtr.toVoidPtr nativeBytes
        let bytesSpan = Span<byte>(nativePtr, size)
        bytesSpan

    let validateSize minSize maxSize (size: int) =
        if size < minSize then
            invalidArg "size" $"size must not be less than {minSize}"
        elif size > maxSize then
            invalidArg "size" "size must not be greater than {maxSize}"

    let validateEvenSize (size: int) =
        if size % 2 <> 0 then
            invalidArg "size" "size must be even"

    let createHexLetters upperCase =
            let zeroLetter = int '0'

            let firstLetter =
                if upperCase then
                    int 'A'
                else
                    int 'a'

            [| for ch = 0 to 9 do
                yield byte (ch + zeroLetter)
               for ch = 0 to 5 do
                   yield byte (ch + firstLetter) |]


    let lowerCaseHexLetters = createHexLetters false

    let upperCaseHexLetters = createHexLetters true

    let encodeBytesToHex upperCase (bytesSpan: Span<byte>) (span: Span<byte>) =
        let mutable j = 0
        let letters = if upperCase then upperCaseHexLetters else lowerCaseHexLetters
        let last = bytesSpan.Length - 1

        for i = 0 to last do
            let b = bytesSpan.[i]
            let ch1 = letters.[int (b >>> 4)]
            let ch2 = letters.[int (b &&& 0x0Fuy)]
            span.[j] <- ch1
            span.[j + 1] <- ch2
            j <- j + 2

    let inline generateRandomSpan (span: Span<byte>) =
        RandomNumberGenerator.Fill(span)

    let generateRandomString (size: int) =
        let span = stackAlloc<byte> (size >>> 1)
        let charSpan = Array.zeroCreate<byte> size
        generateRandomSpan span
        encodeBytesToHex false span (charSpan.AsSpan())
        Encoding.ASCII.GetString(charSpan)

