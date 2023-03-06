open System
open System.CommandLine
open System.Linq
open System.Reflection
open System.Threading
open System.Threading.Tasks

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open FsClean.UniqueIds.Bench

let benchmarks =
    ([ "SpanUtilsASCII", typeof<SpanUtilsASCII> ])
        .ToDictionary(fst, snd, StringComparer.InvariantCultureIgnoreCase)

let benchCommand =

    let command = Command("bench", "Run a benchmark set")

    let nameOption =
        Option<string>("--name", "The name of the benchmark to run")

    nameOption.IsRequired <- true
    command.AddOption(nameOption)

    command.SetHandler(
        Action<_> (fun name ->
            match benchmarks.TryGetValue(name) with
            | true, benchType -> BenchmarkRunner.Run(benchType) |> ignore
            | false, _ ->
                match name with
                | "list" ->
                    let names = String.Join(", ", benchmarks.Keys)
                    printfn "Available benchmarks: %s" names
                | _ -> printfn "Unknown benchmark: %s" name),
        nameOption
    )

    command

let profileCommand =
    let command =
        Command("profile", "Run a profiling operation")

    let nameOption =
        Option<string>("--name", "The name of the profile to run")

    nameOption.IsRequired <- true
    command.AddOption(nameOption)

    let operationOption =
        Option<string>("--operation", "The operation of the profile to run")

    nameOption.IsRequired <- true
    command.AddOption(operationOption)

    let sizeOption =
        Option<int>("--size", (fun () -> -1), "The size of the profile to run")

    command.AddOption(sizeOption)

    let countOption =
        Option<int>("--count", (fun () -> -1), "The count of the profile to run")

    command.AddOption(countOption)

    let seedOption =
        Option<int>("--seed", (fun () -> -1), "The seed of the profile to run")

    command.AddOption(seedOption)

    command.SetHandler(
        (fun name operation size count seed ->
            match benchmarks.TryGetValue(name) with
            | true, benchType ->
                if isNull operation || operation = "list" then
                    let methods =
                        benchType.GetMethods()
                        |> Seq.filter (fun m ->
                            let attr =
                                m.GetCustomAttribute<BenchmarkAttribute>(false)

                            not (isNull attr))
                        |> Seq.map (fun m -> m.Name)

                    let names = methods |> String.concat ", "
                    printfn "Available operations: %s" names

                else
                    let method = benchType.GetMethod(operation)

                    if isNull method then
                        printfn "Unknown operation: %s" name
                    else
                        printfn "Profiling operation: %s.%s" name operation

                        let instance = Activator.CreateInstance(benchType)

                        let setOptProp propName value =
                            if value >= 0 then
                                benchType.InvokeMember(
                                    propName,
                                    BindingFlags.SetProperty
                                    ||| BindingFlags.Instance
                                    ||| BindingFlags.Public,
                                    Type.DefaultBinder,
                                    instance,
                                    [| value :> obj |]
                                )
                                |> ignore

                        setOptProp "Size" size
                        setOptProp "Count" count
                        setOptProp "Seed" seed

                        let setupMethod = benchType.GetMethod("Setup")

                        if not (isNull setupMethod) then
                            setupMethod.Invoke(instance, null) |> ignore

                        method.Invoke(instance, null) |> ignore

            | false, _ ->
                match name with
                | "list" ->
                    let names = String.Join(", ", benchmarks.Keys)
                    printfn "Available profilers: %s" names
                | _ -> printfn "Unknown profiler: %s" name),
        nameOption,
        operationOption,
        sizeOption,
        countOption,
        seedOption
    )

    command

let root = RootCommand("Unique IDs Benchmarks")
root.AddCommand(benchCommand)
root.AddCommand(profileCommand)

root.Invoke(Environment.GetCommandLineArgs().[1..])
|> Environment.Exit
