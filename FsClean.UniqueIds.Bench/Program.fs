open System
open System.CommandLine
open System.Diagnostics
open System.Linq
open System.Reflection
open System.Threading
open System.Threading.Tasks

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open FsClean.UniqueIds.Bench

let isBenchmarkMethod (method: MethodInfo) =
    not (isNull (method.GetCustomAttribute<BenchmarkAttribute>(false)))

let isBenchmarkType (t: Type) =
    t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public)
    |> Seq.exists isBenchmarkMethod

let getAllBenchmarkTypes (assembly: Assembly) =
    assembly.ExportedTypes
    |> Seq.filter isBenchmarkType
    |> Seq.sortBy (fun t -> t.Name)

let getAllOperations (benchType: Type) =
    benchType.GetMethods()
    |> Seq.filter isBenchmarkMethod
    |> Seq.sortBy (fun m -> m.Name)

let runProfilerOperation (benchType: Type) (operation: MethodInfo) size count seed =
    printfn "Profiling operation: %s.%s" benchType.Name operation.Name

    let instance = Activator.CreateInstance(benchType)

    let setOptProp propName value =
        let property = benchType.GetProperty(propName)
        if value >= 0 && not (isNull property) && property.CanWrite then
            property.SetValue(instance, value)
            |> ignore

    setOptProp "Size" size
    setOptProp "Count" count
    setOptProp "Seed" seed

    let setupMethod = benchType.GetMethod("Setup")

    if not (isNull setupMethod) then
        setupMethod.Invoke(instance, null) |> ignore

    let watch = Stopwatch.StartNew()
    operation.Invoke(instance, null) |> ignore
    watch.Stop()
    printfn "Elapsed: %O" watch.Elapsed

let runAllProfilerOperations benchType size count seed =
    let operations = getAllOperations benchType

    for operation in operations do
        runProfilerOperation benchType operation size count seed

let benchmarks =
    (getAllBenchmarkTypes (Assembly.GetExecutingAssembly()))
        .ToDictionary((fun t -> t.Name), id, StringComparer.InvariantCultureIgnoreCase)

let collectBenchmarkTypes names =
    names
    |> Seq.map (fun name ->
        let found, t = benchmarks.TryGetValue(name)
        name, found, t)
    |> Seq.fold
        (fun acc (name, found, type') ->
            match acc, found with
            | Ok types, true -> Ok(type' :: types)
            | Ok _, false -> Error [ name ]
            | Error names, false -> Error(name :: names)
            | Error names, true -> Error names)
        (Ok [])
    |> Result.map (List.rev >> List.distinct >> List.toArray)
    |> Result.mapError (List.rev >> List.distinct >> List.toArray)

let collectBenchmarkOperations (benchType: Type) operations =
    operations
    |> Seq.map (fun operation ->
        let method = benchType.GetMethod(operation)
        operation, not(isNull method), method)
    |> Seq.fold
        (fun acc (name, found, method) ->
            match acc, found with
            | Ok methods, true -> Ok(method :: methods)
            | Ok _, false -> Error [ name ]
            | Error names, false -> Error(name :: names)
            | Error names, true -> Error names)
        (Ok [])
    |> Result.map (List.rev >> List.distinct >> List.toArray)
    |> Result.mapError (List.rev >> List.distinct >> List.toArray)

let benchCommand =
    let command = Command("bench", "Run a benchmark set")

    let nameOption =
        Option<string []>("--name", "The name of the benchmark to run. Can be set multiple times")

    nameOption.IsRequired <- true
    nameOption.AddAlias "-n"
    command.AddOption(nameOption)

    command.SetHandler(
        Action<_> (fun names ->
            match names with
            | [| "list" |] ->
                benchmarks.Keys
                |> Seq.sort
                |> String.concat ", "
                |> printfn "Available benchmarks: all or %s"

            | [| "all" |] ->
                BenchmarkRunner.Run(Assembly.GetExecutingAssembly())
                |> ignore

            | _ ->
                match collectBenchmarkTypes names with
                | Ok [||] -> printfn "No benchmarks to run"
                | Ok types -> BenchmarkRunner.Run(types = types) |> ignore
                | Error [| name |] -> printfn "Unknown benchmark (use list): %s" name
                | Error [||] -> invalidOp "Unreachable code detected"
                | Error names ->
                    names
                    |> Seq.sort
                    |> String.concat ", "
                    |> printfn "Unknown benchmarks (use list): %s"
        ),
        nameOption
    )

    command

let profileCommand =
    let command =
        Command("profile", "Run a profiling operation")

    let nameOption =
        Option<string []>("--name", "The name of the benchmark to run. Can be set multiple times")

    nameOption.IsRequired <- true
    nameOption.AddAlias "-n"
    command.AddOption(nameOption)

    let operationOption =
        Option<string[]>("--operation", "The operation of the profile to run. Can be set multiple times")

    operationOption.AddAlias "-o"
    command.AddOption(operationOption)

    let sizeOption =
        Option<int>("--size", (fun () -> -1), "The size of the profile to run")

    sizeOption.AddAlias "-s"
    command.AddOption(sizeOption)

    let countOption =
        Option<int>("--count", (fun () -> -1), "The count of the profile to run")

    countOption.AddAlias "-c"
    command.AddOption(countOption)

    let seedOption =
        Option<int>("--seed", (fun () -> -1), "The seed of the profile to run")

    seedOption.AddAlias "-r"
    command.AddOption(seedOption)

    command.SetHandler(
        (fun names operations size count seed ->
            match names with
            | [| "list" |] ->
                benchmarks.Keys
                |> Seq.sort
                |> String.concat ", "
                |> printfn "Available profilers: all or %s"

            | [| "all" |] ->
                getAllBenchmarkTypes (Assembly.GetExecutingAssembly())
                |> Seq.iter (fun profiler -> runAllProfilerOperations profiler size count seed)

            | names ->
                match collectBenchmarkTypes names with
                | Ok [||] -> printfn "No profilers to run"
                | Ok [| profiler |] ->
                    match operations with
                    | [| "list" |] ->
                        let operations = getAllOperations profiler
                        let names = operations |> Seq.map (fun m -> m.Name) |> String.concat ", "
                        printfn "Available operations: all or %s" names

                    | [| "all" |] ->
                        runAllProfilerOperations profiler size count seed

                    | operations ->
                        match collectBenchmarkOperations profiler operations with
                        | Ok [||] -> printfn "No operations to run"
                        | Ok operations ->
                            for operation in operations do
                                runProfilerOperation profiler operation size count seed
                        | Error [| name |] -> printfn "Unknown profiler operation (use list): %s" name
                        | Error [||] -> invalidOp "Unreachable code detected"
                        | Error names ->
                            names
                            |> Seq.sort
                            |> String.concat ", "
                            |> printfn "Unknown profiler operations (use list): %s"
                        
                | Ok profilers ->
                    match operations with
                    | [||] ->
                        for profiler in profilers do
                            runAllProfilerOperations profiler size count seed

                    | _ ->
                        printfn "Cannot specify operations for multiple profilers"
                | Error [| name |] -> printfn "Unknown profiler (use list): %s" name
                | Error [||] -> invalidOp "Unreachable code detected"
                | Error names ->
                    names
                    |> Seq.sort
                    |> String.concat ", "
                    |> printfn "Unknown profilers (use list): %s"
        ),
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
