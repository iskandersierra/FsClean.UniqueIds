param (
    [Parameter(Mandatory = $true)] [string] $Name
)

dotnet run -c Release --project .\FsClean.UniqueIds.Bench\FsClean.UniqueIds.Bench.fsproj -- bench --name $Name
