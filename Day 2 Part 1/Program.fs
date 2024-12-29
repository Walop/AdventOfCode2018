open System
open System.IO
open AdventOfCode

#if DEBUG
[<AutoOpen>]
module AutoOpenModule =


    let (|>) value func =
        let result = func value
        result
#endif

let countLetters (line: string) : Map<char, int> =
    line
    |> Seq.groupBy (fun c -> c)
    |> Map.ofSeq
    |> Map.map (fun k v -> Seq.length v)

let hasNOccurences (occurences: int, counts: Map<char, int>) : bool =
    counts |> Map.filter (fun c count -> count = occurences) |> Map.isEmpty |> not

[<EntryPoint>]
let main argv =
    let charCounts =
        Util.readLines (__SOURCE_DIRECTORY__ + "\\input") |> Seq.map countLetters

    let twos = charCounts |> Seq.filter (fun m -> hasNOccurences (2, m)) |> Seq.length

    let threes = charCounts |> Seq.filter (fun m -> hasNOccurences (3, m)) |> Seq.length

    Console.WriteLine(twos * threes)


    0 // return an integer exit code
