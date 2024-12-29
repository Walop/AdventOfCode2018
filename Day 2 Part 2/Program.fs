open System
open AdventOfCode

let rec doStuff (i: int, rows: seq<string>) : string =
    let stringCounts =
        rows
        |> Seq.map (fun s -> s.Remove(i, 1))
        |> Seq.groupBy (fun s -> s)
        |> Map.ofSeq
        |> Map.map (fun k v -> Seq.length v)
        |> Map.filter (fun k v -> v = 2)

    if not (Map.isEmpty stringCounts) then
        stringCounts |> Map.toSeq |> Seq.map fst |> Seq.head
    else
        doStuff (i + 1, rows)


[<EntryPoint>]
let main argv =
    Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
    |> fun lines -> doStuff (0, lines)
    |> printfn "%s"

    0 // return an integer exit code
