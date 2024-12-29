open System
open AdventOfCode

let rec cycle s =
    seq {
        yield! s
        yield! cycle s
    }

[<EntryPoint>]
let main argv =
    Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
    |> Seq.map (fun s -> s |> int)
    |> cycle
    |> Seq.scan (fun (set, sum) number -> (Set.add sum set, sum + number)) (Set.empty, 0)
    |> Seq.find (fun (set, sum) -> Set.contains sum set)
    |> snd
    |> printfn "%d"

    0 // return an integer exit code
