open System
open AdventOfCode

[<EntryPoint>]
let main argv =
    Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
    |> Seq.map (fun line -> line |> int)
    |> Seq.sum
    |> Console.WriteLine

    0
