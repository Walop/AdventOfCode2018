open System
open System.IO

let readLines (filePath: string) = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

[<EntryPoint>]
let main argv =
    readLines( __SOURCE_DIRECTORY__ + "\\input")
    |> Seq.map(fun line -> line |> int)
    |> Seq.sum
    |> Console.WriteLine

    0
