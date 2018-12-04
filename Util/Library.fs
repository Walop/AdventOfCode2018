namespace AdventOfCode

module Util =
    open System.IO

    let readLines (filePath: string): seq<string> = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

    let explodeString (s:string): char list =
        [for c in s -> c]


    let implodeString(xs: char list): string =
        let sb = System.Text.StringBuilder xs.Length
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()
