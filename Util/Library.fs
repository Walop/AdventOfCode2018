namespace AdventOfCode

module Util =
    open System.IO

    let readLines (filePath: string): seq<string> = seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine()
}
