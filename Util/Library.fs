namespace AdventOfCode

module Util =
    open System.IO

    let readLines (filePath: string) : seq<string> =
        seq {
            use sr = new StreamReader(filePath)

            while not sr.EndOfStream do
                yield sr.ReadLine()
        }

    let explodeString (s: string) : char list = [ for c in s -> c ]


    let implodeString (xs: char list) : string =
        let sb = System.Text.StringBuilder xs.Length
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

    let rec getMatchingStrings (nstr: char list) (strings: string list) (matchList: char list) (str: char list) =
        match str with
        | [] ->
            if List.isEmpty nstr then
                strings
            else
                List.append strings (List.singleton (nstr |> implodeString))
        | head :: tail ->
            if List.contains head matchList then
                let nnstr = List.append nstr (List.singleton head)
                getMatchingStrings nnstr strings matchList tail
            else if not (List.isEmpty nstr) then
                let nstrings = List.append strings (List.singleton (nstr |> implodeString))
                getMatchingStrings List.empty nstrings matchList tail
            else
                getMatchingStrings nstr strings matchList tail

    let getStrings (matchList: char list) (str: string) =
        getMatchingStrings List.empty List.empty matchList (explodeString str)
