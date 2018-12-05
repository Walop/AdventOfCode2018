open System
open AdventOfCode

let rec part1 result input =
    match input with
    | [] -> result
    | head::tail ->
        if result = [] then
            part1 (List.singleton head) tail
        else
            let last = List.last result
            let lastLower = Char.ToLower last
            let headLower = Char.ToLower head
            if lastLower = headLower && not (last = head) then
                part1 (List.take ((List.length result) - 1) result) tail
            else
                part1 (List.append result (List.singleton head)) tail

let part2 input =
    ['a' .. 'z']
    |> List.map (fun c -> List.filter (fun ch -> Char.ToLower ch <> c) input)
    |> List.map (fun l -> part1 List.empty l)
    |> List.minBy (fun l -> List.length l)

[<EntryPoint>]
let main argv =
    let explodeFirst = Seq.head >> Util.explodeString

    let input =
        Util.readLines( __SOURCE_DIRECTORY__ + "\\input")
        |> explodeFirst

    let part1 =
        input 
        |> part1 List.empty

    let printLength = List.length >> printfn "%d"

    part1
    |> printLength

    part1
    |> part2
    |> printLength

    0 // return an integer exit code
