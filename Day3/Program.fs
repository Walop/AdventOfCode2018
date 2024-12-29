open System
open AdventOfCode

let rec getNumbers (nstr: char list, numbers: int list, str: char list) =
    match str with
    | [] -> List.append numbers (List.singleton (nstr |> Util.implodeString |> int))
    | head :: tail ->
        if "1234567890".Contains head then
            let nnstr = List.append nstr (List.singleton head)
            getNumbers (nnstr, numbers, tail)
        else if not (List.isEmpty nstr) then
            let nnumbers =
                List.append numbers (List.singleton (nstr |> Util.implodeString |> int))

            getNumbers (List.empty, nnumbers, tail)
        else
            getNumbers (nstr, numbers, tail)

let pointSeq rects =
    seq {
        for rect in rects do
            for x in 0 .. List.item 3 rect - 1 do
                for y in 0 .. List.item 4 rect - 1 do
                    yield (List.head rect, List.item 1 rect + x, List.item 2 rect + y)
    }

let part1 input =
    input
    |> pointSeq
    |> Seq.groupBy (fun (id, x, y) -> (x, y))
    |> Seq.filter (fun g -> Seq.length (snd g) > 1)
    |> Seq.length
    |> printfn "%d"

let part2 input =
    let ids = input |> Seq.map (fun x -> List.head x) |> Set.ofSeq

    input
    |> pointSeq
    |> Seq.groupBy (fun (id, x, y) -> (x, y))
    |> Seq.filter (fun g -> Seq.length (snd g) > 1)
    |> Seq.map (fun g -> snd g)
    |> Seq.concat
    |> Seq.map (fun (id, x, y) -> id)
    |> Set.ofSeq
    |> Set.difference ids
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map (fun str -> getNumbers (List.empty, List.empty, str |> Util.explodeString))

    input |> part1

    input |> part2

    0 // return an integer exit code
