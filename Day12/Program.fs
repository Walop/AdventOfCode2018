open System
open AdventOfCode

let findRule rules batch =
    let id = fst (List.item 2 batch)
    let pots = List.map (fun item -> snd item) batch

    let rule =
        List.tryFind (fun rule -> (fst rule) = pots) rules
        |> Option.defaultValue (List.empty, '.')

    (id, snd rule)

let rec untilGen (gen: int64) (stopGen: int64) rules initial =
    if gen % 1000L = 0L then
        printfn "Gen %i: %s" gen (Util.implodeString (List.map (fun (i, p) -> p) initial))
        printfn "First index: %i" (fst (List.head initial))

    if gen = stopGen then
        (gen, initial)
    else
        let headIndex = fst (List.head initial)
        let lastIndex = fst (List.last initial)

        let padLeft =
            [ for i in headIndex - 3 .. headIndex - 1 do
                  yield (i, '.') ]

        let padRight =
            [ for i in lastIndex + 1 .. lastIndex + 3 do
                  yield (i, '.') ]

        let padded = List.concat [ padLeft; initial; padRight ]

        let ninitial =
            List.windowed 5 padded
            |> List.fold (fun acc batch -> List.append acc (List.singleton (findRule rules batch))) List.empty
            |> List.skipWhile (fun i -> snd i = '.')
            |> List.rev
            |> List.skipWhile (fun i -> snd i = '.')
            |> List.rev

        untilGen (gen + (int64) 1) stopGen rules ninitial

let part1 initial rules =
    untilGen 0L 20L rules initial
    |> fun (gen, result) -> result
    |> List.filter (fun (i, p) -> p = '#')
    |> List.map (fun pot -> fst pot)
    |> List.sum
    |> printfn "Sum of pots with plant %i"

let part2 initial rules =
    // Propably already looping
    let statusat10k = untilGen 0L 10000L rules initial

    let firstat10k =
        statusat10k |> (fun (gen, result) -> result) |> List.head |> fst |> int64

    let statusat20k = untilGen 10000L 20000L rules (snd statusat10k)

    let firstat20k =
        statusat20k |> (fun (gen, result) -> result) |> List.head |> fst |> int64

    let diff = firstat20k - firstat10k

    printfn "10k diff %i" diff

    let totaldiff = ((50000000000L - 20000L) / 10000L) * diff

    printfn "Total diff %i" totaldiff

    statusat20k
    |> fun (gen, result) -> result
    |> List.filter (fun (i, p) -> p = '#')
    |> List.map (fun pot -> Convert.ToInt64(fst pot) + totaldiff)
    |> List.sum
    |> printfn "Sum of pots with plant %i"

[<EntryPoint>]
let main argv =
    let input = Util.readLines (__SOURCE_DIRECTORY__ + "\\input")

    let initial =
        input
        |> Seq.head
        |> Util.getStrings [ '.'; '#' ]
        |> Seq.head
        |> Util.explodeString
        |> List.indexed

    let rules =
        input
        |> Seq.tail
        |> Seq.map (Util.getStrings [ '.'; '#' ])
        |> Seq.filter (fun r -> r <> [])
        |> Seq.map (fun r -> (Util.explodeString r.[0], r.[1] |> char))
        |> List.ofSeq

    printfn "Initial = %A" initial

    printfn "Rules ="
    rules |> List.iter (printfn "%A")

    part1 initial rules
    part2 initial rules

    0 // return an integer exit code
