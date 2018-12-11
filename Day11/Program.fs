open System

let calculatePowerLevel serial coordinates =
    let rackId = (fst coordinates) + 10
    
    rackId 
    |> (*) (snd coordinates)
    |> (+) serial
    |> (*) rackId
    |> string
    |> Seq.rev
    |> Seq.tryItem 2
    |> Option.defaultValue '0'
    |> string
    |> int
    |> (+) -5

let calculatePowerSum (grid: int []) size coordinates =
    let cx = fst coordinates
    let cy = snd coordinates
    let powersum =
        seq {
            for y in cy..cy+(size-1) do
                for x in cx..cx+(size-1) do
                    yield (x, y)
        }
        |> Seq.map (fun c -> (fst c - 1) + (300 * (snd c - 1)))
        |> Seq.map (fun i -> grid.[i])
        |> Seq.sum
    (coordinates, powersum)

let part1 powerlevels =
    seq {
        for y in 1..298 do
            for x in 1..298 do
                yield (x, y)
    }
    |> Seq.map (calculatePowerSum powerlevels 3)
    |> Seq.maxBy (fun ps -> snd ps)

let rec part2 size prevlevel powerlevels  =
    printfn "Calculating size %i" size
    let highestLevel =
        seq {
            for y in 1..300 - (size - 1) do
                for x in 1..300 - (size - 1) do
                    yield (x, y)
        }
        |> Seq.map (calculatePowerSum powerlevels size)
        |> Seq.maxBy (fun ps -> snd ps)
    printfn "Best level at %i,%i: %i" (fst (fst highestLevel)) (snd (fst highestLevel)) (snd highestLevel)
    if (snd highestLevel) >= (snd prevlevel) then
        part2 (size + 1) highestLevel powerlevels
    else
        (size - 1, fst prevlevel, snd prevlevel)

[<EntryPoint>]
let main argv =
    let grid = [|
        for y in 1..300 do
            for x in 1..300 do
                yield (x, y)  
    |]

    let calculate = calculatePowerLevel 9435

    let powerlevels =
        grid
        |> Array.map calculate

    powerlevels
    |> part1
    |> fun (coord, level) -> printfn "Max level at %i,%i with size %i: %i" (fst coord) (snd coord) 3 level

    powerlevels
    |> (part2 1 ((0, 0), 0))
    |> fun (size, coord, level) -> printfn "Max level at %i,%i with size %i: %i" (fst coord) (snd coord) size level
    
    0 // return an integer exit code
