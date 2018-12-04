open System
open AdventOfCode

type Entry = struct
    val year: int
    val month: int
    val day: int
    val hour: int
    val minute: int
    val id: int
    val action: int

    new (y, mm, d, h, min, i, a) =
        { year = y; month = mm; day = d; hour = h; minute = min; id = i; action = a}
end

let rec getNumbers (nstr: char list, numbers: int list, str: char list): int list =
    match str with
    | [] -> List.append numbers (List.singleton(0))
    | head::tail ->
        if "1234567890".Contains head then
            let nnstr = List.append nstr (List.singleton head)
            getNumbers(nnstr, numbers, tail)
        else if not (List.isEmpty nstr) then
            let nnumbers = List.append numbers (List.singleton(nstr |> Util.implodeString |> int))
            getNumbers(List.empty, nnumbers, tail)
        else if head = 'l' then
            List.append numbers [0; 1]
        else if head = 'w' then
            List.append numbers [0; 2]
        else
            getNumbers(nstr, numbers, tail)

let rec getSleep (sleep: (int * int) list, previd: int, entries: Entry list): (int * int) list =
    match entries with
    | [] -> sleep
    | head::tail ->
        if head.action = 0 then
            getSleep(sleep, head.id, tail)
        else if head.action = 1 then
            let nextHead = List.head tail
            let nextTail = List.tail tail
            printfn "ID %d slept %d - %d" previd head.minute nextHead.minute
            let newSleep = [for m in head.minute .. nextHead.minute do yield (previd, m)]
            getSleep(List.append sleep newSleep, previd, nextTail)
        else
            getSleep(sleep, previd, tail)

let findMaxMins (mins: (int * int) list): int =
    mins
    |> List.map snd
    |> List.groupBy (fun m -> m)
    |> List.maxBy (fun m -> List.length (snd m))
    |> fst

let part1 (sleep: (int * int) list) =
    sleep
    |> List.groupBy (fun s -> fst s)
    |> List.maxBy (fun s -> List.length (snd s))
    |> (fun (id, mins) -> (id, findMaxMins mins))
    |> (fun (id, mins) -> printfn "ID %d slept most at %d minutes IDxminute = %d" id mins (id*mins))

let part2 (sleep: (int * int) list) =
    sleep
    |> List.groupBy (fun s -> s)
    |> List.maxBy (fun s -> snd s |> List.length)
    |> fun s -> printfn "%A %d" s (fst (fst s) * snd (fst s))

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines( __SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map (fun str -> getNumbers(List.empty, List.empty, str |> Util.explodeString))
        |> Seq.map (fun nums -> Entry(nums.[0], nums.[1], nums.[2], nums.[3], nums.[4], nums.[5], nums.[6]))
        |> Seq.sortBy (fun e -> (e.year, e.month, e.day, e.hour, e.minute))
        |> (fun entries -> getSleep(List.empty, 0, List.ofSeq entries))
     
    input |> part1
    input |> part2
        
    0 // return an integer exit code
