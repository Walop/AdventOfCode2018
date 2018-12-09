open System
open AdventOfCode

let rec getNumbers (nstr: char list) (numbers: int list) (str: char list) =
    match str with
    | [] -> numbers
    | head::tail ->
        if "1234567890".Contains head then
            let nnstr = List.append nstr (List.singleton head)
            getNumbers nnstr  numbers  tail
        else if not (List.isEmpty nstr) then
            let nnumbers = List.append numbers (List.singleton(nstr |> Util.implodeString |> int))
            getNumbers List.empty nnumbers tail
        else
            getNumbers nstr numbers tail

let insertAt e i l =
    let first, second = List.splitAt i l
    List.append (List.append first (List.singleton e)) second

let removeAt i l =
    let first, second = List.splitAt i l
    (List.head second, List.concat [first; List.tail second])

let rec runGame players currentPlayer currentMarble points playedMarbles remainingMarbles =
    if List.length playedMarbles % 1000 = 0 then
        printfn "%i played" (List.length playedMarbles)
    let ncurrentPlayer =
        if (currentPlayer + 1) > players then
            1
        else
            (currentPlayer + 1)
    match remainingMarbles with
    | [] -> points
    | head::tail ->
        if (head % 23) = 0 then
            let ncurrentMarble = (currentMarble + (List.length playedMarbles) - 7) % (List.length playedMarbles)
            let mpoints, nplayedMarbles = removeAt ncurrentMarble playedMarbles
            let ppoints, plist = removeAt currentPlayer points
            let npoints = insertAt (ppoints + mpoints + head) currentPlayer plist
            
            runGame players ncurrentPlayer ncurrentMarble npoints nplayedMarbles tail

        else
            let ncurrentMarble = (currentMarble + 1) % (List.length playedMarbles) + 1
            let nplayedMarbles = insertAt head ncurrentMarble playedMarbles
            runGame players ncurrentPlayer ncurrentMarble points nplayedMarbles tail

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines( __SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map Util.explodeString
        |> Seq.map (getNumbers List.empty List.empty)
        |> Seq.head

    let marbles = [1 .. input.[1]]
    let points = Array.zeroCreate (input.[0] + 1) |> List.ofArray

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    runGame input.[0] 1 0 points [0] marbles
    |> List.max
    |> printfn "%i"

    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds

    0 // return an integer exit code
