open System

let getDigits nbr =
    let tens = nbr / 10
    let ones = nbr % 10

    if tens > 0 then
        [|tens; ones|]
    else
        [|ones |]

let rec part1 stoplen idxs (recipes: int array) =
    //printfn "%A" recipes
    let first = recipes.[fst idxs]
    let second = recipes.[snd idxs]

    let createdRecipes = getDigits (first + second)
    let newRecipes = Array.append recipes createdRecipes
    let newLen = Array.length newRecipes
    if newLen >= stoplen then
        newRecipes
    else
        if newLen % 10000 = 0 then
                printfn "%i recipes created" newLen
        let fIdx = ((fst idxs) + first + 1) % newLen
        let sIdx = ((snd idxs) + second + 1) % newLen
        part1 stoplen (fIdx, sIdx) newRecipes

let rec part2 stopseq idxs (recipes: int array) =
    //printfn "%A" recipes
    let first = recipes.[fst idxs]
    let second = recipes.[snd idxs]

    let createdRecipes = getDigits (first + second)
    let newRecipes = Array.append recipes createdRecipes
    let newLen = Array.length newRecipes
    let stopLen = Array.length stopseq
    let fIdx = ((fst idxs) + first + 1) % newLen
    let sIdx = ((snd idxs) + second + 1) % newLen
    if stopLen > newLen then
        part2 stopseq (fIdx, sIdx) newRecipes
    else
        let tail1 =
            newRecipes
            |> Array.skip (newLen - stopLen - 1)
            |> Array.take stopLen
        let tail2 =
            newRecipes
            |> Array.skip (newLen - stopLen)
        //printfn "%A" stopseq
        //printfn "%A" tail1
        //printfn "%A" tail2
        if tail1 = stopseq then
            //printfn "Found 1"
            newLen - stopLen - 1
        else if tail2 = stopseq then
            //printfn "Found 2"
            //printfn "%A" newRecipes
            //printfn "%A" createdRecipes
            //printfn "%A" stopseq
            newLen - stopLen
        else
            if newLen % 10000 = 0 then
                printfn "%i recipes created" newLen
            part2 stopseq (fIdx, sIdx) newRecipes
        
let inline charToInt c = int c - int '0'

[<EntryPoint>]
let main argv =
    let initialRecipes = [|3; 7;|]
    let initialIdxs = (0,1)
    let after = 320851
    let stopSeq =
        after
        |> string
        |> Seq.map (fun c -> c)
        |> Seq.map charToInt
        |> Array.ofSeq
        

    part1 (after + 10) initialIdxs initialRecipes
    |> Array.skip after
    |> Array.take 10
    |> Array.map string
    |> String.concat ""
    |> printfn "%s"

    part2 stopSeq initialIdxs initialRecipes
    |> printfn "%i"

    0 // return an integer exit code
