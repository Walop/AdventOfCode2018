open System
open AdventOfCode

let rec getMetadatas metadatas openNodes input =
    if List.isEmpty input then
        metadatas
    else
        let lastOpen = List.last openNodes
        if (fst lastOpen) = 0 then
            let nmetadatas = List.append metadatas (List.take (snd lastOpen) input)
            let newLast =
                if (List.length openNodes) > 1 then
                    List.item (List.length openNodes - 2) openNodes |> fun n -> (fst n - 1, snd n) |> List.singleton
                else
                    List.empty

            let nopenNodes =
                if (List.length openNodes) > 1 then
                    List.append (List.take ((List.length openNodes) - 2) openNodes) newLast
                else
                    openNodes

            getMetadatas nmetadatas nopenNodes (List.skip (snd lastOpen) input)
        else
            let nextNode = List.take 2 input |> fun n -> (n.[0], n.[1])
            getMetadatas metadatas (List.append openNodes (List.singleton (nextNode))) (List.skip 2 input)

let part1 input =
    let firstNode =
        input
        |> List.take 2
        |> fun n -> (n.[0], n.[1])
        |> List.singleton

    input
    |> List.skip 2
    |> (getMetadatas List.empty firstNode)
    |> List.sum
    |> printfn "%i"

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines( __SOURCE_DIRECTORY__ + "\\input")
        |> Seq.head
        |> (fun s -> s.Split [|' '|])
        |> Seq.map int
        |> List.ofSeq

    input |> part1
    


    0 // return an integer exit code
