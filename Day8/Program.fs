open System
open AdventOfCode

type Node =
    struct
        val childcount: int
        val metadatacount: int
        val value: int
        val childvalues: int list

        new(cc, m, v, c) =
            { childcount = cc
              metadatacount = m
              value = v
              childvalues = c }
    end

let getChildvalue i l =
    let idx = i - 1
    List.tryItem idx l |> Option.defaultValue 0

let rec getMetadatas donenodes (openNodes: Node list) mode input =
    if List.isEmpty input then
        donenodes
    else
        let lastOpen = List.last openNodes

        if List.length lastOpen.childvalues = lastOpen.childcount then
            let nodemetadatas = List.take lastOpen.metadatacount input
            printfn "%A" nodemetadatas

            let value =
                if mode = 1 || lastOpen.childcount = 0 then
                    List.sum nodemetadatas
                else
                    List.sum (List.map (fun m -> getChildvalue m lastOpen.childvalues) nodemetadatas)

            printfn
                "Metadata %i, Childcount %i, Value %i, Children: %A"
                lastOpen.metadatacount
                lastOpen.childcount
                value
                lastOpen.childvalues

            let ndonenodes =
                List.append
                    donenodes
                    (List.singleton (Node(lastOpen.metadatacount, lastOpen.childcount, value, lastOpen.childvalues)))

            let newLast =
                if (List.length openNodes) > 1 then
                    List.item (List.length openNodes - 2) openNodes
                    |> fun n ->
                        Node(n.childcount, n.metadatacount, value, List.append n.childvalues (List.singleton value))
                    |> List.singleton
                else
                    List.empty

            let nopenNodes =
                if (List.length openNodes) > 1 then
                    List.append (List.take ((List.length openNodes) - 2) openNodes) newLast
                else
                    openNodes

            getMetadatas ndonenodes nopenNodes mode (List.skip lastOpen.metadatacount input)
        else
            let nextNode = List.take 2 input |> fun n -> Node(n.[0], n.[1], 0, List.empty)
            getMetadatas donenodes (List.append openNodes (List.singleton (nextNode))) mode (List.skip 2 input)

let part1 input =
    let firstNode =
        input
        |> List.take 2
        |> fun n -> Node(n.[0], n.[1], 0, List.empty)
        |> List.singleton

    input
    |> List.skip 2
    |> (getMetadatas List.empty firstNode 1)
    |> List.map (fun n -> n.value)
    //|> printfn "%A"
    |> List.sum
    |> printfn "%i"

let part2 input =
    let firstNode =
        input
        |> List.take 2
        |> fun n -> Node(n.[0], n.[1], 0, List.empty)
        |> List.singleton

    input
    |> List.skip 2
    |> (getMetadatas List.empty firstNode 2)
    |> List.last
    |> fun n -> n.value
    //|> printfn "%A"
    |> printfn "%i"

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
        |> Seq.head
        |> (fun s -> s.Split [| ' ' |])
        |> Seq.map int
        |> List.ofSeq

    input |> part2



    0 // return an integer exit code
