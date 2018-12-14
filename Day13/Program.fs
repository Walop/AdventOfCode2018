open System
open AdventOfCode

type Point =
    struct
        val X: int
        val Y: int

        new (x, y) =
            { X = x; Y = y }
    end

type Cart =
    struct
        val Pos: Point
        val Dir: char
        val Turn: char list

        new (x, y, dir, turn) =
            { Pos = Point (x, y); Dir = dir; Turn = turn}
    end

let replaceCarts (c: char) =
    if List.contains c ['^'; 'v'] then
        '|'
    else if List.contains c ['<'; '>'] then
        '-'
    else
        c

let rec getCarts (carts: Cart seq) x row =
    if Seq.isEmpty row then
        carts
    else
        let turnlist = ['l'; 'f'; 'r']
        let head = Seq.head row
        let tail = Seq.tail row
        if List.contains (snd head) ['<'; '^'; '>'; 'v'] then
            let carts' = Seq.append carts (Seq.singleton (Cart (x, fst head, snd head, turnlist)))
            getCarts carts' x tail
        else
            getCarts carts x tail

let dirMap =
    Map ['<', (-1, 0); '^', (0, -1); '>',(1,0); 'v', (0, 1)]

let runCart map cart =
    let moveTo = Map.item cart.Dir
    let newLocation = Point (cart.Pos.X + (fst moveTo), cart.Pos.Y + (snd moveTo))

[<EntryPoint>]
let main argv =
    let inputlines =
        Util.readLines ( __SOURCE_DIRECTORY__ + "\\input")

    let inputwidth =
        inputlines
        |> Seq.head
        |> Seq.length

    let trackmap =
        inputlines
        |> Seq.map Util.explodeString
        |> Seq.concat
        |> Seq.map replaceCarts
        |> Array.ofSeq

    let carts =
        inputlines
        |> Seq.map (Util.explodeString >> Seq.indexed)
        |> Seq.indexed
        |> Seq.map (fun (x, row) -> getCarts Seq.empty x row)
        |> Seq.concat
        |> List.ofSeq


    //trackmap
    //|> List.ofArray
    //|> List.chunkBySize inputwidth
    //|> List.iter (fun line -> printfn "%s" (Util.implodeString line))

    0 // return an integer exit code
