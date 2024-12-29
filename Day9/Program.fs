open System
open AdventOfCode
open System.IO

type Node(v: int64, p: Node option, n: Node option) =
    let mutable prev = p
    let mutable next = n

    member this.Value = v

    member this.Prev
        with get () = prev
        and set (p) = prev <- p

    member this.Next
        with get () = next
        and set (n) = next <- n

let rec getNumbers (nstr: char list) (numbers: int list) (str: char list) =
    match str with
    | [] -> numbers
    | head :: tail ->
        if "1234567890".Contains head then
            let nnstr = List.append nstr (List.singleton head)
            getNumbers nnstr numbers tail
        else if not (List.isEmpty nstr) then
            let nnumbers =
                List.append numbers (List.singleton (nstr |> Util.implodeString |> int))

            getNumbers List.empty nnumbers tail
        else
            getNumbers nstr numbers tail

let insertNode newValue (after: byref<Node>) =
    let node = Node(newValue, Some after, after.Next)
    after.Next <- Some node
    node.Next.Value.Prev <- Some node
    node

let removeNode (n: Node) =
    let mutable prev = n.Prev.Value
    let mutable next = n.Next.Value

    prev.Next <- Some next
    next.Prev <- Some prev

    n.Value

let rec traverse i (n: Node) =
    if i < 0 then traverse (i + 1) n.Prev.Value
    else if i > 0 then traverse (i - 1) n.Next.Value
    else n

let rec runGame
    (stopWatch: System.Diagnostics.Stopwatch)
    players
    currentPlayer
    currentMarble
    (points: int64 array)
    (remainingMarbles: int64 list)
    =
    let ncurrentPlayer =
        if (currentPlayer + 1) > players then
            1
        else
            (currentPlayer + 1)

    match remainingMarbles with
    | [] -> points
    | head :: tail ->
        if (head % Convert.ToInt64 23) = Convert.ToInt64 0 then
            let toRemove = traverse -7 currentMarble
            let ncurrentMarble = toRemove.Next.Value
            let mpoints = Convert.ToInt64(removeNode toRemove)
            let ppoints = points.[currentPlayer]
            points.[currentPlayer] <- (ppoints + mpoints + head)
            runGame stopWatch players ncurrentPlayer ncurrentMarble points tail

        else
            let mutable insertAfter = (traverse 1 currentMarble)
            let ncurrentMarble = insertNode head &insertAfter
            runGame stopWatch players ncurrentPlayer ncurrentMarble points tail

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map Util.explodeString
        |> Seq.map (getNumbers List.empty List.empty)
        |> Seq.head

    let marbles = [ (int64) 1 .. (int64) (input.[1]) ]
    let points = Array.zeroCreate (input.[0] + 1)
    let mutable startMarble = Node(Convert.ToInt64 0, None, None)

    startMarble.Prev <- Some startMarble
    startMarble.Next <- Some startMarble


    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let best = runGame stopWatch input.[0] 1 startMarble points marbles |> Array.max

    best |> printfn "%i"

    use sw = new StreamWriter("output")
    sw.WriteLine(best |> string)

    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalSeconds

    0 // return an integer exit code
