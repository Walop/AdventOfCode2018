open System
open AdventOfCode
open System.IO

type Point =
    struct
        val X: int
        val Y: int
        val DX: int
        val DY: int

        new (x, y, dx, dy) =
            {X = x; Y = y; DX = dx; DY = dy}
    end

let rec getNumbers (nstr: char list) (numbers: int list) (str: char list) =
    match str with
    | [] -> numbers
    | head::tail ->
        if "-1234567890".Contains head then
            let nnstr = List.append nstr (List.singleton head)
            getNumbers nnstr numbers tail
        else if not (List.isEmpty nstr) then
            let nnumbers = List.append numbers (List.singleton(nstr |> Util.implodeString |> int))
            getNumbers List.empty nnumbers tail
        else
            getNumbers nstr numbers tail

let PointAtTime (p: Point) (t: int) =
    let x' = t * p.DX + p.X
    let y' = t * p.DY + p.Y
    Point(x', y', p.DX, p.DY)

let hasNeighbour (point: Point) (allPoints: Point list) =
    allPoints
    |> List.exists (fun p ->
        (p.X, p.Y) = (point.X - 1, point.Y) ||
        (p.X, p.Y) = (point.X - 1, point.Y + 1) ||
        (p.X, p.Y) = (point.X, point.Y + 1) ||
        (p.X, p.Y) = (point.X + 1, point.Y + 1) ||
        (p.X, p.Y) = (point.X + 1, point.Y) ||
        (p.X, p.Y) = (point.X + 1, point.Y - 1) ||
        (p.X, p.Y) = (point.X, point.Y - 1) ||
        (p.X, p.Y) = (point.X - 1, point.Y - 1))

let printPoints (points: Point list) =
    let minX =
        points
        |> List.minBy (fun p -> p.X)
        |> fun p -> p.X

    let maxX =
        points
        |> List.maxBy (fun p -> p.X)
        |> fun p -> p.X

    let minY =
        points
        |> List.minBy (fun p -> p.Y)
        |> fun p -> p.Y

    let maxY =
        points
        |> List.maxBy (fun p -> p.Y)
        |> fun p -> p.Y

    let positions =
        points
        |> List.map (fun p -> (p.X, p.Y))
        |> List.sort

    let grid = seq {
        for y in minY .. maxY do
            for x in minX .. maxX do
                if List.contains (x, y) positions then
                    yield '#'
                else
                    yield '.'
    }

    grid
    |> Seq.chunkBySize (maxX - minX + 1)
    |> Seq.iter (fun row -> printfn "%s" (Util.implodeString (List.ofArray row)))
    //|> Seq.iter sw.WriteLine

let rec part1and2 t input =
    if t % 10 = 0 then
        printfn "%i seconds passed" t

    let allPoints =
        input
        |> List.map (fun pat -> pat t)

    let allNeighbours =
        allPoints
        |> List.forall (fun p -> hasNeighbour p allPoints)

    if allNeighbours then
        printPoints allPoints
        t
    else
        part1and2 (t + 1) input

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines( __SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map Util.explodeString
        |> Seq.map (getNumbers List.empty List.empty)
        |> Seq.map (fun p -> Point(p.[0], p.[1], p.[2], p.[3]))
        |> Seq.map PointAtTime
        |> List.ofSeq

    input
    |> part1and2 0
    |> printfn "Took %i seconds"

    0 // return an integer exit code
