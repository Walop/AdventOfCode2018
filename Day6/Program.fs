open System
open System.IO
open AdventOfCode

type Point =
    struct
        val Id: int
        val X: int
        val Y: int

        new(id, x, y) = { Id = id; X = x; Y = y }
    end

type Boundaries =
    struct
        val MinX: int
        val MaxX: int
        val MinY: int
        val MaxY: int

        new(x1, y1, x2, y2) =
            { MinX = x1
              MinY = y1
              MaxX = x2
              MaxY = y2 }
    end

let getDistances (gridPoint: Point) (inputPoints: Point seq) =
    inputPoints
    |> Seq.map (fun p -> (p.Id, abs (gridPoint.X - p.X) + abs (gridPoint.Y - p.Y)))

let findClosest (gridPoint: Point) (inputPoints: Point seq) =
    let distances = getDistances gridPoint inputPoints

    let min = distances |> Seq.minBy (fun d -> snd d) |> snd

    let closestPoints = distances |> Seq.filter (fun d -> (snd d) = min)

    if Seq.length closestPoints > 1 then
        Point(99, gridPoint.X, gridPoint.Y)
    else
        closestPoints
        |> Seq.exactlyOne
        |> fun d -> Point(fst d, gridPoint.X, gridPoint.Y)

let writeOutput (grid: Point list) =
    let rows =
        grid
        |> List.groupBy (fun p -> p.X)
        |> List.map (fun g -> snd g)
        |> List.map (fun p -> List.map (fun (p: Point) -> p.Id |> sprintf "%02i") p)
        |> List.map (fun r -> String.concat "|" r)

    use sw = new StreamWriter("output")

    for row in rows do
        sw.WriteLine(row)

let getBoundaries (input: Point seq) =
    let minX = Seq.minBy (fun (p: Point) -> p.X) >> fun p -> p.X
    let maxX = Seq.maxBy (fun (p: Point) -> p.X) >> fun p -> p.X
    let minY = Seq.minBy (fun (p: Point) -> p.Y) >> fun p -> p.Y
    let maxY = Seq.maxBy (fun (p: Point) -> p.Y) >> fun p -> p.Y

    input |> fun i -> Boundaries(minX i, minY i, maxX i, maxY i)

let getGrid (boundaries: Boundaries) =
    [ for x in boundaries.MinX .. boundaries.MaxX do
          for y in boundaries.MinY .. boundaries.MaxY do
              yield Point(0, x, y) ]

let part1 (input: Point seq) =
    let boundaries = getBoundaries input

    let grid = getGrid boundaries

    let sndLength = snd >> List.length

    let claims = grid |> List.map (fun p -> findClosest p input)
    //|> writeOutput

    let excluded =
        claims
        |> Seq.filter (fun p ->
            p.X = boundaries.MinX
            || p.X = boundaries.MaxX
            || p.Y = boundaries.MinY
            || p.Y = boundaries.MaxY)
        |> Seq.map (fun p -> p.Id)
        |> Set.ofSeq
        |> Set.add 99

    claims
    |> List.filter (fun p -> not (Seq.contains p.Id excluded))
    |> List.groupBy (fun p -> p.Id)
    |> List.maxBy (fun g -> sndLength g)
    //|> fst
    //|> printfn "%i"
    |> sndLength
    |> printfn "Largest bounded area: %i"

    grid

let part2 input grid =

    let sumDistances = Seq.map (fun d -> snd d) >> Seq.sum

    grid
    |> List.map (fun p -> getDistances p input)
    |> List.map (fun d -> sumDistances d)
    |> List.filter (fun ds -> ds < 10000)
    |> List.length
    |> printfn "Area of region within 10000 of all points %i"

[<EntryPoint>]
let main argv =
    let parsePoint =
        fun (s: string) -> s.Split [| ',' |]
        >> Array.map (fun (s: string) -> s.Trim ' ')
        >> Array.map (fun s -> Int32.Parse s)
        >> fun s -> (s.[0], s.[1])

    let input =
        Util.readLines (__SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map parsePoint
        |> Seq.indexed
        |> Seq.map (fun (i, (x, y)) -> Point(i, x, y))

    input |> part1 |> part2 input

    0 // return an integer exit code
