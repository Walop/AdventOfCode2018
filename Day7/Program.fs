open System
open AdventOfCode

let rec getUpperLetters (letters: char list) (str: char list) =
    match str with
    | [] -> letters
    | head::tail ->
        if List.contains head ['A' .. 'Z'] then
            let nletters = List.append letters (List.singleton head)
            getUpperLetters nletters tail
        else
            getUpperLetters letters tail

let rec doSteps doneSteps input =
    if Seq.isEmpty input then
        doneSteps
    else
        printfn "Done %A" doneSteps
        printfn "Input left: %A" input
        input
        |> Seq.iter (fun r -> printfn "Missing steps for %c: %A" (fst r) (Seq.except doneSteps (snd r)))
        
        let nextStep =
            input
            |> Seq.find (fun r -> Seq.length (Seq.except doneSteps (snd r)) = 0)
        
        let ndoneSteps = Seq.append doneSteps (Seq.singleton(fst nextStep))
        let ninput = Seq.filter (fun i -> fst nextStep <> fst i) input

        doSteps ndoneSteps ninput

let part1 initialSteps input =
    doSteps initialSteps input
    |> List.ofSeq
    |> (printfn "%s" << Util.implodeString)

[<EntryPoint>]
let main argv =
    let input =
        Util.readLines( __SOURCE_DIRECTORY__ + "\\input")
        |> Seq.map Util.explodeString
        |> Seq.map (getUpperLetters List.empty)
        |> Seq.map (List.tail >> List.rev)
        |> Seq.groupBy Seq.head
        |> Seq.map (fun g -> (fst g, Seq.map List.last (snd g)))
        |> Seq.sortBy fst

    let steps = Seq.map fst input
    let requirements = Seq.concat (Seq.map snd input)
    let initialSteps = Seq.except steps requirements

    part1 initialSteps input

    0 // return an integer exit code
