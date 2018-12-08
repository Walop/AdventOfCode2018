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

let rec part2 (time: int) (queue: (char * int) list)  (doneSteps: char list) (input: (char * (char seq)) list) =
    printfn "Time %i" time
    let progressQueue = List.map (fun q -> (fst q, (snd q) - 1)) queue
    let finishedSteps = List.filter (fun q -> (snd q) = 0) progressQueue
    let ongoingQueue = List.filter (fun q -> not (List.contains q finishedSteps)) progressQueue
    let availableQueue = 5 - (List.length ongoingQueue)

    let newDoneSteps = List.append doneSteps (List.map (fun q -> fst q) finishedSteps)

    let availableSteps =
        input
        |> List.filter (fun r -> List.length (List.except newDoneSteps (List.ofSeq (snd r))) = 0)
        |> List.map (fun a -> (fst a, 60 + (int (fst a) - 64)))

    let takeSteps =
        if List.length availableSteps > availableQueue then
            availableQueue
        else
            List.length availableSteps

    let newJobs = List.take takeSteps availableSteps
    let newInput = List.filter (fun i -> not (List.contains (fst i) (List.map (fun j -> fst j) newJobs))) input

    let newQueue = List.append ongoingQueue newJobs

    printfn "Queue %A" newQueue
    printfn "Done steps %A" newDoneSteps
    printfn "Input %A" newInput
    printfn "-------------"

    if List.isEmpty newQueue then
        time
    else
        part2 (time + 1) newQueue newDoneSteps newInput


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

    // part1 initialSteps input

    let initialQueue = Seq.map (fun s -> (s, 60 + (int s - 64))) initialSteps |> List.ofSeq

    // compensate for inital queue processing
    part2 1 initialQueue List.empty (List.ofSeq input)
    |> printfn "Took time %i"

    0 // return an integer exit code
