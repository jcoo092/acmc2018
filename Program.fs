// Learn more about F# at http://fsharp.org

open Hopac
open System


type IO = bool

type Colour =
    | Red of int
    | Green of int
    | Blue of int

type OutcomeMessage = IO * Colour list option

type L0 = {isPossible: bool; chan: Ch<OutcomeMessage>} with
    member this.recv () = job {
           return! Ch.take this.chan
       }

type L1 = {a: bool; Xi: int; T: bool; chan: Ch<OutcomeMessage>; solution: Colour list option} with
    member this.sendToEnvironment (env: L0)  = job {
            do! Ch.give env.chan (this.T, this.solution)
    }

    member this.recv () = job {
           let! (t, sol) = Ch.take this.chan
           return {this with T = t; solution = sol;}
       }


type L2 = {Ai: int; Aij: Tuple<int,int>[]; s: bool; C: Colour list; T: bool;} with
    member this.send (c1: L1) = job {
        if this.s then
            do! Ch.give c1.chan (true, Some(this.C))
        else
            ()
    }

let guardSExists l2 = l2.s

// In all of the following functions, i is used to represent the current iteration of the process

let r1i i (maxSteps: int) l1 =
    if i <= maxSteps then
        {l1 with Xi = i + 1;}
    else
        l1

let r12n3 i l1 l0 =
    if l1.a && l1.T then
        run (l1.sendToEnvironment l0)
    else
        () // do nothing

let r12n4 i maxSteps l1 =
    if l1.a && l1.Xi = (maxSteps + 1) && l1.T then // The T part is a guard here
        () // send out no
    else
        () // do nothing

let r22im1 i (l2: L2) =
    if guardSExists l2 then // actually does nothing here really
        [{l2 with C = Red i :: l2.C; Ai = i + 1;}; {l2 with Ai = 0; T = true}]
    else
        [l2]

let r22i i n l2  =
    if l2.T && i < n then
        [{l2 with C = Blue i :: l2.C; Ai = i + 1; T = false;}; {l2 with C = Green i :: l2.C; Ai = i + 1; T = false;}]
    else
        [l2]

let r22Combo i n l2 = r22im1 i l2 |> List.collect (r22i i n)

let r22nm1 n l2 =
    if l2.Ai = n && guardSExists l2 then //l2.s representing the guard
        [{l2 with C = Red n :: l2.C; Ai = l2.Ai + 1; T = false;}; {l2 with T = true;}]
    else
        [l2]

let r22n n l2 =
    if l2.Ai = n then
        [{l2 with T = false; C = Blue n :: l2.C; }; {l2 with T = false; C = Green n :: l2.C; }]
    else
        [l2]

let r22nCombo n l2 = r22nm1 n l2 |> List.collect (r22n n)

let guardColours l2 =
    let checkSameColour edge =
        let i,j = edge
        List.contains (Red i) l2.C && List.contains (Red j) l2.C ||
        List.contains (Blue i) l2.C && List.contains (Blue j) l2.C ||
        List.contains (Green i) l2.C && List.contains (Green j) l2.C

    Array.exists checkSameColour (l2.Aij)

let r22np1 l2 =
    if guardColours l2 then
        {l2 with s = false;}
    else
        l2

let (|Even|Odd|) num = if num % 2 = 0 then Even else Odd

[<EntryPoint>]
let main argv =

    let timer = System.Diagnostics.Stopwatch ()

    let numNodes = 12

    (* let E = [for i in 1..numNodes do
                        for j in i..numNodes do
                            if i <> j then
                                yield (i, j)
                    ] *)

    (* let E = [
        for i in 2..numNodes do
            yield (1, i)
    ] *)

    timer.Start()

    //let E = [(1, 2); (1, 3); (1, 4);]
    let E = [|(1, 2); (2, 3); (3, 4); (3, 5); (3, 6); (4, 7); (5, 9); (6, 7); (6, 10); (7, 11); (8, 9); (9, 10); (9, 12);|]
    //let E = [(1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (1, 8); (1, 9); (1, 10)]
    //let E = [|(1, 3); (1, 4); (1, 6); (2, 4); (2, 5); (2, 7); (3, 5); (3, 8); (4, 9); (5, 10); (6, 7); (6, 10); (7, 8); (8, 9); (9, 10)|] // Petersen graph

    let maxSteps = 2 * numNodes + 2

    let mutable env = {isPossible = false; chan = Ch ()}
    let mutable C1 = {a = true; Xi = 1; T = false; chan = Ch(); solution = None}
    let mutable C2 = List.singleton {Ai = 1; Aij = E; s = true; C = List.empty; T = false; }

    // using C1.Xi as the counter
    while C1.Xi <= (maxSteps - 4) do
        let i = C1.Xi
        C1 <- r1i i maxSteps C1

        match i with
        | Odd ->
            let j = (i + 1) / 2
            C2 <- List.collect (r22im1 j) C2 //|> List.map r22np1 |> List.filter (fun c -> c.s)
        | Even ->
            let j = i / 2
            C2 <- List.collect (r22i j numNodes) C2

    C2 <- List.collect (r22nCombo numNodes) C2
    C2 <- List.map r22np1 C2 |> List.filter (fun c -> c.s)

    //printfn "C1: %A" C1
    //List.iter (fun c -> printfn "%A" c) C2

    let getAnswer = Promise.Now.delay (C1.recv())

    let communication =
        if List.isEmpty C2 then
            job {do! Ch.give C1.chan (false, None)}
        else
            let sender = List.head C2
            sender.send C1

    Job.start communication |> run

    C1 <- run (getAnswer)

    Job.start (C1.sendToEnvironment env) |> run
    let (_, outcome) = env.recv() |> run

    timer.Stop()

    match outcome with
    | Some colours -> printfn "Colouring is possible!  A potential solution is:  %A" colours
    | None -> printfn "No colouring is possible!"

    printfn "Total time taken was %02fs" timer.Elapsed.TotalSeconds

    //printfn "Hello World from F#!"
    0 // return an integer exit code
