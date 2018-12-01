// Learn more about F# at http://fsharp.org

open System
open Hopac
open Hopac.Infixes


type IO = bool

type Colour =
    | Red of int
    | Green of int
    | Blue of int

type L0 = {isPossible: bool; chan: Ch<bool>} with
    member this.recv () = job {
           return! Ch.take this.chan
       }

type L1 = {a: bool; Xi: int; T: bool; chan: Ch<bool>} with

    member this.recv () = job {
           let! newT = Ch.take this.chan
           return newT
       }
    member this.sendToEnvironment (env: L0) = job {
            do! Ch.give env.chan this.T
    }



type L2 = {Ai: int; Aij: List<Tuple<int,int>>; s: bool; C: Colour list; T: bool;
             XY: bool;} with //For XY, true means X, false means Y
(*     member this.Guard () =
        this.s *)
    member this.send (c1: L1) = job {
        if this.T then
            do! Ch.give c1.chan this.T
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
        () // send out yes
    else
        () // do nothing

let r12n4 i maxSteps l1 =
    if l1.a && l1.Xi = (maxSteps + 1) && l1.T then // The T part is a guard here
        () // send out no
    else
        () // do nothing

let r22im1 i (l2: L2) =
    //if l2.s then // l2.s representing the guard
    //if l2.Guard() then
    if guardSExists l2 then
        //[{l2 with Ri = i; Ai = i + 1;}; {l2 with Ai = 0; T = true}]
        [{l2 with C = Red i :: l2.C; Ai = i + 1;}; {l2 with Ai = 0; T = true}]
    else
        [l2]

let r22i i n l2  =
    if l2.T && i < n then
        //[{l2 with Bi = i; Ai = i + 1; T = false;}; {l2 with Gi = i; Ai = i + 1; T = false;}]
        [{l2 with C = Blue i :: l2.C; Ai = i + 1; T = false;}; {l2 with C = Green i :: l2.C; Ai = i + 1; T = false;}]
    else
        [l2]

let r22Combo i n l2 = r22im1 i l2 |> List.collect (r22i i n)

let r22nm1 n l2 =
    if l2.Ai = n && guardSExists l2 then //l2.s representing the guard
        [{l2 with C = Red n :: l2.C; XY = true; Ai = 0; T = false;}; {l2 with T = true;}]
    else
        [l2]

let r22n n l2 =
    if l2.Ai = n then
        [{l2 with T = false; C = Blue n :: l2.C; XY = true}; {l2 with T = false; C = Green n :: l2.C; XY = true;}]
    else
        [l2]

let r22nCombo n l2 = r22nm1 n l2 |> List.collect (r22n n)

let guardColours l2 =
    let checkSameColour edge =
        let i,j = edge
        List.contains (Red i) l2.C && List.contains (Red j) l2.C ||
        List.contains (Blue i) l2.C && List.contains (Blue j) l2.C ||
        List.contains (Green i) l2.C && List.contains (Green j) l2.C

    List.exists checkSameColour l2.Aij

let r22np1 l2 =
    if guardColours l2 then
        {l2 with s = false;}
    else
        l2

(* let guardColours neighbours l2 =
    List.exists (fun n -> false) neighbours

let r22np1 n _Aij l2s l2 =
    if true then
        {l2 with s = false;}
        let neighbours = List.filter (fun (a,_) -> a = l2.Ai) Aij //|> List.map snd
        //let bbb = Seq.any ()
    else
        l2 *)

//let numNodes = 3
//let maxSteps = 2 * numNodes + 2

(* let odd x = x % 2 <> 0
let even x = x % 2 = 0 *)

let (|Even|Odd|) num = if num % 2 = 0 then Even else Odd

[<EntryPoint>]
let main argv =
    let mut iteration = 0

    let E = [(1, 2); (2,3); (1,3); (1,4);]
    let numNodes = List.length E
    let maxSteps = 2 * numNodes + 2

    let mutable env = {isPossible = false; chan = Ch ()}
    let mutable C1 = {a = true; Xi = 1; T = false; chan = Ch()}
    let mutable C2 = List.singleton {Ai = 1; Aij = E; s = true; C = List.empty; T = false; XY = true;}

    // using C1.Xi as the counter
    while C1.Xi <= (maxSteps - 4) do
        let i = C1.Xi
        C1 <- r1i i maxSteps C1
        (* if odd i then
            let j = (i + 1) / 2
            //printfn "j = %d" j
            C2 <- List.collect (r22im1 j) C2
        else
            let j = i / 2
            //printfn "j = %d" j
            C2 <- List.collect (r22i j numNodes) C2 *)


        match i with
        | Odd ->
            let j = (i + 1) / 2
            C2 <- List.collect (r22im1 j) C2
        | Even ->
            let j = i / 2
            C2 <- List.collect (r22i j numNodes) C2

    C2 <- List.collect (r22nCombo numNodes) C2
    C2 <- List.map r22np1 C2 |> List.filter (fun c -> c.s)

    printfn "C1: %A" C1
    List.iter (fun c -> printfn "%A" c) C2

    let sends = List.filter (fun c -> c.T) C2 |> List.map (fun (c: L2) -> c.send C1)
    Job.conIgnore sends |> ignore
    let cc = C1.recv() |> Promise.start |> run
    let finalT = Job.start (C1.recv()) |> run


    (* for i in 1..(numNodes - 1) do
        C1 <- r1i i maxSteps C1
        C2 <- List.collect (r22Combo i numNodes) C2

    printfn "C1: %A" C1
    List.iter (fun c -> printfn "%A" c) C2 *)

    //printfn "C1: %A" C1
    //printfn "C2: %A" C2

    (* C2 <- List.collect (r22nCombo numNodes) C2
    C2 <- List.map r22np1 C2 |> List.filter (fun c -> c.s)

    printfn "C1: %A" C1
    //printfn "C2: %A" C2
    List.iter (fun c -> printfn "%A" c) C2

    if not (List.isEmpty C2) then
        () //send a T to C1
    else
        () // do nothing *)

    // if C1 has a T, report true
    // else, report false

    //printfn "Hello World from F#!"
    0 // return an integer exit code
