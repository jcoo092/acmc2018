﻿// Learn more about F# at http://fsharp.org

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
           printfn "L1 started receiving"
           return! Ch.take this.chan
       }
    member this.sendToEnvironment (env: L0) = job {
            do! Ch.give env.chan this.T
    }



type L2 = {Ai: int; Aij: List<Tuple<int,int>>; s: bool; C: Colour list; T: bool;
             XY: bool;} with //For XY, true means X, false means Y
(*     member this.Guard () =
        this.s *)
    member this.send (c1: L1) = job {
        printfn "L2 send entered"
        if this.s then
            do! Ch.give c1.chan this.T
            printfn "L2 finished sending"
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
    //if l2.s then // l2.s representing the guard
    //if l2.Guard() then
    if guardSExists l2 then
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

let (|Even|Odd|) num = if num % 2 = 0 then Even else Odd

[<EntryPoint>]
let main argv =
    let mut iteration = 0

    let E = [(1, 2); (2,3); (3,4);]
    let numNodes = List.length E
    let maxSteps = 2 * numNodes + 2

    let mutable env = {isPossible = false; chan = Ch ()}
    let mutable C1 = {a = true; Xi = 1; T = false; chan = Ch()}
    let mutable C2 = List.singleton {Ai = 1; Aij = E; s = true; C = List.empty; T = false; XY = true;}

    // using C1.Xi as the counter
    while C1.Xi <= (maxSteps - 4) do
        let i = C1.Xi
        C1 <- r1i i maxSteps C1

        match i with
        | Odd ->
            let j = (i + 1) / 2
            C2 <- List.collect (r22im1 j) C2
        | Even ->
            let j = i / 2
            C2 <- List.collect (r22i j numNodes) C2

    C2 <- List.collect (r22nCombo numNodes) C2
    C2 <- List.map r22np1 C2 |> List.filter (fun c -> c.s)

    //printfn "C1: %A" C1
    //List.iter (fun c -> printfn "%A" c) C2

    let getAnswer = Promise.Now.delay (C1.recv())

    let send = List.head C2
    Job.start (send.send C1) |> run
    let finalT = run (getAnswer)
    printfn "finalT: %A" finalT

    // if C1 has a T, report true
    // else, report false

    //printfn "Hello World from F#!"
    0 // return an integer exit code
