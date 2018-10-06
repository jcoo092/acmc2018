// Learn more about F# at http://fsharp.org

open System

type IO = bool

type L1 = {a: bool; Xi: int; T: int;}
type L2 = {Ai: int; Aij: List<Tuple<int,int>>; s: bool; Ri: int; Gi: int; Bi: int; Ti: int; XY: bool;} with //For XY, true means X, false means Y
    member this.Guard () =
        this.s

let guardSExists l2 = l2.s

// In all of the following functions, i is used to represent the current iteration of the process

let r1i i (maxSteps: int) l1 =
    if i <= maxSteps then
        {l1 with Xi = l1.Xi + 1;}
    else
        l1

let r12n3 i l1 l0 =
    if l1.a && l1.T > 0 then
        () // send out yes
    else
        () // do nothing

let r12n4 i maxSteps l1 =
    if l1.a && l1.Xi = (maxSteps + 1) && l1.T > 0 then // The T part is a guard here
        () // send out no
    else
        () // do nothing

let r22i1 i (l2: L2) =
    //if l2.s then // l2.s representing the guard
    //if l2.Guard() then
    if guardSExists l2 then
        [{l2 with Ri = l2.Ri + 1; Ai = l2.Ai + 1}; {l2 with Ai = 0; Ti = l2.Ai}]
    else
        [l2]

let r22i l2 i n =
    if l2.Ti > 0 && i <= (n-1) then
        [{l2 with Bi = 1; Ai = i + 1; Ti = 0;}; {l2 with Gi = 1; Ai = i + 1; Ti = 0;}]
    else
        [l2]

let r22nm1 i l2 =
    if l2.Ai = i && guardSExists l2 then //l2.s representing the guard
        [{l2 with Ri = i; XY = true; Ai = 0; Ti = 0;}; {l2 with Ti = i;}]
    else
        [l2]

let r22n i l2 =
    if l2.Ti = i then
        [{l2 with Ti = 0; Bi = i; XY = true}; {l2 with Ti = 0; Gi = i; XY = true;}]
    else
        [l2]

let r22np1 i l2 Aij L2s =
    if true then
        {l2 with s = false;}
        let neighbours = List.filter (fun (a,_) -> a = l2.Ai) Aij |> List.map snd
        let bbb = Seq.any ()
    else
        l2

let numNodes = 3
let maxSteps = 2 * numNodes + 2

[<EntryPoint>]
let main argv =
    let mut iteration = 0

    let E = [(1, 2); (2,3); (1,3)]

    let C1 = {a = true; Xi = 1; T = 0;}
    let C2 = List.singleton {Ai = 1; Aij = E; s = true; Ri = 0; Gi = 0; Bi = 0; Ti = 0; XY = true;}
    printfn "Hello World from F#!"
    0 // return an integer exit code
