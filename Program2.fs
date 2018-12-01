open System.Collections.Concurrent

//type SolutionFound = bool // type alias denoting that a solution has been found - corresponds to T in the paper

(* type C1Objects =
    | Step of int

type C2Objects =
    | A of int
    | Red of int
    | Green of int
    | Blue of int *)

type Objects =
    | Step of int
    | A of int
    | Red of int
    | Green of int
    | Blue of int
    | SolutionFound of bool // - corresponds to T in the paper

type C1 = {objects: Objects ConcurrentBag; rules: (C1 -> C1) list}
type C2 = {objects: Objects ConcurrentBag; rules: (C2 -> C2) list}

// rules for C1 go here

// let C1Rules = [rules1; rules2] etc