type value = 
    | Int_Val of int
    | Bool_Val of bool
    | Closure of environment * ident * exp

and ident = Var of string

and exp =
    | Number of int
    | Add of exp * exp
    | Mult of exp * exp
    | Div of exp * exp
    | Sub of exp * exp

and environment = (ident * value) list

and statement =  
    | Let of ident * exp 
    | If of string * exp 
    | Else of string * exp 
    | While of string * exp

let state_of_string s =
    match s with
    | Let (Var x, Number y) -> Printf.sprintf "Let %s %d" x y
    | _ -> "none"
