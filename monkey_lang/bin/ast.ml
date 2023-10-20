type value = 
    | Int_Val of int
    | Bool_Val of bool
    | Closure of environment * string * exp

and exp =
    | Var of string
    | Number of int
    | Add of exp * exp
    | Mult of exp * exp
    | Div of exp * exp
    | Sub of exp * exp

and environment = (string * value) list

and statement =  
    | Let of string * exp 
    | If of string * exp 
    | Else of string * exp 
    | While of string * exp

