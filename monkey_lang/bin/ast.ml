type value = 
    | Int_Val of int
    | Bool_Val of bool
    | Closure of environment * string * exp

and exp =
    | Var of string
    | Number of int
    | Bool of bool
    | Neg of exp
    | Add of exp * exp
    | Mult of exp * exp
    | Div of exp * exp
    | Sub of exp * exp
    | And of exp * exp
    | Or of exp * exp
    | Not of exp
    


and environment = (string * value) list

and statement =  
    | Let of string * exp
    | Return of exp 
    | Exp of exp 
    | If of string * exp 
    | Else of string * exp 
    | While of string * exp

