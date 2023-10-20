exception ParseError of string

(* needs work to handle other types of exp variants, for now handles the Ast.Number variant *)
let rec parse_exp (t : Tokens.token list) : Ast.exp = 
    match t with
    | Integer x::_ -> Number x
    | Ident x::_ -> Var x
    | SemCol::t -> parse_exp t
    | _ -> raise (ParseError "Parse error in parse_exp function")

(* takes a list with a Let as its head and returns a Ast.Let variant *)
let parse_let (t : Tokens.token list) : Ast.statement =
    match t with
    | Ident x::_::t -> Ast.Let(x, parse_exp t)
    | _ -> raise (ParseError "Let statement is wrong")

(* the S rule of our parse tree *)
let rec parse_program (t : Tokens.token list) : Ast.statement list =
    match t with
    | Let::t -> (parse_let t)::(parse_program t)
    | _::t -> parse_program t
    | [] -> []

let string_of_exp (i : Ast.exp) = 
    match i with
    | Number x -> Printf.sprintf "Number %d" x
    | _ -> " "

let string_of_st (i : Ast.statement) = 
    match i with
    | Let (a, b) -> Printf.sprintf "Let(%s, %s)"  a (string_of_exp b)
    | _ -> " "

let rec print_stl (s : Ast.statement list ) = 
    match s with
    | h::t -> Printf.printf "%s\n" (string_of_st h); print_stl t
    | [] -> ()


