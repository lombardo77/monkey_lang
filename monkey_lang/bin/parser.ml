exception ParseError of string

(* needs work to handle other types of exp variants, for now handles the Ast.Number variant *)
let rec parse_exp (t : Tokens.token list) : Ast.statement*Tokens.token list = 
    match t with
    | Integer x::SemCol::t -> (Exp (Number x), t)
    | Ident x::SemCol::t -> (Exp (Var x), t)
    | SemCol::t -> parse_exp t
    | _ -> raise (ParseError "Parse error in parse_exp function")

let get_type (a, _) = a
let get_tail (_, b) = b

(* takes a list with a Let as its head and returns a Ast.Let variant *)
let parse_let (t : Tokens.token list) : Ast.statement*Tokens.token list =
    match t with
    | Ident x::_::t -> let expr = parse_exp t in (Ast.Let(x, get_type expr), get_tail expr)
    | _ -> raise (ParseError "Let statement is wrong")

let parse_return (t : Tokens.token list) : Ast.statement*Tokens.token list =
    let expr = parse_exp t in
    (Ast.Return(get_type expr), get_tail expr)

(* the S rule of our parse tree *)
let rec parse_program (t : Tokens.token list) : Ast.statement list =
    let proceed p t = let a = p t in (get_type a)::(parse_program (get_tail a)) in
    match t with
    | Let::t -> proceed parse_let t
    | Return::t -> proceed parse_return t
    | Ident x::t -> proceed parse_exp (Ident x::t)
    | _::t -> parse_program t
    | [] -> []

(* helper functions for printing *)
let string_of_exp (i : Ast.exp) : string = 
    match i with
    | Number x -> Printf.sprintf "Number %d" x
    | Var x -> Printf.sprintf "Var %s" x
    | _ -> "undefined "

let string_of_st (i : Ast.statement) : string = 
    match i with
    | Let (a, Exp b) -> Printf.sprintf "Let(%s, %s);"  a (string_of_exp b)
    | Return Exp a -> Printf.sprintf "Return (%s);"  (string_of_exp a)
    | Exp a -> Printf.sprintf "%s;"  (string_of_exp a)
    | _ -> " "

let rec print_stl (s : Ast.statement list ) : unit = 
    match s with
    | h::t -> Printf.printf "%s\n" (string_of_st h); print_stl t
    | [] -> ()


