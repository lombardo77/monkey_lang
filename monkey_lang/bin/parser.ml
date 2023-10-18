(*open Ast*)
(*open Lexer*)

exception ParseError of string
(* if head of list t is a Tokens.Ident, then returns a Ast.Var else fails with a ParseError*)
let parse_ident (t : Tokens.token list) : Ast.ident = 
    match t with
    | (Tokens.Ident x)::_ -> Ast.Var x
    | _ -> raise (ParseError "Parse error in parse_ident function")

(* needs work to handle other types of exp variants, for now handles the Ast.Number variant *)
let rec parse_exp (t : Tokens.token list) : Ast.exp = 
    match t with
    | (Tokens.Integer x)::_ -> Ast.Number x
    | (Tokens.Assign)::t -> parse_exp t
    | (Tokens.Ident _)::t -> parse_exp t
    | _ -> raise (ParseError "Parse error in parse_exp function")

(* takes a list with a Let as its head and returns a Ast.Let variant *)
let parse_let (t : Tokens.token list) : Ast.statement = 
    Ast.Let(parse_ident t, parse_exp t)

(* the S rule of our parse tree *)
let rec parse_program (t : Tokens.token list) : Ast.statement list =
    match t with
    | Tokens.Let::t -> (parse_let t)::(parse_program t)
    | _::t -> parse_program t
    | [] -> []


(* HELPER FUNCTIONS FOR PRINTING *)
let string_of_ident (i : Ast.ident) = 
    match i with
    | Ast.Var x -> Printf.sprintf "%s" x

let string_of_exp (i : Ast.exp) = 
    match i with
    | Ast.Number x -> Printf.sprintf "%d" x
    | _ -> " "

let rec print_stl (s : Ast.statement list ) = 
    match s with
    | (Ast.Let(a, b))::t -> Printf.printf "LET %s <- %s\n"  (string_of_ident a) (string_of_exp b); print_stl t
    | _::_ -> ()
    | [] -> ()


