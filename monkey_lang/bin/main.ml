open Tokens
open Printf

let _regex = Str.regexp {|\([ ]+\)|};;


let get_token s = 
    match s with
    | '(' -> (LPAREN, s)
    | ')' -> (RPAREN, s)
    | '=' -> (EQ, s)
    | ';' -> (SEMCOL, s)
    | '+' -> (ADD, s)
    | '-' -> (MINUS, s)
    | '*' -> (MULT, s)
    | '/' -> (DIV, s)
    | '}' -> (RBRACE, s)
    | '{' -> (LBRACE, s)
    | ',' -> (COMMA, s)
    | _ -> (COMMA, s)

let is_letter c =
    let open Char in
    let av = code c in 
    if (av >= code 'A' && av <= code 'Z' 
        || av >= code 'a' && av <= code 'z')
    then true 
    else false

(* pretty printing functions *)
let rec pp l =
    match l with
    | h :: t ->(
        match h with
        | (LET, _) -> printf "LET, "
        | (LPAREN, _) -> printf "LPAREN, "
        | (RPAREN, _) -> printf "RPAREN, "
        | (EQ, _) -> printf "EQ, "
        | (SEMCOL, _) -> printf "SEMCOL, "
        | (ADD, _) -> printf "ADD, "
        | (MINUS, _) -> printf "MINUS, "
        | (MULT, _) -> printf "MULT, "
        | (DIV, _) -> printf "DIV, "
        | (ILLEGAL, _) -> printf "ILL, "
        | _ -> ()
    ); pp t
    | [] -> printf "\n"

let rec read_input s n len = 
    let open String in
    if n < len  then (
        match get s n with
        | ' ' -> read_input s (n + 1) len
        | h -> ( get_token h )::( read_input s (n + 1) len)
    )
    else [] 
;;

let input = "let x = 9 + 9;"
let tokens = read_input input 0 (String.length input);;
pp tokens;;

