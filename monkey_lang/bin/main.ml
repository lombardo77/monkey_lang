open Tokens
open Printf

let _regex = Str.regexp {|\([ ]+\)|};;


let get_token s =
    let f x = Char.escaped x in
    match s with
    | '(' -> (LPAREN, f s)
    | ')' -> (RPAREN, f s)
    | '=' -> (EQ, f s)
    | ';' -> (SEMCOL, f s)
    | '+' -> (ADD, f s)
    | '-' -> (MINUS, f s)
    | '*' -> (MULT, f s)
    | '/' -> (DIV, f s)
    | '}' -> (RBRACE, f s)
    | '{' -> (LBRACE, f s)
    | ',' -> (COMMA, f s)
    | _ -> (ILLEGAL, f s)

let is_letter c =
    let open Char in
    let av = code c in 
    if (av >= code 'A' && av <= code 'Z' 
        || av >= code 'a' && av <= code 'z')
    then true 
    else false

let is_number c =
    let open Char in
    let av = code c in 
    if (av >= code '0' && av <= code '9') 
    then true 
    else false

(* is letter or number *)
let is_lorn c = is_number c || is_letter c

(* pretty printing functions *)
let rec pp l =
    match l with
    | h :: t ->(
        match h with
        | (LET, _) -> printf "LET, "
        | (LPAREN, _) -> printf "LPAREN, "
        | (RPAREN, _) -> printf "RPAREN, "
        | (LBRACE, _) -> printf "LBRACE, "
        | (RBRACE, _) -> printf "RBRACE, "
        | (EQ, _) -> printf "EQ, "
        | (SEMCOL, _) -> printf "SEMCOL, "
        | (ADD, _) -> printf "ADD, "
        | (MINUS, _) -> printf "MINUS, "
        | (MULT, _) -> printf "MULT, "
        | (DIV, _) -> printf "DIV, "
        | (IF, _) -> printf "IF, "
        | (ELSE, _) -> printf "ELSE, "
        | (IDENT a, _) -> printf "IDENT(%s), " a
        | (INTEGER a, _) -> printf "IDENT(%d), " a
        | (ILLEGAL, _) -> printf "ILL, "
        | _ -> ()
    ); pp t
    | [] -> printf "\n"

(* char list to string*)
let clts cl = String.of_seq (List.to_seq cl)

(* walks though string until reaches a non-alphabetical character*)
let read_word s n =
    let rec aux s n =
        let open String in
        let c = get s n in
        if is_lorn c then c::(aux s (n + 1)) else [] in
    clts (aux s n)

(* find new index after ecounntering alphabetica char helper function*)
let rec new_index s n =
    let open String in 
    let c = get s n in 
    if is_lorn c then new_index s (n + 1) else n

(* read number*)
let read_number s n =
    let rec aux s n =
        let open String in
        let c = get s n in
        if is_number c then c::(aux s (n + 1)) else [] in
    clts (aux s n)

let get_word_token s =
    match s with
    | "let" -> (LET, s)
    | "fn" -> (FN, s)
    | "for" -> (FOR, s)
    | "while" -> (WHILE, s)
    | "if" -> (IF, s)
    | "else if" -> (ELIF, s)
    | "else" -> (ELSE, s)
    | _ -> (IDENT s, s)

let get_int_token s = (INTEGER (int_of_string s), s)

let rec read_input s n len =
    let skip f = f s (n + 1) len in
    let open String in
    if n < len  then (
        match get s n with
        | ' ' ->  skip read_input
        | '\t' -> skip read_input
        | '\n' -> skip read_input
        | h ->  if is_letter h then (get_word_token (read_word s n))::( read_input s (new_index s n) len)
                else if is_number h then (get_int_token (read_number s n))::(read_input s (new_index s n) len)
                else ( get_token h )::( read_input s (n + 1) len)
    )
    else [] 
;;

let input = 
    "let x0 = 9 + 9; 
    if x = 9 { x + 1};"
let tokens = read_input input 0 (String.length input);;
pp tokens;;

