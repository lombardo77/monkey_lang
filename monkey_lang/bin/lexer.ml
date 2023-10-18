open Tokens
open Printf

(**************************************)
(**************************************)
(************ RECOGNIZERS *************)
(**************************************)
(**************************************)

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

(**************************************)
(**************************************)
(************** READERS ***************)
(**************************************)
(**************************************)

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

exception TokenError of string
(* read number*)
let read_number s n =
    let rec aux s n =
        let open String in
        let c = get s n in
        if is_number c then c::(aux s (n + 1)) else if is_letter c then raise (TokenError "illegal token") else [] in
    clts (aux s n)



(**************************************)
(**************************************)
(************ GET TOKENS **************)
(**************************************)
(**************************************)

let get_sym_token s =
    match s with
    | '(' -> LParen
    | ')' -> RParen 
    | '=' -> Assign
    | '<' -> LT
    | '>' -> GT
    | ';' -> SemCol
    | '+' -> Add
    | '-' -> Minus
    | '*' -> Mult
    | '/' -> Div
    | '}' -> RBrace
    | '{' -> LBrace
    | ',' -> Comma
    | '!' -> Not
    | _ -> Illegal

let get_word_token s =
    match s with
    | "let" -> Let
    | "fn" -> Fn
    | "for" -> For
    | "while" -> While
    | "if" -> If
    | "else if" -> Elif
    | "else" -> Else
    | "return" -> Return
    | _ -> Ident s

let get_int_token s = Integer (int_of_string s)

(* looks at the next char to determine whether symbol is e.g., '=' or '=='*)
let peek s n = String.get s (n + 1)

let get_token s n =
    let c = String.get s n in
    if is_letter c then (get_word_token (read_word s n), new_index s n)
    else if is_number c then (get_int_token (read_number s n), new_index s n)
    else (match c with
            | '=' -> if peek s n = '=' then (Eq, n + 2) else (get_sym_token c, n + 1)
            | '!' -> if peek s n = '=' then (NotEq, n + 2) else (get_sym_token c, n + 1)
            | _ -> (get_sym_token c, n + 1)
    )

(**************************************)
(**************************************)
(********** INPUT READER **************)
(**************************************)
(**************************************)

let first (x, _) = x
let second (_, y) = y

let rec read_input s n len =
    let skip f = f s (n + 1) len in
    let open String in
    if n < len  then (
        let token_tuple = get_token s n in
        let token = first token_tuple in
        let ind = second token_tuple in
        match get s n with
        | ' ' ->  skip read_input
        | '\t' -> skip read_input
        | '\n' -> skip read_input
        | h ->  if is_letter h || is_number h then token::(read_input s ind len)
                else token::( read_input s ind len)
    )
    else [EOF] 
;;

(**************************************)
(**************************************)
(************** PRINT *****************)
(**************************************)
(**************************************)


(* pretty printing functions *)
let rec pp l =
    match l with
    | h :: t ->(
        match h with
        | Let -> printf "Let, "
        | LParen -> printf "LParen, "
        | RParen -> printf "RParen, "
        | LBrace -> printf "LBrace, "
        | RBrace -> printf "RBrace, "
        | Eq -> printf "Eq, "
        | NotEq -> printf "NotEq, "
        | Not -> printf "Not, "
        | Assign -> printf "Assign, "
        | LT -> printf "LT, "
        | GT -> printf "GT, "
        | SemCol -> printf "SemCol, "
        | Comma -> printf "Comma, "
        | Add -> printf "Add, "
        | Minus -> printf "Minus, "
        | Mult -> printf "Mult, "
        | Div -> printf "Div, "
        | If -> printf "If, "
        | For -> printf "For, "
        | While -> printf "While, "
        | Else -> printf "Else, "
        | Fn -> printf "Fn, "
        | Ident a -> printf "Ident(%s), " a
        | Integer a -> printf "Int(%d), " a
        | Return -> printf "Return, "
        | EOF -> printf "EOF, "
        | Illegal -> printf "Ill, "
        | _ -> ()
    ); pp t
    | [] -> printf "\n"

let rec ppp l =
    match l with
    | h::t -> pp h; ppp t
    | [] -> ()



