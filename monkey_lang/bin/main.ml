open Tokens
open Str
open Printf

let regex = Str.regexp {|\([ ]+\)|};;

let f x = split regex x

let get_token s = 
    match s with
    | "let" -> (LET, s)
    | "(" -> (LPAREN, s)
    | ")" -> (RPAREN, s)
    | "=" -> (EQ, s)
    | ";" -> (SEMCOL, s)
    | _ -> (ILLEGAL, s)
;;

let rec pp l =
    match l with
    | h :: t ->(
        match h with
        | (LET, _) -> printf "LET, "
        | (LPAREN, _) -> printf "LPAREN, "
        | (RPAREN, _) -> printf "RPAREN, "
        | (EQ, _) -> printf "EQ, "
        | (SEMCOL, _) -> printf "SEMCOL, "
        | (ILLEGAL, _) -> printf "ILL, "
        | _ -> ()
    ); pp t
    | [] -> ()

let rec read_input s = 
    match s with
    | h::t -> ( get_token h )::( read_input t )
    | [] -> []
;;

let tokens = read_input (f "let x = 9") ;;

pp tokens;;

