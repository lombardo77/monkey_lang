type token = 
    (* non numberic/alphabetical tokens *)
    | LParen
    | RParen
    | Assign
    | LT
    | GT 
    | SemCol
    | LBrace
    | RBrace
    | Comma
    (* arithmetic tokens *)
    | Add
    | Minus
    | Mult 
    | Div
    (* numeric/alphabetical tokens *)
    (* integers and indetifiers *)
    | Ident of string
    | Integer of int
    (* keywords *)
    | Fn
    | Let 
    | For
    | While
    | If
    | Elif
    | Else
    | Return
    (* logical operators *)
    | True
    | False
    | NotEq
    | Not
    | Eq
    (* special tokens *)
    | Illegal
    | EOF
