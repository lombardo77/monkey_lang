type token = 
    | LET 
    | LPAREN
    | RPAREN
    | EQ
    | IDENT of string
    | INTEGER of int 
    | SEMCOL 
    | ILLEGAL
