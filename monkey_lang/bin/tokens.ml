type token = 
    (* non numberic/alphabetical tokens *)
    | LPAREN
    | RPAREN
    | EQ
    | LT
    | GT 
    | SEMCOL
    | LBRACE
    | RBRACE
    | COMMA
    (* arithmetic tokens *)
    | ADD
    | MINUS
    | MULT
    | DIV
    (* numeric/alphabetical tokens *)
    (* integers and indetifiers *)
    | IDENT of string
    | INTEGER of int
    (* keywords *)
    | FN 
    | LET 
    | FOR
    | WHILE
    | IF
    | ELIF
    | ELSE
    (* special tokens *)
    | ILLEGAL
    | EOF
