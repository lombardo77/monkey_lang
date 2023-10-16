type token = 
    (* non numberic/alphabetical tokens *)
    | LPAREN
    | RPAREN
    | ASSIGN
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
    | RETURN 
    (* logical operators *)
    | TRUE
    | FALSE
    | NOTEQ
    | NOT
    | EQ
    (* special tokens *)
    | ILLEGAL
    | EOF
