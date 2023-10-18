open Lexer
open Core.In_channel


let file = read_all "./file.mk"
let tokens = read_input file 0 (String.length file);;
pp tokens;;

Parser.print_stl (Parser.parse_program tokens)

