open Lexer

let input = 
    "let ten = 10;
    let five = 5;

    let add = fn(x, y) {
        x + y;
    }
    if (x != 9) { x}
    else if (x == 9)

    let result = add(five, 10);
    "
let tokens = read_input input 0 (String.length input);;
pp tokens;;

