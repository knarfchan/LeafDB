open Interpret

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let sql = Parser.prog Lexer.read lexbuf in
  sql