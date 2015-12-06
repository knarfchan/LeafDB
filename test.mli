(* test.mli *)
open Ast

(* [parse s] parses the string into an expr and returns it if the string is
 * valid, and throws an exception otherwise *)
val parse: string -> expr

(* [ast_to_string e] takes an expression and returns the variant as a string
 * useful for testing and debugging purposes *)
val ast_to_string: expr -> string