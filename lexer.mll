{
open Parser
exception Eof
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let string = '''(letter | digit | white)*'''
let frac = '.'digit*
let float = digit* frac?


(* The final section of the lexer definition defines how to parse a character
   stream into a token stream.  Each of the rules below has the form
     | regexp { action }
   If the lexer sees the regular expression [regexp], it produces the token
   specified by the [action].  We won't go into details on how the actions
   work.  *)

rule read =
  parse
  | white             { read lexbuf }
  | "("               { LEFT_PAREN }
  | ")"               { RIGHT_PAREN }
  | ";"               { SEMICOLON }
  | ","               { COMMA }
  | "SELECT"          { SELECT }
  | "FROM"            { FROM }
  | "WHERE"           { WHERE }
  | "INSERT INTO"     { INSERT }
  | "JOIN"            { JOIN }
  | "CREATE TABLE"    { CREATETABLE }
  | "CREATE DATABASE" { CREATEDB }
  | "DROP TABLE"      { DROPTABLE }
  | "DROP DATABASE"   { DROPDB }
  | "SET"             { SET }
  | "DELETE FROM"     { DELETE }
  | "UPDATE"          { UPDATE }
  | "="               { EQUAL }
  | ">"               { GREATER }
  | "<"               { LESS }
  | ">="              { GREATER_EQUAL }
  | "<="              { LESS_EQUAL }
  | "!="              { NOT_EQ }
  | "%"               { PERCENTAGE }
  | "'" | "\""        { QUOTE }
  | "*"               { ASTERISK }
  | "LIKE"            { LIKE_REGEX }
  | "NOT LIKE"        { NOT_LIKE }
  | "ON"              { ON }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | id                { ID (Lexing.lexeme lexbuf) }
  | string            { STRING (Lexing.lexeme lexbuf) }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | eof               { EOF }