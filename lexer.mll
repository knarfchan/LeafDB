{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let string = [letter digit ' ']*
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
  | "INSERT"          { INSERT }
  | "JOIN"            { JOIN }
  | "CREATE TABLE"    { CREATETABLE }
  | "CREATE DATABASE" { CREATEDB }
  | "DROP TABLE"      { DROPTABLE }
  | "DROP DATABASE"   { DROPDB }
  | "SET"             { SET }
  | "DELETE"          { DELETE }
  | "UPDATE"          { UPDATE }
  | "="               { EQUALS }
  | ">"               { GREATER }
  | "<"               { LESS }
  | ">="              { GREATER_EQUAL }
  | "<="              { LESS_EQUAL }
  | "!="              { NOT_EQUAL }
  | "%"               { PERCENTAGE }
  | "'"               { QUOTE }
  | "\""              { QUOTE }
  | "*"               { ASTERISK }
  | "LIKE"            { LIKE_REGEX }
  | "NOT LIKE"        { NOT_LIKE }
  | "in"              { IN }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | id                { ID (Lexing.lexeme lexbuf) }
  | string            { STRING (Lexing.lexeme lexbuf) }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | eof               { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
