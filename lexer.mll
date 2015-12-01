{
open Parser
exception Eof
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let symb = ['_' '@' '#']
let id = (letter | symb) (letter | symb | digit | '$')*
let inner = (letter | digit | white | symb)*
let string = (''' inner ''') | ('\"' inner '\"')
let frac = '.'digit*
let float = digit* frac


(* The final section of the lexer definition defines how to parse a character
   stream into a token stream.  Each of the rules below has the form
     | regexp { action }
   If the lexer sees the regular expression [regexp], it produces the token
   specified by the [action].  We won't go into details on how the actions
   work.  *)

rule read =
  parse
  | white                       { read lexbuf }
  | "("                         { LEFT_PAREN }
  | ")"                         { RIGHT_PAREN }
  | ";"                         { SEMICOLON }
  | ","                         { COMMA }
  | "SELECT" | "select"         { SELECT }
  | "FROM" | "from"             { FROM }
  | "WHERE" | "where"           { WHERE }
  | "INSERT" | "insert"         { INSERT }
  | "INTO" | "into"             { INTO }
  | "JOIN" | "join"             { JOIN }
  | "CREATE" | "create"         { CREATE }
  | "DROP" | "drop"             { DROP }
  | "DATABASE" | "database"     { DATABASE }
  | "TABLE" | "table"           { TABLE }
  | "SET" | "set"               { SET }
  | "DELETE" | "delete"         { DELETE }
  | "UPDATE" | "update"         { UPDATE }
  | "="                         { EQUAL }
  | ">"                         { GREATER }
  | "<"                         { LESS }
  | ">="                        { GREATER_EQUAL }
  | "<="                        { LESS_EQUAL }
  | "!="                        { NOT_EQ }
  | "%"                         { PERCENTAGE }
  | "'" | "\""                  { QUOTE }
  | "*"                         { ASTERISK }
  | "NOT" | "not"               { NT }
  | "LIKE" | "like"             { LK }
  | "ON" | "on"                 { ON }
  | "VALUES" | "values"         { VALUES }
  | "true"                      { TRUE }
  | "false"                     { FALSE }
  | id                          { ID (Lexing.lexeme lexbuf) }
  | string                      { STRING (Lexing.lexeme lexbuf) }
  | int                         { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float                       { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | eof                         { EOF }