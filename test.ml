open Ast
open Typs

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let sql = Parser.prog Lexer.read lexbuf in
  sql

let rec l2str lst acc =
  match lst with
  | [] -> acc^"]"
  | h::[] -> acc^h^"]"
  | h::t -> l2str(t)(acc^h^"; ")

let list_to_string lst =
  l2str lst "["

let op_to_string op =
  match op with
  | Gt -> "Gt"
  | Lt -> "Lt"
  | Eq -> "Eq"
  | GtEq -> "GtEq"
  | LtEq -> "LtEq"
  | NotEq -> "NotEq"
  | LikeBegin -> "LikeBegin"
  | LikeEnd -> "LikeEnd"
  | LikeSubstring -> "LikeSubstring"
  | NotLikeBegin -> "NotLikeBegin"
  | NotLikeEnd -> "NotLikeEnd"
  | NotLikeSubstring -> "NotLikeSubstring"

let val_to_string v =
  match v with
  | VInt x -> "VInt(" ^ string_of_int(x) ^ ")"
  | VString s -> "VString(" ^ s ^ ")"
  | VBool b -> "VBool(" ^ string_of_bool(b) ^ ")"
  | VFloat f -> "VFloat(" ^ string_of_float(f) ^ ")"

let rec vl2str vl acc =
  match vl with
  | [] -> acc^"]"
  | h::[] -> acc^val_to_string(h)^"]"
  | h::t -> vl2str(t)(acc^val_to_string(h)^"; ")

let vlist_to_string vl =
  vl2str vl "["

let rec cvl2str cvl acc =
  match cvl with
  | [] -> acc^"]"
  | (c,v)::[] -> acc ^ "(" ^ c ^ ", " ^ val_to_string(v) ^ ")" ^ "]"
  | (c,v)::t -> cvl2str(t)(acc ^ "(" ^ c ^ ", " ^ val_to_string(v) ^ ")" ^ "; ")

let cvlist_to_string cvl =
  cvl2str cvl "["

let where_to_string w =
  match w with
  | Condition(col, op, v) -> "(" ^col^ op_to_string(op) ^ val_to_string(v) ^ ")"
  | Null -> "Null"

let rec ast_to_string ast =
  match ast with
  | Select(cl, s, w) -> "Select(" ^ list_to_string(cl) ^ ", " ^ s ^
                         ", " ^ where_to_string(w) ^ ")"
  | SelectAll(s, w) -> "SelectAll(" ^ s ^ ", " ^ where_to_string(w) ^ ")"
  | Insert(s, cl, vl) -> "Insert(" ^ s ^ list_to_string(cl) ^ ", " ^
                            vlist_to_string(vl) ^ ")"
  | InsertAll(s, vl) -> "InsertAll(" ^ s ^ ", " ^ vlist_to_string(vl) ^ ")"
  | JoinTables(s1, s2, (on1, on2)) -> "JoinTables(" ^ s1 ^ ", " ^ s2 ^ ", " ^
                                        "(" ^ on1 ^ ", " ^ on2 ^ "))"
  | JoinTabQuer(s1, e, (on1, on2)) -> "JoinTabQuer(" ^ s1 ^ ", " ^
                                        ast_to_string(e) ^ ", " ^ "(" ^ on1 ^
                                        ", " ^ on2 ^ "))"
  | JoinQuerTab(e, s1, (on1, on2)) -> "JoinQuerTab(" ^ ast_to_string(e) ^ ", " ^
                                       s1 ^ ", " ^ "(" ^ on1 ^ ", " ^ on2 ^ "))"
  | JoinQueries(e1, e2, (on1, on2)) -> "JoinQueries(" ^ ast_to_string(e1) ^ ", "
                                        ^ ast_to_string(e2) ^ ", " ^ "(" ^ on1 ^
                                        ", " ^ on2 ^ "))"
  | Update(s, cvl, w) -> "Update(" ^ s ^ ", " ^ cvlist_to_string(cvl) ^ ", " ^
                          where_to_string(w) ^ ")"
  | Delete(s, w) -> "Delete(" ^ s ^ ", " ^ where_to_string(w) ^ ")"
  | CreateTable(s, cvl) -> "CreateTable(" ^s^ ", " ^ cvlist_to_string(cvl) ^ ")"
  | CreateDb(s) -> "CreateDb(" ^ s ^ ")"
  | DropTable(s) -> "DropTable(" ^ s ^ ")"
  | DropDb(s) -> "DropDb(" ^ s ^ ")"
