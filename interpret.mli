(* interpret.mli *)
open Query
open Database

(* represent our lexed and parsed expressions *)
type expr =
  | Select of column list * string * where
  | Insert of string * column list * value list
  | Join of string * string * on
  | Update of string * (column, value) list
  | Delete of string * (column, value) list
  | Create of column_dec list * string
  | Drop of string

(* do a SQL evaluation *)
val eval : Database.t -> expr -> Query.t option