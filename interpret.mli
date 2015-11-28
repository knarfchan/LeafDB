(* interpret.mli *)

(* represent our lexed and parsed expressions *)
type expr =
| Select of column list * string * where
| SelectAll of string * where
| Insert of string * column list * value list
| JoinTables of string * string * on
| JoinTabQuer of string * expr * on
| JoinQuerTab of expr * string * on
| JoinQueries of expr * expr * on
| Update of string * (column, value) list
| Delete of string * (column, value) list
| CreateTable of string * column_dec list
| CreateDb of string
| DropTable of string
| DropDb of string

(* do a SQL evaluation *)
val eval : Database.t -> expr -> Query.t option