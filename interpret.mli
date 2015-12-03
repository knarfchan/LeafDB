(* interpret.mli *)
open Typs
open Ast

type evaluated = Table.t option * bool

type dbresult = Database.t option * string option * bool

(* do a SQL evaluation *)
val eval : Database.t -> expr -> evaluated

val eval_dbms : Dbms.t -> expr -> dbresult