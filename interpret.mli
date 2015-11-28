(* interpret.mli *)
open Types
open Ast

(* do a SQL evaluation *)
val eval : Database.t -> expr -> Query.t option