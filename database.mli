(* database.mli *)
open Typs

(* type representing our database *)
type t

(* create a table in a database and return the database *)
val create: unit -> t

(* takes a table, removes it from the database, and return the database *)
val drop: t -> string -> bool

(* finds a table in the database based on its name *)
val lookup: t -> string -> Table.t option

val add_table: t -> string -> Table.t -> bool

val update_table: t -> string -> Table.t -> unit

val get_tables : t -> Table.t