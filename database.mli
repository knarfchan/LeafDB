(* database.mli *)
open Typs

(* type representing our database *)
type t

(* [create ()] create empty database and return the database
 * precondition: none
 * postcondition: return empty database *)
val create: unit -> t

(* [drop db str] drops a table with name str from db
 * precondition : none
 * postcondition: drop table of name str if it exists in db, return true
 *                otherwise return false *)
val drop: t -> string -> bool

(* finds a table in the database based on its name *)
val lookup: t -> string -> Table.t option

(* [add_table db str tbl] adds a table to a specified database
 * precondition  : none
 * postcondition : table is not in the specified database *)
val add_table: t -> string -> Table.t -> bool

(* [update_table db str tbl] updates a table in a specified database
 * precondition: none
 * postcondition: replaces the table in the database with the give tbl *)
val update_table: t -> string -> Table.t -> unit

(* [get_tables db tbl] returns a table with one column consisting of strings
 * of all the tables in the database *)
val get_tables : t -> Table.t