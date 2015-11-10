(* database.mli *)
open Table

(* type representing our database *)
type t

(* takes the name of the database and returns the database *)
val use: string -> database

(* adds a table to a database and return the database *)
val add: Table.t -> database -> database

(* takes a table, removes it from the database, and return the database *)
val drop: table -> database -> database

