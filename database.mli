(* database.mli *)
open Table

(* type representing our database *)
type t

(* takes the name of the database and returns the database *)
val use: string -> t

(* adds a table to a database and return the database *)
val add: Table.t -> t -> t

(* takes a table, removes it from the database, and return the database *)
val drop: Table.t -> t -> t

(* finds a table in the database based on its name *)
val lookup: string -> Table.t