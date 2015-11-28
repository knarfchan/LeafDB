(* database.mli *)

(* type representing our database *)
type t

(* takes the name of the database and returns the database *)
val use: string -> t

(* create a table in a database and return the database *)
val create: column_dec list -> string -> t

(* takes a table, removes it from the database, and return the database *)
val drop: Table.t -> t -> t

(* finds a table in the database based on its name *)
val lookup: string -> Table.t