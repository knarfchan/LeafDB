(* filesystem.mli *)
open Csv
(* takes the name of a database and goes through the file directory, returning
 * Some Database.t if it exists physically and None if it does not exist
 *)
val read_db : string -> Database.t

(* takes the name of a table and goes through the file directory, returning Some
 * Some Table.t if it exists physically and None if it does not exist
 *)
val read_tbl : string -> Table.t

(* takes a Database.t and creates a new database directory *)
val add_db : Database.t -> unit

val add_tbl : Table.t -> unit

val delete_tbl : Table.t -> unit

(* takes a Table.t and stores it in file, updating the old file *)
val write_tbl : Table.t -> unit

(* takes the name of a database and removes it from file, returning true if
 * it succesfully deleted and false otherwise
 *)
val delete_db : string -> bool
