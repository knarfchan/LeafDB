(* filesystem.mli *)
open Csv
(* *)
val get_database_names : unit -> string list

(* takes the name of a database and a Database.t and fills it with the tables
 * found in the file directory
 *)
val read_db : string -> Database.t -> unit

(* takes the name of a database then  table and goes through the file directory,
 * returning Some Table.t if it exists physically and None if it does not exist
 *)
val read_tbl : string -> string -> Table.t

(* takes a database name and creates a new directory *)
val add_db : string -> unit

(* takes a database and table name and a Table.t and creates the csv *)
val add_tbl : string -> string -> Table.t -> unit

(* takes a database and table name and delete it *)
val delete_tbl : string -> string -> unit

(* takes a Table.t and stores it in file, updating the old file *)
val write_tbl : string -> string -> Table.t -> unit

(* takes the name of a database and removes it from file, returning true if
 * it succesfully deleted and false otherwise
 *)
val delete_db : string -> unit