(* filesystem.mli *)

(* precondition: "./DBMS" folder exists
 * postcondition: returns the names of the database folders in "./DBMS" as
 *                a string list *)
val get_database_names : unit -> string list

(* precondition: string is the name of a table that exists in the database
                 specified by the second parameter
 * postcondition: fills the Database.t with the tables in the database folder
 *)
val read_db : string -> Database.t -> unit

(* precondition: fst string is the name of a database that exists
 *               snd string is the name of a table that exists in the db
 * postcondition: return the Table.t type representing the csv file in our dir.
 *)
val read_tbl : string -> string -> Table.t

(* precondition: none
 * postcondition: creates a new directory with the string as the name *)
val add_db : string -> unit

(* precondition: first string is the name of a database that exists
 * postcondition: creates a csv with second string as name in the database
 *                folder and stores the data from Table.t into it *)
val add_tbl : string -> string -> Table.t -> unit

(* precondition: first string is the name of a database that exists
 * postcondition: creates an empty csv with the second string as the name in the
                  database folder *)
val add_empty_tbl : string -> string -> unit

(* precondition: first string is the name of a database that exists
 *               second string is the name of a table that exists in the db
 * postcondition: deletes the csv file with table name in the database folder *)
val delete_tbl : string -> string -> unit

(* precondition: first string is the name of a database that exists
 *               second string is the name of a table that exists in the db
 * postcondition: update the csv file with table name in the database folder
 *                with the new data from the Table.t *)
val write_tbl : string -> string -> Table.t -> unit

(* precondition: first string is the name of a database that exists
 * postcondition: deletes the database folder, including all files inside it
 *)
val delete_db : string -> unit