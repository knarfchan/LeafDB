open Typs

(* type representing our database management system *)
type t

(* precondition: none
 * postcondition: create an empty Dbms t to store databases *)
val create: t

(* precondition: none
 * postcondition: add an empty Database.t with name string to a t if the name is
 *                not already in the t and return true. If the name is already
 *                taken return false
 *)
val add_database: t -> string -> bool

(* precondition: none
 * postcondition: return Some x when the database of name string exists, where
 *                          x is the Database.t associated with the name
 *                       None otherwise *)
val use: t -> string -> Database.t option

(* precondition: none
 * postcondition: return true after dropping database when there is a database
 *                  with name string in t
 *                return false if there is no database in t with name string
 *)
val drop: t -> string -> bool

(* precondition: none
 * postcondition: return a Table.t with one column of strings of all the
 *                databases that exist in this t*)
val get_databases: t -> Table.t

(* precondition: none
 * postcondition: return a t with empty databases with names from string list*)
val load_databases: string list -> t