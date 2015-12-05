(* table.mli *)
open Typs
open Maps

(* t represents a table *)
type t

(* precondition: None
 * poscondition : turns a (string * Map.t) list into a table *)
val to_table : (string * Maps.t) list -> t

(* precondition: Column must be a column in t
 * poscondition : returns the map in table t with the column name parameter *)
val get_one_map : column -> t -> Maps.t

(* precondition : None
 * postcondition : Returns the size of a table t *)
val get_size : t -> int

(* precondition : None
 * postcondition : Returns the difference in size between two tables *)
val get_diff  : t -> t -> int

(* precondition : Columns in column list must be in table and column in where
 *                condition must be in table
 * postcondition : Returns a table containing the columns of the original table
 *                 that meet the where condition *)
val select    : column list -> t -> where -> t

(* precondition : Column in where condition must be in table
 * postcondition : Returns a table containing the all the columns from the
 *                 original table that meet the where condition *)
val selectAll : t -> where -> t

(* precondition : Columns in column list must be in table, size of column list
 *                and value list must be the same length
 * postcondition : Inserts an item described in the column and value list
 *                 into table t *)
val insert    : t -> column list -> value list -> t

(* precondition : None
 * postcondition : Converts the table to a string list list *)
val matrix_of_table : t -> string list list

(* precondition : Value list must be the same size as the number of columns in
 *                table
 * postcondition : Inserts an item described in the column and value list
 *                 into table t *)
val insertAll : t -> value list -> t

(* precondition : the columns in the column value list and the where condition
 *                must be in table, the values must be of the correct type for
 *                the column
 * postcondition : Updates the table to new values specified in the column
 *                 value list on the values that satisfy the where condition *)
val update    : t -> (column * value) list -> where -> t

(* precondition : Column in where condition must be in table
 * postcondition : Returns a table with the values that satisfies the where
 *                 condition removed *)
val delete    : t -> where -> t

(* precondition : Column in on condition in the first and second table
 * postcondition : Returns a table all the columns from both tables that have
 *                 common values in the columns in the on condition *)
val join      : t -> t -> on -> t

(* precondition : Names in the column_dec list must be unique
 * postcondition : creates a table with the columns in the column_dec list *)
val create    : column_dec list -> t

(* precondition : None
 * postcondition : Prints the table in a matrix form *)
val print_tbl : t -> unit

(* precondition : None
 * postcondition : Converts a bytes list list into a table *)
val read_tbl : bytes list list -> t