(* table.mli *)
open Query

(* represents our table *)
type t

(* a match between columns for relating tables *)
type on = column * column

(* Takes a list of columns, a table, and a condition and returns a query given
 * that the columns listed are in the table and the condition is valid on the
 * values of the columns
 *)
val select    : column list -> table -> where -> table

(* Takes a table, a list of columns, a list of values that correspond
 * respectively with the data types of the columns, and return a table with the
 * values appended to the columns
 *)
val insert    : t -> column list -> value list -> t

(* Takes a table, and a list which has a length equal to the number of
 * columns and which values correspond to the data types of the columns in order
 * of the columns, and returns a table with the values appended to the columns
 *)
val insertAll : t -> value list -> t

(* Takes a table, an updated list of (column * value) pairs, and a where
 * condition and returns an updated table for all records in which the condition
 * holds true
 *)
val update    : t -> (column, value) list -> where -> t

(* Takes a table and an updated list of (column * value) pairs and returns a
 * table with all of the records updated
 *)
val updateAll : t -> (column, value) list -> t

(* Takes a table, a list of (column * value) pairs, and returns *)
val delete    : t -> (column, value) list -> t

(* inner join
 * Takes all rows from both tables where there is a match between columns
 * and joins them in a new table
 *)
val join      : t -> t -> on -> t

(* convert a query into a table *)
val convert   : Query.t -> t