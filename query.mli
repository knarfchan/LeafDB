(* query.mli *)

open Core.Date

(* data types our database support *)
type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VFloat of float
  | VDate of Core.Date.t

(* supported operators in where used to conditionally select rows *)
type operator =
  | Gt | Lt | Eq | GtEq | LtEq | NotEq
  | Like

(* represent our table columns as a string *)
type column  = string

(* SQL where expression: operate on the columns with the given value if there
 * is a condition, or Null if there is no condition
 *)
type where    =
  | Condition of (column * operator * value)
  | Null

(* declaration of a column with its associated value *)
type column_dec = column * value

(* type representing our query *)
type t

(* [precondition] : the two queries have the same number of columns
 * Takes two queries with the same number of columns and corresponding data
 * types. and appends one onto the other in a new query *)
val union     : t -> t -> t

val convert   : Table.t -> t

val is_empty  : t -> bool

