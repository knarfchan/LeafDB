(* table.ml *)
open Core.Date

(* represent our table columns as a string *)
type column  = string

(* a match between columns for relating tables *)
type on = column * column

(* data types our database support *)
type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VFloat of float
  | VDate of int

(* supported operators in where used to conditionally select rows *)
type operator =
  | Gt | Lt | Eq | GtEq | LtEq | NotEq
  | LikeBegin | LikeEnd | LikeSubstring
  | NotLikeBegin | NotLikeEnd | NotLikeSubstring

(* SQL where expression: operate on the columns with the given value if there
 * is a condition, or Null if there is no condition
 *)
type where    =
  | Condition of (column * operator * value)
  | Null

(* declaration of a column with its associated value *)
type column_dec = column * value

type 'a node = {mutable prev: 'a node option; mutable next: 'a node option;
                value: 'a}

type 'a dlist = {mutable first: 'a node option; mutable last: 'a node option}

type t = (string * value Map.t) list

let lookup = failwith "unimplemented"

let select = failwith "unimplemented"

let insert = failwith "unimplemented"

let insertAll = failwith "unimplemented"

let update = failwith "unimplemented"

let updateAll = failwith "unimplemented"

let delete = failwith "unimplemented"

let union = failwith "unimplemented"

let join = failwith "unimplemented"