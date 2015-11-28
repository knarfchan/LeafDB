(* table.ml *)
open Types
open Maps

type t = (string * Maps.t) list

let lookup col tbl = failwith "unimplemented"

let select = failwith "unimplemented"

let rec insert tbl clst vlst =
  match tbl, clst, vlst with
  | (name, map)::tl, a::b, a'::b' -> (name, Maps.insert a a' map)::(insert tl b b')
  | [], _, _ -> []
  | _, [], _ -> []
  | _, _, [] -> []

let insertAll = failwith "unimplemented"

let update = failwith "unimplemented"

let updateAll = failwith "unimplemented"

let delete = failwith "unimplemented"

let union = failwith "unimplemented"

let join = failwith "unimplemented"
