(* table.ml *)
open Types
open Maps

type t = (string * Maps.t) list

let next_val =
 let counter = ref 0 in fun () ->
   incr counter;
   !counter

let lookup col tbl = failwith "unimplemented"

let select = failwith "unimplemented"

let rec insert (tbl:t) (clst:column list) (vlst: value list) =
  let rowid = next_val () in
  match tbl, clst, vlst with
    | (name, map)::tl, a::b, a'::b' -> if name = a then (name, Maps.insert a' rowid map):: (insertAll tl b b')

let rec insertAll (tbl:t) (vlst: value list) =
  let rowid = next_val () in
  match tbl, vlst with
    | (name, map)::tl, a::b ->
        (name, Maps.insert a rowid map)::(insertAll tl b)
    | _, _ -> tbl

let update = failwith "unimplemented"

let updateAll = failwith "unimplemented"

let rec delete table where = match table, where with
  | ((name,map)::t), (Condition (col,op,v)) -> (if (name = col) then (name, (Maps.delete map op v))::t
    else delete t where)
  | _ , Null -> []
  | _ -> failwith "Error"

let union = failwith "unimplemented"

let join = failwith "unimplemented"
