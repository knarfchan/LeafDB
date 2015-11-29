(* table.ml *)
open Types

type t = (string * Maps.t) list

let lookup = failwith "unimplemented"

let select = failwith "unimplemented"

let insert = failwith "unimplemented"

let insertAll = failwith "unimplemented"

let update = failwith "unimplemented"

let updateAll = failwith "unimplemented"

let rec delete table where = match table, where with
  | ((name,map)::t), (Condition (col,op,v)) -> (if (name = col) then (name, (Maps.delete map op v))::t
    else delete t where)
  | _ , Null -> []
  | _ -> failwith "Error"

let union = failwith "unimplemented"

let join = failwith "unimplemented"
