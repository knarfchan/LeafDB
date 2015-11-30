(* table.ml *)
open Typs
open Maps

type t = (column * Maps.t) list

let next_val =
 let counter = ref 0 in fun () ->
   incr counter;
   !counter

let lookup col tbl = failwith "unimplemented"

let select = failwith "unimplemented"

let rec get_cvlst (clst: column list) (vlst: value list) (acc: (column * value) list) =
  match clst, vlst with
  | [],[] -> acc
  | h::t, h'::t' -> get_cvlst t t' (acc @ [(h,h')])

let rec insert_helper (tbl:t) clst (acc: (column * value) list)=
  match tbl with
  | [] -> acc
  | (col, map)::tl -> if List.mem_assoc col clst then insert_helper tl clst acc
              else insert_helper tl clst (acc @ [col, ()])

let rec insert (tbl:t) (clst:column list) (vlst: value list) =
  let rowid = next_val () in


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
