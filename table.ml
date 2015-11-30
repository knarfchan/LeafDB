(* table.ml *)
open Types
open Maps

type t = (column * Maps.t) list

let next_val =
 let counter = ref 0 in fun () ->
   incr counter;
   !counter

let lookup col tbl = failwith "unimplemented" (*Idk if this is necessary*)

let rec find_col col clst =
  match clst with
  | [] -> false
  | hd::tl -> if col = hd then true else find_col col tl

(*
let select_help clst tbl w acc =
  type where    =
  | Condition of (column * operator * value)
  | Null

  match tbl, w with
  | (name, map)::t, Condition () -> Maps.select w*)

let select clst tbl w = failwith "Unimplemented"


let rec get_cvlst (clst: column list) (vlst: value list) (acc: (column * value) list) =
  match clst, vlst with
  | [],[] -> acc
  | h::t, h'::t' -> get_cvlst t t' (acc @ [(h,h')])
  | _, _ -> failwith "Column list and value list should be the same length"

(*
let rec insert_helper (tbl:t) (cvlst: (column * value) list) (acc: (column * unit) list)=
  match tbl with
  | [] -> acc
  | (col, map)::tl -> if List.mem_assoc col cvlst then insert_helper tl cvlst acc
              else insert_helper tl cvlst (acc @ [col, ()])*)

let rec insert_help (tbl:t) (cvlst: (column * value) list) (rowid) (acc) =
    match tbl with
    | [] -> acc
    | (name, map)::tl -> if List.mem_assoc name cvlst then
                            insert_help tl cvlst rowid
                            (acc @ [(name, Maps.insert (List.assoc name cvlst) rowid map)])
                         else insert_help tbl cvlst rowid (acc @ [name, map])


let rec insertAll (tbl:t) (vlst: value list) rowid acc =
  match tbl, vlst with
    | (name, map)::tl, a::b ->
        (insertAll tl b rowid (acc @ [(name, Maps.insert a rowid map)]))
    | _, _ -> acc

let rec insert (tbl:t) (clst:column list) (vlst: value list) =
  let cvlst = (get_cvlst clst vlst []) in
  let rowid = next_val () in
    if List.length tbl = List.length clst then (insertAll tbl vlst rowid [])
    else (insert_help tbl cvlst rowid [])

let update = failwith "unimplemented"

let updateAll = failwith "unimplemented"

let rec delete table where = match table, where with
  | ((name,map)::t), (Condition (col,op,v)) -> (if (name = col) then (name, (Maps.delete map op v))::t
    else delete t where)
  | _ , Null -> []
  | _ -> failwith "Error"

let union = failwith "unimplemented"

let join = failwith "unimplemented"
