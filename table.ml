(* table.ml *)
open Typs
open Maps

type t = (column * Maps.t) list

let next_val =
 let counter = ref 0 in fun () ->
   incr counter;
   !counter

let lookup col tbl = failwith "unimplemented" (*Idk if this is necessary*)
(*
let rec find_col col clst =
  match clst with
  | [] -> false
  | hd::tl -> if col = hd then true else find_col col tl*)

let make_select tbl col =
  if List.mem_assoc col tbl then Maps.empty (List.assoc col tbl)
  else failwith "Column not found in table"

let rec select_col tbl rows col acc =
  match rows with
  | [] -> acc
  | h::t -> if List.mem_assoc col tbl then
              let map = List.assoc col tbl in
              if Maps.is_member h map then
                select_col tbl t col (Maps.insert (Maps.lookup h map) h acc)
              else select_col tbl t col acc
            else failwith "Column is not found in table"

let rec all_col tbl clst rows acc =
  match clst with
  | [] -> acc
  | h::t -> all_col tbl t rows (acc @ [(h, (select_col tbl rows h (make_select tbl h)))])

let rec strip_tbl tbl acc =
  match tbl with
  | [] -> acc
  | (_,b)::t -> (strip_tbl t (acc @ [b]))

let select clst tbl w =
  let map =
    (match w with
    | Condition (col,op,v) -> if List.mem_assoc col tbl then Maps.select (List.assoc col tbl) op v
                              else failwith "Column is not found in table"
    | Null -> (match clst with
              | [] -> failwith "No columns chosen for select"
              | h::t -> Maps.get_longest (strip_tbl tbl []) 0 (List.assoc h tbl))) in
  let rows = Maps.get_rows map in
    (all_col tbl clst rows [])

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
