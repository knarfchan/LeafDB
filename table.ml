(* table.ml *)
open Typs
open Maps

type t = (column * Maps.t) list

let next_val =
 let counter = ref 0 in fun () ->
   incr counter;
   !counter

let lookup col tbl = failwith "unimplemented"

(* precondition: col must be a column in Table tbl
 * postcondition: [make_select] will return an empty map with the the same type of the map of col*)
let make_select tbl col =
  if List.mem_assoc col tbl then Maps.empty (List.assoc col tbl)
  else failwith "Column not found in table"

(* precondition:
 * postcondition: *)
let rec select_col tbl rows col acc =
  match rows with
  | [] -> acc
  | h::t -> if List.mem_assoc col tbl then
              let map = List.assoc col tbl in
              if Maps.is_member h map then
                select_col tbl t col (Maps.insert (Maps.lookup h map) h acc)
              else select_col tbl t col acc
            else failwith "Column is not found in table"

(* precondition:
 * postcondition: *)
let rec all_col tbl clst rows acc =
  match clst with
  | [] -> acc
  | h::t -> all_col tbl t rows (acc @ [(h, (select_col tbl rows h (make_select tbl h)))])

(* precondition:
 * postcondition: strip away cols*)
let rec strip_tbl tbl acc : Maps.t list =
  match tbl with
  | [] -> acc
  | (_,b)::t -> (strip_tbl t (acc @ [b]))

let get_size (tbl:t) : int = match tbl with
  | (a,b)::t -> Maps.size (Maps.get_longest (strip_tbl tbl []) 0 (List.assoc a tbl))
  | [] -> 0

let get_diff (first:t) (second:t) : int =
  let old_length = (get_size first) in
  let new_length = (get_size second) in
    (new_length - old_length)

(* precondition:
 * postcondition: *)
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

(* precondition:
 * postcondition: *)
let rec strip_col (tbl:t) (acc:column list) :column list =
  match tbl with
  | [] -> acc
  | (a,_)::t -> (strip_col t (acc @ [a]))

(* precondition:
 * postcondition: *)
let selectAll tbl w =
  select (strip_col tbl []) tbl w

(* precondition:
 * postcondition: *)
let rec get_cvlst (clst: column list) (vlst: value list) (acc: (column * value) list) =
  match clst, vlst with
  | [],[] -> acc
  | h::t, h'::t' -> get_cvlst t t' (acc @ [(h,h')])
  | _, _ -> failwith "Column list and value list should be the same length"

(* precondition:
 * postcondition: *)
let rec insert_help (tbl:t) (cvlst: (column * value) list) (rowid) (acc) =
    match tbl with
    | [] -> acc
    | (name, map)::tl -> if List.mem_assoc name cvlst then
                            insert_help tl cvlst rowid
                            (acc @ [(name, Maps.insert (List.assoc name cvlst) rowid map)])
                         else insert_help tbl cvlst rowid (acc @ [name, map])

(* precondition:
 * postcondition: *)
let rec insertAll (tbl:t) (vlst: value list) rowid acc =
  match tbl, vlst with
    | (name, map)::tl, a::b ->
        (insertAll tl b rowid (acc @ [(name, Maps.insert a rowid map)]))
    | _, _ -> acc

(* precondition:
 * postcondition: *)
let rec insert (tbl:t) (clst:column list) (vlst: value list) =
  let cvlst = (get_cvlst clst vlst []) in
  let rowid = next_val () in
    if List.length tbl = List.length clst then (insertAll tbl vlst rowid [])
    else (insert_help tbl cvlst rowid [])

(* precondition:
 * postcondition: *)
let rec get_col cvlst acc =
  match cvlst with
  | [] -> acc
  | (c,v)::t -> get_col t (acc @ [c])

(* precondition:
 * postcondition: *)
let rec update_help new_tbl cvlst acc =
  match new_tbl, cvlst with
  | [], [] -> acc
  | (a,b)::t, (c,v)::t' -> update_help t t' (acc @ [(a, Maps.update b v)])
  | _, _ -> failwith "tbl and column value lists should not be of different size"

let rec update_all_col tbl new_tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> update_all_col t new_tbl
                      (acc @ [(name,
                      (if List.mem_assoc name new_tbl
                        then Maps.replace (List.assoc name new_tbl) map
                       else map))])

(* precondition:
 * postcondition: *)
let update tbl cvlst w =
  let new_tbl = select (get_col cvlst []) tbl w in
  let updated_tbl = update_help new_tbl cvlst [] in
    (update_all_col tbl updated_tbl [])

(* precondition:
 * postcondition: *)
let updateAll = failwith "unimplemented" (*I don't think this needs to be implemented*)

(* precondition:
 * postcondition: *)
let rec delete table where = match table, where with
  | ((name,map)::t), (Condition (col,op,v)) -> (if (name = col) then (name, (Maps.delete map op v))::t
    else delete t where)
  | _ , Null -> []
  | _ -> failwith "Error"

(* precondition:
 * postcondition: *)
let union = failwith "unimplemented"

(* precondition:
 * postcondition: *)
let join = failwith "unimplemented"

let rec create_help cdl acc =
  match cdl with
  | [] -> acc
  | (col, v)::t -> create_help t ((col, Maps.create v)::acc)

(* [create cdl] creates an empty table
 * precondition:
 * postcondition:  *)
let rec create (cdl : column_dec list) =
  create_help cdl []
