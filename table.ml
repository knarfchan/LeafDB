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
let insert (tbl:t) (clst:column list) (vlst: value list) : t =
  let cvlst = (get_cvlst clst vlst []) in
  let rowid = next_val () in
    insert_help tbl cvlst rowid []

(* precondition:
 * postcondition: *)
let rec insertAll_help (tbl:t) (vlst: value list) rowid acc =
  match tbl, vlst with
    | (name, map)::tl, a::b ->
        (insertAll_help tl b rowid (acc @ [(name, Maps.insert a rowid map)]))
    | _, _ -> acc

(* precondition:
 * postcondition: *)
let insertAll (tbl:t) (vlst: value list) : t =
  let rowid = next_val () in
    insertAll_help tbl vlst rowid []

(* precondition:
 * postcondition: *)
let rec get_col cvlst acc =
  match cvlst with
  | [] -> acc
  | (c,_)::t -> get_col t (acc @ [c])

let rec get_val_from_cvlst cvlst acc =
  match cvlst with
  | [] -> acc
  | (_,v)::t -> get_val_from_cvlst t (acc @ [v])

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
let rec delete table where = match table, where with
  | ((name,map)::t), (Condition (col,op,v)) -> (if (name = col) then (name, (Maps.delete map op v))::t
    else delete t where)
  | _ , Null -> []
  | _ -> failwith "Error"



let rec create_help cdl acc =
  match cdl with
  | [] -> acc
  | (col, v)::t -> create_help t ((col, Maps.create v)::acc)

(* [create cdl] creates an empty table
 * precondition:
 * postcondition:  *)
let rec create (cdl : column_dec list) =
  create_help cdl []

let rec get_row row tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if Maps.is_member row map then
                        get_row row t (acc @ [(Maps.lookup row map)])
                      else get_row row t (acc @ [VNull])

let rec get_all_rows rows tbl acc =
  match rows with
  | [] -> acc
  | h::t -> get_all_rows t tbl acc @ [(get_row h tbl [])]

let rec tbl_to_matrix (tbl : t) acc =
  match tbl with
  | [] -> [[]]
  | (a,b)::t -> let new_map = Maps.get_longest (strip_tbl tbl []) 0 (List.assoc a tbl) in
              let rows = Maps.get_rows new_map in
                get_all_rows rows tbl acc

let convert_matrix (tbl:t) =
  tbl_to_matrix tbl []


let rec row_val (r:value list) acc =
  match r with
  | [] -> acc
  | VInt x::t -> row_val t (acc @ [string_of_int x])
  | VString x::t -> row_val t (acc @ [x])
  | VBool x::t -> row_val t (acc @ [string_of_bool x])
  | VFloat x::t -> row_val t (acc @ [string_of_float x])
  | VNull::t -> row_val t (acc @ ["NULL"])

let rec tbl_val (m: value list list) acc: string list list=
  match m with
  | [] -> acc
  | h::t -> tbl_val t (acc @ [row_val h []])

let rec row_to_array (m: column list list) acc =
  match m with
  | [] -> acc
  | h::t -> row_to_array t (acc @ [Array.of_list h])

let row_size tbl =
  let size = Array.make (Array.length tbl.(0)) 0 in
    (for i = 0 to (Array.length tbl) - 1 do
      for j = 0 to (Array.length tbl.(0)) -1 do
        if Bytes.length tbl.(i).(j) > size.(j) then
          size.(j) <- (Bytes.length tbl.(i).(j) + 1)
        else ()
      done
    done);
  size

(* precondition: the number of columns of tbl and size must be the same
 * postcondition: format_array returns a string matrix where all values are the same length*)
let format_array tbl size =
  (for i = 0 to (Array.length tbl) - 1 do
    for j = 0 to (Array.length tbl.(0)) -1 do
      tbl.(i).(j) <- (Bytes.make (size.(j) - (Bytes.length tbl.(i).(j))) ' ') ^ (tbl.(i).(j))
    done
  done); tbl

let get_bars (size: int array) length:int=
  (for i = 0 to (Array.length size) - 1 do
    length := !length + size.(i)
  done); length := !length + ((Array.length size) * 3) + 1; !length

let print_tbl_helper (tbl:string array array) =
  let total_size = row_size tbl in
  let bar = get_bars total_size (ref 0) in
  let matrix = format_array tbl (total_size) in
    (Printf.printf "%s\n" (" " ^ (Bytes.make bar '-')));
    (for i = 0 to (Array.length matrix) - 1 do
      (for j = 0 to (Array.length matrix.(0)) -1 do
        if i = 0 then Printf.printf "%s" (" | " ^ tbl.(i).(j))
        else Printf.printf "%s" (" | " ^ tbl.(i).(j))
      done);
        if i = 0 then Printf.printf "%s\n" (" |\n" ^ " " ^ (Bytes.make bar '-'))
        else (Printf.printf "%s\n" " |")
    done); (Printf.printf "%s\n" (" " ^ (Bytes.make bar '-')))

let print_tbl (tbl:t) =
  print_tbl_helper (Array.of_list (row_to_array ((strip_col tbl []) ::
  (tbl_val (convert_matrix tbl) [])) []))

let rec get_vals (tbl:t) row acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if Maps.is_member row map then
                        get_vals t row (acc @ [(name, Maps.lookup row map)])
                      else get_vals t row acc

let rec get_cvlst (t1:t) (t2:t) rows acc =
  match rows with
  | [] -> acc
  | (r1, r2)::t -> get_cvlst t1 t2 t (acc @ [(get_vals t1 r1 []) @ (get_vals t2 r2 [])])

let rec empty_table tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> (empty_table t (acc @ [(name, Maps.empty map)]))



let rec join_help tbl cvlst =
  match cvlst with
  | [] -> tbl
  | lst::t -> join_help (insert tbl (get_col lst []) (get_val_from_cvlst lst [])) t

(* precondition:
 * postcondition: *)
let join t1 t2 o =
  let rows = (match o with
             | (c1, c2) -> if List.mem_assoc c1 t1 && List.mem_assoc c2 t2 then
                             Maps.join (List.assoc c1 t1) (List.assoc c2 t2)
                           else failwith "Columns are not found in tables") in
  join_help ((empty_table t1 []) @ (empty_table t2 [])) (get_cvlst t1 t2 rows [])

(* precondition:
 * postcondition: *)
let union = failwith "unimplemented"
