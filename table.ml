(* table.ml *)
open Typs
open Maps
open Assertions

type t = (column * Maps.t) list

(* [next_val ()] is a mutable counter used to assign unique rowid's to
 * table rows*)
let next_val =
 let counter = ref 0 in fun () ->
   incr counter;
   !counter

(* [make_select tbl col] makes an empty Map.t with the same type as col in tbl*)
let make_select tbl col =
  if List.mem_assoc col tbl then Maps.empty (List.assoc col tbl)
  else raise (Failure "Column specified is not found in table")

let rec select_col tbl rows col acc =
  match rows with
  | [] -> acc
  | h::t -> if List.mem_assoc col tbl then
              let map = List.assoc col tbl in
              if Maps.is_member h map then
                select_col tbl t col (Maps.insert (Maps.lookup h map) h acc)
              else select_col tbl t col acc
            else raise (Failure "Column specified is not found in table")

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

let get_diff (t1:t) (t2:t) : int =
  let l1 = (get_size t1) in
  let l2 = (get_size t2) in
    abs (l1 - l2)

(* precondition:
 * postcondition: *)
let select clst tbl w =
  let map =
    (match w with
    | Condition (col,op,v) -> if List.mem_assoc col tbl then Maps.select (List.assoc col tbl) op v
                              else raise (Failure "Column specified is not found in table")
    | Null -> (match clst with
              | [] -> raise (Failure "No columns specified while selecting")
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

(* [get_cvlst clst vlst acc] accumulates the list of columns and list of values
 * into one (column, value) list
 * precondition: clst and vlst must be the same length *)
let rec get_cvlst (clst: column list) (vlst: value list)
                  (acc: (column * value) list) =
  match clst, vlst with
  | [],[] -> acc
  | h::t, h'::t' -> get_cvlst t t' (acc @ [(h,h')])
  | _, _ -> raise (Failure "List of columns and values must be the same length")

(* [insert_help tbl cvlst rowid acc] *)
let rec insert_help (tbl:t) (cvlst: (column * value) list) rowid acc =
  match tbl with
  | [] -> acc
  | (name, map)::tl ->
      if List.mem_assoc name cvlst then
        insert_help tl cvlst rowid
        (acc @ [(name, Maps.insert (List.assoc name cvlst) rowid map)])
      else insert_help tl cvlst rowid (acc @ [name, map])


let insert (tbl:t) (clst:column list) (vlst: value list) : t =
  let cvlst = (get_cvlst clst vlst []) in
  let rowid = next_val () in
    insert_help tbl cvlst rowid []


let rec insertAll_help (tbl:t) (vlst: value list) rowid acc =
  match tbl, vlst with
    | (name, map)::tl, a::b ->
        (insertAll_help tl b rowid (acc @ [(name, Maps.insert a rowid map)]))
    | _, _ -> acc

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



let rec get_all_rows table where =
  match table, where with
  | ((name,map)::t), (Condition (col,op,v)) -> (if (name = col) then Maps.select map op v
                                                                else get_all_rows t where)
  | ((name,map)::t) , Null -> map
  | _ -> raise (Failure "Column specified is not found in table")

let rec make_removed ids table =
  match table with
  | (name,map)::t -> (name, (Maps.delete ids map))::(make_removed ids t)
  | [] -> []

let delete table where =
  let rows = get_all_rows table where in
  let ids = Maps.get_rows rows in
  make_removed ids table

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
  Printf.printf "\n"; print_tbl_helper (Array.of_list (row_to_array ((strip_col tbl []) ::
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

let rec remove_on tbl col acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if col = name then acc @ t else remove_on t col (acc @ [(name, map)])

(* precondition:
 * postcondition: *)
let join t1 t2 o =
  let rows = (match o with
             | (c1, c2) -> if List.mem_assoc c1 t1 && List.mem_assoc c2 t2 then
                             Maps.join (List.assoc c1 t1) (List.assoc c2 t2)
                           else raise (Failure "Column specified is not found in table")) in
    List.rev (remove_on (List.rev (join_help ((empty_table t1 []) @
      (empty_table t2 [])) (get_cvlst t1 t2 rows []))) (snd o) [])


(*I am sad*)
let rec get_maps rowids tbl name new_map acc =
  match rowids with
  | [] -> (new_map, acc)
  | h::t -> if Maps.is_member h (List.assoc name tbl) then
              (get_maps t tbl name (Maps.insert (Maps.lookup h (List.assoc name tbl)) h new_map) acc)
            else
              (get_maps t tbl name (Maps.insert (Maps.get_type new_map) h new_map)
              (acc @ [(name, h, Maps.get_type new_map)]))

let rec get_all_maps rowidlst tbl (maps:t) (acc:t) inserts =
  match maps with
  | [] -> (acc, inserts)
  | (name, map)::t ->
    let nmap = (get_maps rowidlst tbl name (Maps.create (Maps.get_type map)) []) in
    get_all_maps rowidlst tbl t (acc @ [(name, fst nmap)]) (inserts @ (snd nmap))

(* precondition:
 * postcondition: *)
let rec update_help new_maps cvlst acc =
  match new_maps, cvlst with
  | [], [] -> acc
  | (a,b)::t, (c,v)::t' -> update_help t t' (acc @ [(a, Maps.update b v)])
  | _, _ -> raise (Failure "Different number of columns in table and column list")

let rec update_all_col tbl new_tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> update_all_col t new_tbl
                      (acc @ [(name,
                      (if List.mem_assoc name new_tbl
                        then Maps.replace map (List.assoc name new_tbl)
                       else map))])

let rec insert_nulls tbl inserts (acc:t) =
  match inserts with
  | [] -> acc
  | (name, rowid, v)::t -> insert_nulls tbl t (acc @ [(name, Maps.insert v rowid (List.assoc name tbl))])

let rec insert_all_nulls (tbl:t) null_cols (acc:t):t =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if List.mem_assoc name null_cols then
                        insert_all_nulls t null_cols (acc @ [(name, List.assoc name null_cols)])
                      else insert_all_nulls t null_cols (acc @ [name, map])

(* precondition:
 * postcondition: *)
let update tbl cvlst w =
  let new_tbl = select (get_col cvlst []) tbl w in
  let rowids = Maps.get_rows (Maps.get_longest (strip_tbl new_tbl []) 0 (Maps.create (VInt 0))) in
  let new_maps = (get_all_maps rowids tbl new_tbl [] []) in
  let updated_tbl = (update_help (fst new_maps) cvlst []) in
    ((print_tbl new_tbl); (print_tbl updated_tbl);
    (update_all_col (insert_all_nulls tbl (insert_nulls tbl (snd new_maps) []) []) updated_tbl []))

(*
TEST_MODULE "insert_test" = struct

  let tbl = [("Name", Maps.create (VString "")); ("Age", Maps.create (VInt 0));
             ("Height", Maps.create (VFloat 0.0))]

  let tbl' = insert tbl ["Name"; "Age"; "Height"] [VString "Annie"; VInt 19; VFloat 5.3]

  TEST_UNIT = get_size tbl' === 1

  let tbl'' = insert tbl' ["Name"; "Age"; "Height"] [VString "Erin"; VInt 19; VFloat 5.8]

  TEST_UNIT = get_size tbl'' === 2

  let _ = print_tbl tbl''

  let tbl''' = insert tbl'' ["Name"; "Height"] [VString "Frank"; VFloat 6.0]

  TEST_UNIT = get_size tbl''' === 3

  let _ = print_tbl tbl'''

  let sel = select ["Name"; "Age"] tbl''' (Condition ("Height", Gt, VFloat 5.4))

  let _ = print_tbl sel

  let del = delete tbl''' (Condition ("Height", Lt, VFloat 5.9))

  let _ = print_tbl del

  let tibble = [("Name", Maps.create (VString "")); ("Hair Color", Maps.create (VString ""));
                ("Male?", Maps.create (VBool true))]

  let tibble' = insert tibble ["Name";"Hair Color"; "Male?"] [VString "Louis"; VString "Black"; VBool true]

  let _ = print_tbl tibble'

  let tibble'' = insert tibble' ["Name";"Hair Color"; "Male?"] [VString "Frank"; VString "Black"; VBool true]

  let _ = print_tbl tibble''

  let tibble''' = insert tibble'' ["Name";"Hair Color"; "Male?"] [VString "Erin"; VString "Brown/Black"; VBool false]

  let _ = print_tbl tibble'''

  let j = join (tbl''') (tibble''') ("Name", "Name")

  let _ = print_tbl j

  let u = update j [("Age", VInt 20); ("Height", VFloat 7.0)] (Condition ("Name", Eq, VString "Frank"))

  let _ = print_tbl u

(*
  let _ = print_tbl u

  let dj = delete j (Condition ("Name", Eq, VString "Erin"))

  let _ = print_tbl dj

  let dj' = delete j (Null)

  let _ = print_tbl dj'

  let dj'' = delete j (Null)

  let _ = print_tbl dj''*)

end*)
(*
TEST_MODULE "insert_test" = struct

  let tbl = [("Name", Maps.create (VString "")); ("Age", Maps.create (VInt 0));
             ("Height", Maps.create (VFloat 0.0))]

  let tbl' = insertAll tbl [VString "Annie"; VInt 19; VFloat 5.3]
  let tbl'' = insertAll tbl' [VString "Erin"; VInt 19; VFloat 5.8]
  let tbl''' = insertAll tbl'' [VString "Frank"; VInt 19; VFloat 6.0]

  let _ = print_tbl tbl'''

  let sel = selectAll (tbl''') (Condition ("Name", LikeEnd, VString "k"))

  let _ = print_tbl sel

end
*)
