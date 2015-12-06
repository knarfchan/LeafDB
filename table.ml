(* table.ml *)
open Typs
open Maps
open Assertions
open Unix

type t = (column * Maps.t) list

(* [next_val ()] is a row id generator *)
let next_val =
  let time = ref (int_of_float (Unix.gettimeofday ())) in fun () ->
  incr time;
  abs(!time)

(* [to_table lst] converts a (string * Maps.t) list into a table *)
let to_table (lst: (string * Maps.t) list) = lst

(* [get_one_map] searches the table t for a column and returns the map *)
let get_one_map (column: string) (t:t) = List.assoc column t

(* [matrix_of_table tbl] returns a string matrix of the table *)
let rec matrix_of_table (tbl:t) =
  match tbl with
  |[] -> []
  |(col,map)::t -> (Maps.build_col col map)::(matrix_of_table t)

(* [make_select tbl col] makes an empty Map.t with the same type as col in tbl*)
let make_select tbl col =
  if List.mem_assoc col tbl then Maps.empty (List.assoc col tbl)
  else raise (Failure "Column specified is not found in table")

(* [select_col tbl rows col acc] accumulates all the values from a table tbl
 * and column col with a row id in rows *)
let rec select_col tbl rows col acc =
  match rows with
  | [] -> acc
  | h::t -> if List.mem_assoc col tbl then
              let map = List.assoc col tbl in
              if Maps.is_member h map then
                select_col tbl t col (Maps.insert (Maps.lookup h map) h acc)
              else select_col tbl t col acc
            else raise (Failure "Column specified is not found in table")

(* [all_col tbl clst rows acc] selects all the values in clst with a row id
 * in rows *)
let rec all_col tbl clst rows acc =
  match clst with
  | [] -> acc
  | h::t -> all_col tbl t rows
            (acc @ [(h, (select_col tbl rows h (make_select tbl h)))])

(* [strip_tbl tbl acc] returns a list of maps in Table tbl*)
let rec strip_tbl tbl acc : Maps.t list =
  match tbl with
  | [] -> acc
  | (_,b)::t -> (strip_tbl t (acc @ [b]))

(* [get_size tbl] returns the size of the table tbl*)
let get_size (tbl:t) : int =
  match tbl with
  | (a,b)::t ->
      Maps.size (Maps.get_longest (strip_tbl tbl []) 0 (List.assoc a tbl))
  | [] -> 0

(* [get_diff t1 t2] returns the difference in size between t1 and t2*)
let get_diff (t1:t) (t2:t) : int =
  let l1 = (get_size t1) in
  let l2 = (get_size t2) in
    abs (l1 - l2)

(* [select clst tbl w] selects the columns in clst from table tbl with the
 * condition w *)
let select clst tbl w =
  let map =
    (match w with
    | Condition (col,op,v) ->
        if List.mem_assoc col tbl then Maps.select (List.assoc col tbl) op v
        else raise (Failure "Column specified is not found in table")
    | Null ->
      (match clst with
      | [] -> raise (Failure "No columns specified while selecting")
      | h::t -> Maps.get_longest (strip_tbl tbl []) 0 (List.assoc h tbl))) in
  let rows = Maps.get_rows map in
    (all_col tbl clst rows [])

(* [strip_col tbl acc] returns the list of all the columns in table tbl *)
let rec strip_col (tbl:t) (acc:column list) :column list =
  match tbl with
  | [] -> acc
  | (a,_)::t -> (strip_col t (acc @ [a]))

(* [selectAll tbl w] selects all columns from table tbl with the condition w*)
let selectAll tbl w =
  select (List.rev (strip_col tbl [])) tbl w

(* [get_cvlst clst vlst acc] accumulates the list of columns and list of values
 * into one (column, value) list
 * precondition: clst and vlst must be the same length *)
let rec get_cvlst (clst: column list) (vlst: value list)
                  (acc: (column * value) list) =
  match clst, vlst with
  | [],[] -> acc
  | h::t, h'::t' -> get_cvlst t t' (acc @ [(h,h')])
  | _, _ -> raise (Failure "List of columns and values must be the same length")

(* [insert_help tbl cvlst rowid acc] inserts the values from cvlst into tbl with
 * the row id rowid*)
let rec insert_help (tbl:t) (cvlst: (column * value) list) rowid acc =
  match tbl with
  | [] -> acc
  | (name, map)::tl ->
      if List.mem_assoc name cvlst then
        insert_help tl cvlst rowid
        (acc @ [(name, Maps.insert (List.assoc name cvlst) rowid map)])
      else insert_help tl cvlst rowid (acc @ [name, map])

(* [insert tbl clst vlst] inserts the item specified by clst and vlst into tbl*)
let insert (tbl:t) (clst:column list) (vlst: value list) : t =
  let cvlst = (get_cvlst clst vlst []) in
  let rowid = next_val () in
    insert_help tbl cvlst rowid []

(* [insertAll_help tbl vlst rowid acc] inserts the values from vlst into tbl
 *  with the row id rowid *)
let rec insertAll_help (tbl:t) (vlst: value list) rowid acc =
  match tbl, vlst with
    | (name, map)::tl, a::b ->
        (insertAll_help tl b rowid (acc @ [(name, Maps.insert a rowid map)]))
    | _, _ -> acc

(* [insertAll] inserts the values in vlst into tbl *)
let insertAll (tbl:t) (vlst: value list) : t =
  let rowid = next_val () in
    insertAll_help tbl vlst rowid []

(* [get_col cvlst acc] returns the columns of a cvlst *)
let rec get_col cvlst acc =
  match cvlst with
  | [] -> acc
  | (c,_)::t -> get_col t (acc @ [c])

(* [get_val_from_cvlst cvlst acc] returns the values from a cvlst *)
let rec get_val_from_cvlst cvlst acc =
  match cvlst with
  | [] -> acc
  | (_,v)::t -> get_val_from_cvlst t (acc @ [v])

(* [get_all_rows table where] returns the rows of all the items in table that
 * satisfies the where condition *)
let rec get_all_rows table where =
  match table, where with
  | ((name,map)::t), (Condition (col,op,v)) ->
      (if (name = col) then Maps.select map op v else get_all_rows t where)
  | ((name,map)::t) , Null -> map
  | _ -> raise (Failure "Column specified is not found in table")

(* [make_removed ids table] remove the items from table with the row id ids*)
let rec make_removed ids table =
  match table with
  | (name,map)::t -> (name, (Maps.delete ids map))::(make_removed ids t)
  | [] -> []

(* [delete table where] deletes items from a table that satisfies the where
 * condition *)
let delete table where =
  let rows = get_all_rows table where in
  let ids = Maps.get_rows rows in
  make_removed ids table

(* [create_help cdl acc] creates a table with the columns specified in cdl*)
let rec create_help cdl acc =
  match cdl with
  | [] -> acc
  | (col, v)::t -> create_help t ((col, Maps.create v)::acc)

(* [create cdl] creates an empty table *)
let rec create (cdl : column_dec list) =
  create_help cdl []

(* [get_row row tbl acc] gets all the items in one row in table tbl *)
let rec get_row row tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if Maps.is_member row map then
                        get_row row t (acc @ [(Maps.lookup row map)])
                      else get_row row t (acc @ [VNull])

(* [get_all_rows row tbl acc] gets all the rows in the table rows from tbl *)
let rec get_all_rows_print rows tbl acc =
  match rows with
  | [] -> acc
  | h::t -> get_all_rows_print t tbl acc @ [(get_row h tbl [])]

(* [tbl_to_matrix tbl acc] converts a table to a matrix and accumulates
 * into acc *)
let rec tbl_to_matrix (tbl : t) acc =
  match tbl with
  | [] -> [[]]
  | (a,b)::t ->
    let new_map = Maps.get_longest (strip_tbl tbl []) 0 (List.assoc a tbl) in
      let rows = Maps.get_rows new_map in
        get_all_rows_print rows tbl acc

(* [convert_matrix] converts a table to a matrix *)
let convert_matrix (tbl:t) =
  tbl_to_matrix tbl []

(* [row_val r acc] converts the value in a row to a string *)
let rec row_val (r:value list) acc =
  match r with
  | [] -> acc
  | VInt x::t -> row_val t (acc @ [string_of_int x])
  | VString x::t -> row_val t (acc @ [x])
  | VBool x::t -> row_val t (acc @ [string_of_bool x])
  | VFloat x::t -> row_val t (acc @ [string_of_float x])
  | VNull::t -> row_val t (acc @ ["NULL"])

(* [tbl_val m acc] converts a value list list to a string list list*)
let rec tbl_val (m: value list list) acc: string list list=
  match m with
  | [] -> acc
  | h::t -> tbl_val t (acc @ [row_val h []])

(* [row_to_array] converts all the rows of a matrix to arrays *)
let rec row_to_array (m: column list list) acc =
  match m with
  | [] -> acc
  | h::t -> row_to_array t (acc @ [Array.of_list h])

(* [row_size tbl] returns a list of a the longest string in each column *)
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

(* [format_array tbl size] returns a string matrix where all values are the
 * same length*)
let format_array tbl size =
  (for i = 0 to (Array.length tbl) - 1 do
    for j = 0 to (Array.length tbl.(0)) -1 do
      tbl.(i).(j) <-
      (Bytes.make (size.(j) - (Bytes.length tbl.(i).(j))) ' ') ^ (tbl.(i).(j))
    done
  done); tbl

(* [get_bars size] is used to generate the column bars in printing *)
let get_bars (size: int array) length:int=
  (for i = 0 to (Array.length size) - 1 do
    length := !length + size.(i)
  done); length := !length + ((Array.length size) * 3) + 1; !length

(* [print_tbl_helper tbl] is used to help print a table from a string array
 * array *)
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

(* [print_tbl tbl] prints the table tbl *)
let print_tbl (tbl:t) =
  Printf.printf "\n"; print_tbl_helper
  (Array.of_list (row_to_array ((strip_col tbl []) ::
  (tbl_val (convert_matrix tbl) [])) []))

(* [get_val tbl row acc] gets all the values from a row in a table *)
let rec get_vals (tbl:t) row acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if Maps.is_member row map then
                        get_vals t row (acc @ [(name, Maps.lookup row map)])
                      else get_vals t row acc

(* [get_cvlst t1 t2 rows acc] returns a cvlst of the values from 2 different
 * tables *)
let rec get_cvlst (t1:t) (t2:t) rows acc =
  match rows with
  | [] -> acc
  | (r1, r2)::t -> get_cvlst t1 t2 t
                   (acc @ [(get_vals t1 r1 []) @ (get_vals t2 r2 [])])

(* [empty_table tbl acc] returns an empty table *)
let rec empty_table tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> (empty_table t (acc @ [(name, Maps.empty map)]))

(* [join_help tbl cvlst] creates a new table based on the cvlst *)
let rec join_help tbl cvlst =
  match cvlst with
  | [] -> tbl
  | lst::t -> join_help (insert tbl (get_col lst [])
              (get_val_from_cvlst lst [])) t

(* [remove_ontbl col acc] removes one column specified in the on command of a
 * join *)
let rec remove_on tbl col acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> if col = name then acc @ t
                      else remove_on t col (acc @ [(name, map)])

(* [join t1 t2 o] joins two tables t1 and t2 accordingly to the on command *)
let join t1 t2 o =
  let rows =
    (match o with
    | (c1, c2) -> if List.mem_assoc c1 t1 && List.mem_assoc c2 t2 then
                    Maps.join (List.assoc c1 t1) (List.assoc c2 t2)
                  else raise (Failure "Column specified is not found in table"))
    in List.rev (remove_on (List.rev (join_help ((empty_table t1 []) @
      (empty_table t2 [])) (get_cvlst t1 t2 rows []))) (snd o) [])

(* [parse_item s] parses a string into a type with the rowid and value *)
let parse_item (s:string) =
  let sep = Str.search_forward (Str.regexp_string "*") s 0 in
    (int_of_string (Bytes.sub s 0 sep),
    (Bytes.sub s (sep + 1) (Bytes.length s - sep - 1)))

(* [make_map typ] creates a map with the type specified by typ *)
let make_map typ =
  match typ with
  | "VInt" -> Maps.create (VInt 0)
  | "VString" -> Maps.create (VString "")
  | "VBool" -> Maps.create (VBool false)
  | "VFloat" -> Maps.create (VFloat 0.0)
  | _ -> raise (Failure "Error: Not a valid SQL type")

(* [parse_all_items] parses all the items in a column into a rowid, value list*)
let rec parse_all_items row acc =
  match row with
  | [] -> acc
  | h::t -> parse_all_items t (acc @ [(parse_item h)])

(* [insert_items typ map row] the parsed items from a row of a csv file into a
 * map *)
let rec insert_items (typ:string) map row =
  match typ, map, row with
  | "VInt", m, (id, v)::t ->
      insert_items typ (Maps.insert (VInt (int_of_string v)) id m) t
  | "VString", m, (id, v)::t ->
      insert_items typ (Maps.insert (VString v) id m) t
  | "VBool", m, (id, v)::t ->
      insert_items typ (Maps.insert (VBool (bool_of_string v)) id m) t
  | "VFloat", m, (id, v)::t ->
      insert_items typ (Maps.insert (VFloat (float_of_string v)) id m) t
  | _ , _, [] -> map
  | _, _, _ -> raise (Failure "Error: Type and Map should be specified")

(* [read_tbl_helper] converts the matrix into a table into the accumulator acc*)
let rec read_tbl_helper (matrix:bytes list list) (acc:t) =
  match matrix with
  | [] -> acc
  | (typ::name::t)::t'->
      read_tbl_helper t' (acc @ [(name, insert_items typ (make_map typ)
      (parse_all_items t []))])
  | _::t' -> raise (Failure "Error: Row should have more than 2 items")

(* [read_tbl_helper] converts a bytes list list into a table to read from a
 * csv file *)
let read_tbl (matrix:bytes list list) =
  read_tbl_helper matrix []

(* [get_maps rowids tbl name new_map acc] returns a column using the rowids and
 * accumulates the new items inserted *)
let rec get_maps rowids tbl name new_map acc =
  match rowids with
  | [] -> (new_map, acc)
  | h::t ->
    if Maps.is_member h (List.assoc name tbl) then
      (get_maps t tbl name (Maps.insert
      (Maps.lookup h (List.assoc name tbl)) h new_map) acc)
    else
      (get_maps t tbl name (Maps.insert (Maps.get_type new_map) h new_map)
      (acc @ [(name, h, Maps.get_type new_map)]))

(* [get_all_maps] returns a table with the row ids in the list rowids and
 * returns a list of the items inserted*)
let rec get_all_maps rowid tbl (maps:t) (acc:t) inserts =
  match maps with
  | [] -> (acc, inserts)
  | (name, map)::t ->
    let nmap = (get_maps rowid tbl name (Maps.create (Maps.get_type map)) []) in
    get_all_maps rowid tbl t (acc @ [(name, fst nmap)]) (inserts @ (snd nmap))

(* [update_help new_maps cvlst acc] updates the new maps with the values from
 * cvlst *)
let rec update_help new_maps cvlst acc =
  match new_maps, cvlst with
  | [], [] -> acc
  | (a,b)::t, (c,v)::t' -> update_help t t' (acc @ [(a, Maps.update b v)])
  | _, _ ->
    raise (Failure "Different number of columns in table and column list")

(* [update_all_col tbl new_tbl acc] updates all the values in the original table
 * with the values in the new table*)
let rec update_all_col tbl new_tbl acc =
  match tbl with
  | [] -> acc
  | (name, map)::t -> update_all_col t new_tbl
                      (acc @ [(name,
                      (if List.mem_assoc name new_tbl
                        then Maps.replace map (List.assoc name new_tbl)
                       else map))])

(* [insert_nulls tbl inserts acc] inserts once null values into a table *)
let rec insert_nulls tbl inserts (acc:t) =
  match inserts with
  | [] -> acc
  | (name, rowid, v)::t ->  insert_nulls tbl t (acc @
                            [(name, Maps.insert v rowid (List.assoc name tbl))])

(* [insert_all_nulls] inserts the updated null values back into tbl *)
let rec insert_all_nulls (tbl:t) null_cols (acc:t):t =
  match tbl with
  | [] -> acc
  | (name, map)::t ->
    if List.mem_assoc name null_cols then
      insert_all_nulls t null_cols (acc @ [(name, List.assoc name null_cols)])
    else insert_all_nulls t null_cols (acc @ [name, map])

(* [update tbl cvlst w] updates the items specified by where with the values
 * from cvlst into tbl*)
let update tbl cvlst w =
  let new_tbl = select (get_col cvlst []) tbl w in
  let new_tbl2 = match w with
  | Null -> select (get_col cvlst []) tbl w
  | Condition (c, _, _) -> select (c::(get_col cvlst [])) tbl w in
    let rowids = Maps.get_rows (Maps.get_longest (strip_tbl new_tbl2 []) 0
                 (Maps.create (VInt 0))) in
    let new_maps = (get_all_maps rowids tbl new_tbl [] []) in
    let updated_tbl = (update_help (fst new_maps) cvlst []) in
      (update_all_col (insert_all_nulls tbl (insert_nulls tbl (snd new_maps) [])
      []) updated_tbl [])

(*
TEST_MODULE "insert_test" = struct

  (* Insert Test Cases *)
  let tbl = [("Name", Maps.create (VString "")); ("Age", Maps.create (VInt 0));
             ("Height", Maps.create (VFloat 0.0))]

  let tbl' = insert tbl ["Name"; "Age"; "Height"]
             [VString "Annie"; VInt 19; VFloat 5.3]

  TEST_UNIT = get_size tbl' === 1

  let tbl'' = insert tbl' ["Name"; "Age"; "Height"]
              [VString "Erin"; VInt 19; VFloat 5.8]

  TEST_UNIT = get_size tbl'' === 2

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (tbl'')))
              tbl'' []) === [[VString "Annie"; VInt 19; VFloat 5.3];
                            [VString "Erin"; VInt 19; VFloat 5.8]]

  let tbl''' = insert tbl'' ["Name"; "Height"] [VString "Frank"; VFloat 6.0]

  TEST_UNIT = get_size tbl''' === 3

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (tbl''')))
              tbl''' []) === [[VString "Annie"; VInt 19; VFloat 5.3];
                            [VString "Erin"; VInt 19; VFloat 5.8];
                            [VString "Frank"; VNull; VFloat 6.0]]

  (* Select Test Cases *)
  let sel = select ["Name"; "Age"] tbl''' (Condition ("Height", Gt, VFloat 5.4))

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Name" (sel)))
              sel []) === [[VString "Erin"; VInt 19];
                            [VString "Frank"; VNull]]

  (* Delete Test Cases *)
  let del = delete tbl''' (Condition ("Height", Lt, VFloat 5.9))

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (del)))
              del []) === [[VString "Frank"; VNull; VFloat 6.0]]

  let tibble = [("Name", Maps.create (VString ""));
                ("Hair Color", Maps.create (VString ""));
                ("Male?", Maps.create (VBool true))]
  let tibble' = insert tibble ["Name";"Hair Color"; "Male?"]
                              [VString "Louis"; VString "Black"; VBool true]

  let tibble'' = insert tibble' ["Name";"Hair Color"; "Male?"]
                                [VString "Frank"; VString "Black"; VBool true]

  let tibble''' = insert tibble'' ["Name";"Hair Color"; "Male?"]
                  [VString "Erin"; VString "Brown/Black"; VBool false]

  TEST_UNIT = (get_all_rows_print (Maps.get_rows(List.assoc "Name" (tibble''')))
              tibble''' []) ===
                [[VString "Erin"; VString "Brown/Black"; VBool false];
                [VString "Frank"; VString "Black"; VBool true];
                [VString "Louis"; VString "Black"; VBool true]]

  (* Join Test Cases *)
  let j = join (tbl''') (tibble''') ("Name", "Name")

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (j)))
              j []) ===
    [[VString "Erin"; VInt 19; VFloat 5.8; VString "Brown/Black"; VBool false];
    [VString "Frank"; VNull; VFloat 6.; VString "Black"; VBool true]]

  (* Update Test Cases *)
  let u = update j [("Age", VInt 20); ("Height", VFloat 7.0)]
          (Condition ("Name", Eq, VString "Frank"))

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (u)))
              u []) ===
    [[VString "Erin"; VInt 19; VFloat 5.8; VString "Brown/Black"; VBool false];
    [VString "Frank"; VInt 20; VFloat 7.; VString "Black"; VBool true]]

  let _ = print_tbl tbl'''

  let u' = update (tbl''') [("Age", VInt 20)]
          (Condition ("Name", Eq, VString "Frank"))

  let _ = print_tbl u'

  (* Delete Test Cases *)
  let dj = delete j (Condition ("Name", Eq, VString "Erin"))

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (dj)))
              dj []) ===
    [[VString "Frank"; VNull; VFloat 6.; VString "Black"; VBool true]]

  let dj' = delete j (Null)

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (dj')))
              dj' []) === []

  let dj'' = delete j (Null)

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Height" (dj'')))
              dj'' []) === []

  (* Insert All Test Cases *)
  let tbls = [("Name", Maps.create (VString "")); ("Age", Maps.create (VInt 0));
             ("Height", Maps.create (VFloat 0.0))]

  let tbls' = insertAll tbl [VString "Annie"; VInt 19; VFloat 5.3]

  let tbls'' = insertAll tbl' [VString "Erin"; VInt 19; VFloat 5.8]

  let tbls''' = insertAll tbl'' [VString "Frank"; VInt 19; VFloat 6.0]

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Name" (tbls''')))
              tbls''' []) === [[VString "Annie"; VInt 19; VFloat 5.3];
              [VString "Erin"; VInt 19; VFloat 5.8];
              [VString "Frank"; VInt 19; VFloat 6.]]

  (* Select All Test Cases *)
  let sel' = selectAll (tbls''') (Condition ("Name", LikeEnd, VString "k"))

  TEST_UNIT = (get_all_rows_print (Maps.get_rows (List.assoc "Name" (sel')))
              sel' []) === [[VFloat 6.; VInt 19; VString "Frank"]]

<<<<<<< HEAD
end*)
