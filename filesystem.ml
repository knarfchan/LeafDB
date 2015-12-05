open Sys
open Maps
open Str
open Typs

(*filesystem.ml*)
(* gets the name of every file in the specified directory
 * path is the path of a folder you want the files from
 * it should begin with "./" ? *)
let get_files_paths path =
  let db_names = Sys.readdir path in
    Array.to_list(db_names)

(* remove extentions of the string *)
let remove_ext str =
  global_replace(regexp("\..*"))("")(str)

(* appends a filename to a path where it resides *)
let append_path path name =
  if string_match(regexp(".*/$"))(path)(0) then path ^ name
  else path ^ "/" ^ name

(* returns all the database folders full paths in DBMS *)
(* returns databases for simplicity *)
let get_dbs_dir path =
  List.map(fun name -> append_path "./DBMS" name)(Array.to_list(Sys.readdir "./DBMS"))

(* returns the database path with name name *)
let get_db_path name =
  append_path "./DBMS" name

(* takes a database folder and return a list of tuples, (name of the table, string list list)*)
let to_sll path db_lst =
  List.map(fun name -> remove_ext(name), Csv.load(path))(db_lst)

(* for annie *)
(* takes a database folder and a table and gives you a string list list *)
let dbms_sll db_name tab_name =
  to_sll(append_path (get_db_path(db_name)) (tab_name))

let read_db folder = failwith "not implemented"

let parse_item (s:string) =
  let sep = Str.search_forward (regexp_string "*") s 0 in
    (int_of_string (Bytes.sub s 0 sep),
    (Bytes.sub s (sep + 1) (Bytes.length s - sep - 1)))

let make_map typ =
  match typ with
  | "VInt" -> Maps.create (VInt 0)
  | "VString" -> Maps.create (VString "")
  | "VBool" -> Maps.create (VBool false)
  | "VFloat" -> Maps.create (VFloat 0.0)
  | _ -> raise (Failure "Error: Not a valid SQL type")

let rec parse_all_items row acc =
  match row with
  | [] -> acc
  | h::t -> parse_all_items t (acc @ [(parse_item h)])

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


let rec read_tbl_helper (matrix:bytes list list) acc =
  match matrix with
  | [] -> acc
  | (typ::name::t)::t'->
      read_tbl_helper t' (acc @ [(name, insert_items typ (make_map typ)
      (parse_all_items t []))])
  | _::t' -> raise (Failure "Error: Row should have more than 2 items")

let read_tbl file = failwith "unimplemented"

let add_db db = failwith "not implemented"

let write_tbl tbl = failwith "not implemented"

let delete_db file = failwith "not implemented"

let delete_tbl tbl = failwith "not implemented"

let add_tbl tbl = failwith "not implemented"
