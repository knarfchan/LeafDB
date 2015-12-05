open Maps
open Str
open Typs

(*filesystem.ml*)
(* gets the name of every file in the specified directory
 * path is the path of a folder you want the files from
 * it should begin with "./" ? *)
let get_files_paths path : string list =
  let db_names = Sys.readdir path in
    Array.to_list(db_names)

(* remove extentions of the string *)
let remove_ext str =
  global_replace(regexp("\\..*"))("")(str)

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

let get_tbl_path db_str tbl_str =
  append_path(get_db_path(db_str))(tbl_str ^ ".csv")

let read_tbl db_str tbl_str =
  let path = get_tbl_path db_str tbl_str in
  Table.read_tbl(Csv.load(path))

let add_db db = Unix.mkdir(get_db_path db)(0o777)

let write_tbl (db_name:bytes) (tbl_name:bytes) (tbl:Table.t) : unit  =
  let mtx = Table.matrix_of_table tbl in
  Csv.save (get_tbl_path db_name tbl_name) mtx

let rec read_db_help (db_str: string) (db: Database.t) (tbl_lst: string list) : unit =
  match tbl_lst with
  | [] -> ()
  | h::t -> ignore(Database.add_table(db)(h)(read_tbl db_str h));
              read_db_help(db_str)(db)(t)

let read_db (db_str: string) (db: Database.t) : unit =
  let tables = get_files_paths(get_db_path db_str) in
    read_db_help(db_str)(db)(tables)

let rec delete_all_tables tbl_lst =
  match tbl_lst with
  | [] -> ()
  | h::t -> Sys.remove(h);delete_all_tables t

let delete_db db_str =
  let path = get_db_path db_str in
  let tables = get_files_paths(path) in
    delete_all_tables(tables); Unix.rmdir(path)

let delete_tbl db_str tbl_str =
  let path = get_tbl_path db_str tbl_str in
    Unix.rmdir(path)

let add_tbl db_str tbl_str tbl =
  let path = get_tbl_path db_str tbl_str in
  let mtx = Table.matrix_of_table tbl in
    ignore(Sys.command("touch " ^ path)); Csv.save(path)(mtx)
