open Maps
open Str
open Typs

(*filesystem.ml*)

(* precondition: path is a valid directory path
 * postcondition: [get_files_paths p] returns a string list containing the
 *                name of all files in the directory p *)
let get_files_paths path : string list =
  let db_names = Sys.readdir path in
    Array.to_list(db_names)

(* precondition: databases exist in the "./DBMS" folder
 * postcondition: returns the names of the database folders in a string list *)
let get_database_names _ =
  get_files_paths("./DBMS")

(* precondition: none
 * postcondition: removes the file extension on a string if it exists *)
let remove_ext str =
  global_replace(regexp("\\..*"))("")(str)

(* precondition: none
 * postcondition: appends name to path with a "/" character between them
 *                if one does not already exist *)
let append_path path name =
  if string_match(regexp(".*/$"))(path)(0) then path ^ name
  else path ^ "/" ^ name

(* precondition: path is a valid filepath
 * postcondition: returns the full path of all the database folders in DBMS *)
let get_dbs_dir path =
  List.map(fun n -> append_path "./DBMS" n)(Array.to_list(Sys.readdir "./DBMS"))

(* precondition: name is a valid database name
 * postcondition: returns the database path with name name *)
let get_db_path name =
  append_path "./DBMS" name

(* precondition: db_str is a valid db name and tb_str is a valid table name
 * postcondition: returns the path to the database and table specified *)
let get_tbl_path db_str tbl_str =
  append_path(get_db_path(db_str))(tbl_str ^ ".csv")

let read_tbl db_str tbl_str =
  let path = get_tbl_path db_str tbl_str in
  Table.read_tbl(Csv.load(path))

let add_db db = Unix.mkdir(get_db_path db)(0o777)

let write_tbl (db_name:bytes) (tbl_name:bytes) (tbl:Table.t) : unit  =
  let mtx = Table.matrix_of_table tbl in
  Csv.save (get_tbl_path db_name tbl_name) mtx

(* precondition: db_str is the name of a db that exists
                 db is the Database.t object associated with db_str
                 tbl_lst contains all the table names in the db *)
let rec read_db_help (db_str: string) (db: Database.t) (tbl_lst: string list) =
  match tbl_lst with
  | [] -> ()
  | h::t -> ignore(Database.add_table(db)(h)(read_tbl db_str h));
              read_db_help(db_str)(db)(t)

let read_db (db_str: string) (db: Database.t) : unit =
  let tables = List.map(fun name -> remove_ext(name))(get_files_paths(get_db_path db_str)) in
    read_db_help(db_str)(db)(tables)

(* precondition: tbl_lst contains valid paths to csv files
 * postcondition: deletes all the files at the paths in tbl_lst *)
let rec delete_all_tables tbl_lst =
  match tbl_lst with
  | [] -> ()
  | h::t -> Sys.remove(h);delete_all_tables t

let delete_db db_str =
  let path = get_db_path db_str in
  let tables = List.map(fun n -> append_path path n)(get_files_paths path) in
    delete_all_tables(tables); Unix.rmdir(path)

let delete_tbl db_str tbl_str =
  let path = get_tbl_path db_str tbl_str in
    Sys.remove(path)

let add_tbl db_str tbl_str tbl =
  let path = get_tbl_path db_str tbl_str in
  let mtx = Table.matrix_of_table tbl in
    ignore(Sys.command("touch " ^ path)); Csv.save(path)(mtx)

let add_empty_tbl db_str tbl_str =
  let path = get_tbl_path db_str tbl_str in
    ignore(Sys.command("touch " ^ path));