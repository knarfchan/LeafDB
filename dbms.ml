open Typs

type t = (string, Database.t) Hashtbl.t

let create () : t =
  Hashtbl.create 5

(* adds an empty database to a DBMS *)
let add_database (dbs: t) (str: string) : bool =
  if Hashtbl.mem dbs str then false
  else (Hashtbl.add dbs str (Database.create()); true)

(* [use dbs str] takes the name of a database and returns it
 * precondition  : none
 * postcondition : *)
let use (dbs: t) (str: string) : Database.t option =
  if Hashtbl.mem dbs str then Some (Hashtbl.find dbs str)
  else None

let drop (dbs: t) (str: string) : bool =
  if Hashtbl.mem dbs str then (Hashtbl.remove dbs str; true)
  else false

let rec insert_databases (strs: value list) (tab: Table.t) : Table.t =
  match strs with
  | [] -> tab
  | h::t -> insert_databases(t)(Table.insertAll(tab)([h]))

let get_databases (dbs: t) : Table.t =
  let db_list = ref [] in
    (Hashtbl.iter (fun x y -> db_list := VString(x)::(!db_list))(dbs));
    let new_tab = Table.create([("Databases", VString(""))]) in
      insert_databases(!db_list)(new_tab)

let rec load_help (dbs: t) (db_names: string list) : unit =
  match db_names with
  | [] -> ()
  | h::t -> ignore(add_database(dbs)(h)); load_help(dbs)(t)

let load_databases (db_names : string list) : t =
  let dbs = create() in
  load_help(dbs)(db_names); dbs
