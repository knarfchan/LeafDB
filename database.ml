open Table
open Typs

(* type representing our database *)
type t = (string, Table.t) Hashtbl.t

(* creates an empty Database.t *)
let create () : t =
  Hashtbl.create 7

(* [add_table db str tbl] adds a table to a specified database
 * precondition  : none
 * postcondition : mem tables str ~ add_table db str tbl
                   mem tables str ~ add_table db str tbl *)
let add_table (db: t) (str: string) (tbl: Table.t) : bool =
  if Hashtbl.mem db str then false
  else ((Hashtbl.add db str tbl); true)

(* specs will go here *)
let update_table (db: t) (str: string) (tbl: Table.t) : unit =
  Hashtbl.replace db str tbl

(* takes the table with name str, removes it from the database *)
let drop (db: t) (str: string) : bool =
  if Hashtbl.mem db str then (Hashtbl.remove db str; true)
  else false

(* precondition  : none
 * postcondition : [lookup db str] returns Some table in the database db
                    with name str if it exists and None otherwise *)
let lookup (db: t) (str: string) : Table.t option =
  if Hashtbl.mem db str then Some (Hashtbl.find db str)
  else None

let rec insert_tables (strs: value list) (tab: Table.t) : Table.t =
  match strs with
  | [] -> tab
  | h::t -> insert_tables(t)(Table.insertAll(tab)([h]))

let get_tables (d: t) : Table.t =
  let tb_list = ref [] in
    (Hashtbl.iter (fun x y -> tb_list := VString(x)::(!tb_list))(d));
    let new_tab = Table.create([("Tables", VString(""))]) in
      insert_tables(!tb_list)(new_tab)