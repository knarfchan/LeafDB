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