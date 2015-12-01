open Table

(* type representing our database *)
type t = (string * Table.t) Hashtbl.t

(* [add_table db str tbl] adds a table to a specified database
 * precondition  : none
 * postcondition : mem tables str ~ true implies add_table db str tbl ~ Some ()
                   mem tables str ~ false implies add_table db str tbl ~ None *)
let add_table (db: Database.t) (str: string) (tbl: Table.t) : unit =
  let tables = snd db in
    if Hashtbl.mem tables str then None
    else (Hashtbl.add tables str tbl; Some())

(* takes the table with name str, removes it from the database *)
let drop (db: Database.t) (str: string) : unit option =
  let tables = snd db in
    if Hashtbl.mem tables str then (Hashtbl.remove tables str; Some ())
    else None

(* precondition  : none
 * postcondition : [lookup db str] returns Some table in the database db
                    with name str if it exists and None otherwise *)
let lookup (db: Database.t) (str: string) : Table.t option =
  let tables = snd db in
    if Hashtbl.mem tables str then Some (Hashtbl.find tables str)
    else None