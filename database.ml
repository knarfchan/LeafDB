open Table

(* type representing our database *)
type t = (string, (string, Table.t) Hashtbl.t)

(* since this is an imperative data structure, we can
avoid the functional way of returning the structure after
altering it so... things return units?*)

(* i think all of this belongs in a higher module *)
(* takes the name of the database and returns the database *)
let use str = failwith "unimplemented"

(* add a table to a database *)
let add_table (db: Database.t) (str: string) (tbl: Table.t) : unit =
  let tables = snd db in
    Hashtbl.add tables str tbl

(* takes a table, removes it from the database *)
let drop (db: Database.t) (str: string) : unit  =
  let tables = snd db in
    Hashtbl.remove tables str
(* need another method in a higher module that takes a string db name and returns
the actual db*)
(* finds a table in the database based on its name *)
let lookup (db: Database.t) (str: string) : Table.t option =
  let tables = snd db in
    if Hashtbl.mem tables str then Some (Hashtbl.find tables str)
    else None