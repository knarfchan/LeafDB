open Database
open Interpret
open Table
open Maps

type t = (string, Database.t) Hashtbl.t

(* [use dbs str] takes the name of a database and returns it
 * precondition  : none
 * postcondition : *)
let use (dbs: t) (str: string) : Database.t option =
  if Hashtbl.mem dbs str then Some (Hashtbl.find tables str)
    else None

let print_tbl (tbl: Table.t) =


let print_commands (q: Table.t option) e =
  match q with
  | None -> Printf.printf "Table does not exist"
  | Some x -> match e with
              | Select _ ->

let repl (d:Database.t option) (e:expr) =
  let input = read_line() in
  match d with
  | None -> Printf.printf "Specify a function by calling USE [database]\n"
  | Some x -> (Interpret.eval x e)
