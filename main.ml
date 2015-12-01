open Dbms
open Database
open Interpret
open Table
open Maps
open Ast

(*let print_tbl (tbl: Table.t) =*)
let print_command (q: Table.t) e =
  match e with
  | Select (lst, tbl, w) -> failwith "unimplemented"
  | SelectAll (tbl, w) -> failwith "unimplemented"
  | Insert (tbl, clst, vlst) -> (match Database.lookup tbl with
                                | Some t -> Printf.printf "Inserted %d items" (Table.get_diff t q)
                                | None -> Printf.printf "Table does not exist")
  | InsertAll (tbl, vlst) -> (match Database.lookup tbl with
                              | Some t -> Printf.printf "Inserted %d items" (List.length vlst)
                              | None -> Printf.printf "Table does not exist")
  | JoinTables (str1, str2, o) -> failwith "unimplemented"
  | JoinTabQuer (str, e, o) -> failwith "unimplemented"
  | JoinQuerTab (e, str, o) -> failwith "unimplemented"
  | JoinQueries (e1, e2, o) -> failwith "unimplemented"
  | Update (tbl, cvlst, w) -> Printf.printf "Updated %d items" (List.length cvlst)
  | Delete (tbl, w) -> (match Database.lookup tbl with
                        | Some t -> Printf.printf "Deleted %d items" (Table.get_diff q t)
                        | None -> Printf.printf "Table does not exist")
  | CreateTable (str, cdl) -> failwith "unimplemented"
  | CreateDb str -> failwith "unimplemented"
  | DropTable str -> failwith "unimplemented"
  | DropDb str -> failwith "unimplemented"

let valid_command (q: Table.t option) e =
  match q with
  | None -> Printf.printf "Table does not exist"
  | Some x -> print_command x e

let repl (d:Database.t option) (e:expr) = failwith "no"
  (*let input = read_line() in
  match d with
  | None -> Printf.printf "Specify a function by calling USE [database]\n"
  | Some x -> (Interpret.eval x e)*)