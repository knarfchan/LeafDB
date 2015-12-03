open Ast

let print_command2 (q: Table.t option) (d: Database.t) (e: expr) (b: bool) =
  match e with
  | Select (lst, tbl, w) -> (match q with
                            | Some t -> (Table.print_tbl t)
                            | None -> Printf.printf "Error: Select failed. Table %s not found.\n" tbl)
  | SelectAll (tbl, w) -> (match q with
                          | Some t -> (Table.print_tbl t)
                          | None -> Printf.printf "Error: Insert failed. Table %s not found.\n" tbl)
  | Insert (tbl, clst, vlst) -> (match q with
                                | Some t1 -> (match Database.lookup d tbl with
                                              | Some t2 -> Printf.printf "Inserted %d items into table %s.\n" (Table.get_diff t1 t2) tbl
                                              | None -> Printf.printf "Error: Insert failed. Table %s not found.\n" tbl)
                                | None -> Printf.printf "Error: Insert failed. Table %s not found.\n" tbl)
  | InsertAll (tbl, vlst) -> (match q with
                              | Some t -> Printf.printf "Inserted %d items into table %s.\n" (List.length vlst) tbl
                              | None -> Printf.printf "Error: Insert all failed. Table %s not found.\n" tbl)
  | JoinTables (str1, str2, o) -> (match q with
                                  | Some t -> Table.print_tbl t
                                  | None -> Printf.printf "Error: Join failed. Table not found.\n")
  | JoinTabQuer (str, e, o) -> (match q with
                                | Some t -> Table.print_tbl t
                                | None -> (Printf.printf "Error: Join failed. Table not found.\n"))
  | JoinQuerTab (e, str, o) -> (match q with
                                | Some t -> Table.print_tbl t
                                | None -> Printf.printf "Error: Join failed. Table not found.\n")
  | JoinQueries (e1, e2, o) -> (match q with
                                | Some t -> Table.print_tbl t
                                | None -> Printf.printf "Error: Join failed. Table not found.\n")
  | Update (tbl, cvlst, w) -> (match q with
                              | Some t -> (let updates = Table.get_size (Table.selectAll t w) in
                                          Printf.printf "Updated %d items in table %s.\n" updates) tbl
                              | None -> Printf.printf "Error: Update failed. Table %s not found.\n" tbl)
  | Delete (tbl, w) -> (match q with
                        | Some t1 -> (match Database.lookup d tbl with
                                      | Some t2 -> Printf.printf "Deleted %d items in table %s.\n" (Table.get_diff t1 t2) tbl
                                      | None -> Printf.printf "Error: Delete failed. Table %s not found.\n" tbl)
                        | None -> Printf.printf "Error: Delete failed. Table %s not found.\n" tbl)
  | CreateTable(str, cdl) -> (if b then Printf.printf "Table %s created.\n" str
                             else Printf.printf "Error: Create table failed. Table %s already exists.\n" str)
  | DropTable(str) -> (if b then Printf.printf "Table %s dropped.\n" str
                      else Printf.printf "Error: Drop table failed. Table %s not found.\n" str)
  | _ -> failwith "Invalid command.\n"

let print_command1 (e: expr) (b: bool) =
  match e with
  | CreateDb(str) -> (if b then (Printf.printf "Database with name %s created.\n" str)
                     else (Printf.printf "Error: Create database failed. Database %s already exists.\n" str))
  | DropDb(str) -> (if b then (Printf.printf "Database with name %s dropped.\n" str)
                   else (Printf.printf "Error: Drop database failed. Database %s not found.\n" str))
  | Use(str) -> (if b then (Printf.printf "Entered database with name %s.\n" str)
                else (Printf.printf "Error: Database %s not found.\n" str))
  | _ -> failwith "Invalid command.\n"

let rec repl2 (dbs: Dbms.t) (d: Database.t) (name: string)=
  Printf.printf "\027[32mLeafDB>%s>" name;Printf.printf("\027[37m");
  let input = read_line() in
  let e = Test.parse input in
  Printf.printf"%s" (Test.ast_to_string e);
    if e = ExitDb then (Printf.printf "Exiting database.\n"; repl1 dbs)
    else let (t,b) = Interpret.eval d e in
    (print_command2 t d e b ; repl2 dbs d name)

and repl1 (dbs: Dbms.t) =
  Printf.printf("\027[32mLeafDB>");Printf.printf("\027[37m");
  let input = read_line() in
  let e = Test.parse input in
  Printf.printf"%s" (Test.ast_to_string e);
  if e = ShowDatabases then (Table.print_tbl (Dbms.get_databases dbs); repl1 dbs)
  else let result = Interpret.eval_dbms dbs e in
      match result with
      | (Some (d), Some(s), b) -> (print_command1 e b; if b then repl2 dbs d s else repl1 dbs)
      | (_, _, b) -> (print_command1 e b; repl1 dbs)

let main = repl1 (Dbms.create ())