open Typs
open Ast

type evaluated = Table.t option * bool
type dbresult = (Database.t option) * string option * bool

let attempt_op db tbl op =
  match Database.lookup db tbl with
  | None -> (None, false)
  | Some x -> (Some (op x), true)

let attempt_join p1 p2 o =
  match p1, p2 with
  | Some x, Some y -> (Some (Table.join x y o), true)
  | _ -> (None, false)

let eval_select (db: Database.t) (e: expr) : Table.t option =
  match e with
  | Select (lst, tbl, w) ->
      (match Database.lookup db tbl with
        | None -> None
        | Some x -> Some(Table.select lst x w))
  | SelectAll (tbl, w) ->
      (match Database.lookup db tbl with
        | None -> None
        | Some x -> Some(Table.selectAll x w))
  | _ -> None

let eval (db : Database.t) (e : expr): evaluated =
  match e with
  | Select (lst, tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.select lst x w)
  | SelectAll (tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.selectAll x w)
  | Insert (tbl, clst, vlst) ->
      attempt_op(db)(tbl)(fun x -> Table.insert x clst vlst)
  | InsertAll (tbl, vlst) ->
      (attempt_op(db)(tbl)(fun x -> Table.insertAll x (List.rev vlst)))
  | Delete (tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.delete x w)
  | Update (tbl, cvlst, w) ->
      attempt_op(db)(tbl)(fun x -> Table.update x cvlst w)
  | JoinTables (str1, str2, o) ->
      attempt_join (Database.lookup db str1) (Database.lookup db str2) o
  | JoinTabQuer (str, e, o) ->
      attempt_join (Database.lookup db str) (eval_select db e) o
  | JoinQuerTab (e, str, o) ->
      attempt_join (eval_select db e) (Database.lookup db str) o
  | JoinQueries (e1, e2, o) -> attempt_join (eval_select db e1) (eval_select db e2) o
  | CreateTable(str, cdl) -> (None, Database.add_table db str (Table.create cdl))
  | DropTable(str) -> (None, Database.drop db str)
  | _ -> (None, false)

let eval_dbms (dbs : Dbms.t) (e) : dbresult =
  match e with
  | CreateDb(str) -> (None, None, Dbms.add_database dbs str)
  | DropDb(str) -> (None, None, Dbms.drop dbs str)
  | Use(str) -> (match Dbms.use dbs str with
                | Some d -> (Some d, Some str, true)
                | None -> (None, Some str, false))
                (*(Dbms.use dbs str, Some str, true)*)
  | ExitDb -> exit 0
  | _ -> (None, None, false)
