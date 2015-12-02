open Typs
open Ast

type evaluated = Table.t option * bool

let attempt_op db tbl op =
  match Database.lookup db tbl with
  | None -> (None, false)
  | Some x -> (Some (op x), true)

let attempt_join p1 p2 o =
  match p1 p2 with
  | Some x, Some y -> (Some(Table.join x y o), true)
  | _ -> (None, false)

let eval_select (db: Database.t) (e: expr) : Table.t option =
  match e with
  | Select (lst, tbl, w) ->
      (match Database.lookup db tbl with
        | None -> None
        | Some x -> Some(Table.select lst x w))
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
      attempt_op(db)(tbl)(fun x -> Table.insertAll x vlst)
  | Delete (tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.delete x w)
  | Update (tbl, cvlst, w) ->
      attempt_op(db)(tbl)(fun x -> Table.update x cvlst w)
  | JoinTables (str1, str2, o) ->
      (match Database.lookup db str1, Database.lookup db str2 with
            | Some x, Some y -> (Some(Table.join x y o), true)
            | _ -> (None, false))
  | JoinTabQuer (str, e, o) ->
      (match Database.lookup db str, eval_select db e with
            | Some x, Some y -> (Some(Table.join x y o), true)
            | _ -> (None, false))
  | JoinQuerTab (e, str, o) ->
      (match eval_select db e, Database.lookup db str with
            | Some x, Some y -> (Some(Table.join x y o), true)
            | _ -> (None, false))
  | JoinQueries (e1, e2, o) ->
      (match eval_select db e1, eval_select db e2 with
            | Some x, Some y -> (Some(Table.join x y o), true)
            | _ -> (None, false))
  | _ -> (None, false)

let eval_dbms (dbms : Dbms.t) (e) : TBD =
  | CreateTable(str, cdl) -> (None, Database.add_table db str (Table.create cdl))
  | CreateDb(str) -> (None, Dbms.add_database str)
  | DropTable(str) -> (None, Database.drop db str)
  | DropDb(str) -> (None, Dbms.drop db str)
