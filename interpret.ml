open Typs
open Ast

let eval (d : Database.t) (e : expr): Query.t option = failwith "unimplemented"

  (*
  match expr with
  | Select (lst, tbl, w) ->
      let q = Query.convert (Table.select lst (Database.lookup tbl) w) in
        if q.is_empty then None else Some q
  | Insert (tbl, clst, vlst) ->
      let q = Query.convert Table.insert (Database.lookup tbl) clst vlst in
        if q.is_empty then None else Some q
  | Join (t1, t2, o) ->
      let q = Query.convert Table.join t1 t2 o in
        if q.is_empty then None else Some q
  | Update (tbl, lst, w) ->
      let q = if w <> Null then Table.update tbl lst w
              else Table.updateAll tbl lst in
        if q.is_empty then None else Some q
  | Delete (tbl, lst) ->
      let q = Table.delete tbl lst in
        if q.is_empty then None else Some q
  | Create (lst, tbl) -> Database.create lst tbl (*Returns a database*)
  | Drop tbl -> Database.drop (Database.lookup tbl) d (*Returns a database*)
*)


type expr =
  | Select of column list * string * where
  | SelectAll of string * where
  | Insert of string * column list * value list
  | Join of string * string * on
  (*| JoinTables of string * string * on
  | JoinTabQuer of string * expr * on
  | JoinQuerTab of expr * string * on
  | JoinQueries of expr * expr * on*)
  | Update of string * (column, value) list * where
  | Delete of string * (column, value) list * where
  | CreateTable of string * column_dec list
  | CreateDb of string
  | DropTable of string
  | DropDb of string

let eval (d : database) (e : expr): Table.t option =
  match expr with
  | Select (lst, tbl, w) -> match Database.lookup tbl with
                            | None -> None
                            | Some x -> Table.select lst x w
  | SelectAll (tbl, w) -> failwith "Unimplemented"
  | Insert (tbl, clst, vlst) -> (*Write insert method*)
      match Database.lookup tbl with
      | None -> None
      | Some x -> Some (Table.insert x clst vlst)
  | Join (t1, t2, o) -> (*Write join method*)
      match Database.lookup t1, Database.lookup t2 with
      | None, None -> None
      | None, Some x -> Some x (*I am unsure about this*)
      | Some x, None -> Some x (*I am unsure about this*)
      | Some x, Some y -> Table.join x y o
  | Update tbl cvlst w -> (*How will I know to update versus updateAll*)
      match Database.lookup t1 with
      | None -> None
      | Some x -> Table.update x cvlst w
  | Delete tbl cvlst w ->
      match Database.lookup tbl with
      | None -> None
      | Some x -> Table.delete x cvlst w
>>>>>>> 356b929afc053ccd0b897d58a0e838dd0afe7033


