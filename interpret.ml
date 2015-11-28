open Types
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


