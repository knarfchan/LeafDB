open Typs
open Ast

let eval (d : Database.t) (e : expr): Table.t option =
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
  | CreateTable(str, cdl) -> Database.add_table d str cdl
