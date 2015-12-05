(*filesystem.ml*)
open Maps
open Str
open Table

let read_db folder = failwith "not implemented"

let parse_item (s:string) =
  Str.search_forward

let read_row row acc =
  let new_row = Array.of_list row in
    let map = (match new_row.(i) with
    | "VInt" -> Maps.create (VInt 0)
    | "VString" -> Maps.create (VString "")
    | "VBool" -> Maps.create (VBool false)
    | "VFloat" -> Maps.create (VFloat 0.0)
    | _ -> failwith "Error: Not a valid SQL type") in
    for i = 2 to (Array.size new_row) - 1 do


let read_tbl_helper file =
  let matrix = ______ (string list list) in
  match matrix with
  | [] ->
  | h::t ->

let read_tbl file =


let add_db db = failwith "not implemented"

let write_tbl (db_name:bytes) (tbl_name:bytes) (tbl:Table.t) : unit  =
  let mtx = matrix_of_table tbl in
  Csv.save ("./DBMS/"^db_name^"/"^tbl_name^".csv") mtx

let delete_db file = failwith "not implemented"
