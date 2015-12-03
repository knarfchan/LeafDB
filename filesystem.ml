open Sys
(*filesystem.ml*)

let get_files_paths folder =
  let dir = "/path/to/dir" in
  let children = Sys.readdir dir in
    Array.iter print_endline children;;

let read_db folder = failwith "not implemented"

let read_tbl file = failwith "not implemented"

let add_db db = failwith "not implemented"

let write_tbl tbl = failwith "not implemented"

let delete_db file = failwith "not implemented"
