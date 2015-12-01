open Database
open Table
open Maps
open Format
open Typs

type t = (string, Database.t) Hashtbl.t

(* [use dbs str] takes the name of a database and returns it
 * precondition  : none
 * postcondition : *)
let use (dbs: t) (str: string) : Database.t option =
  if Hashtbl.mem dbs str then Some (Hashtbl.find dbs str)
    else None

let rec row_val (r:value list) acc =
  match r with
  | [] -> acc
  | VInt x::t -> row_val t (acc @ [string_of_int x])
  | VString x::t -> row_val t (acc @ [x])
  | VBool x::t -> row_val t (acc @ [string_of_bool x])
  | VFloat x::t -> row_val t (acc @ [string_of_float x])
  | VNull::t -> row_val t (acc @ ["NULL"])

let rec tbl_val (m: value list list) acc: string list list=
  match m with
  | [] -> acc
  | h::t -> tbl_val t (acc @ [row_val h []])

let rec row_to_array (m: column list list) acc =
  match m with
  | [] -> acc
  | h::t -> row_to_array t (acc @ [Array.of_list h])

let row_size tbl =
  let size = Array.make (Array.length tbl.(0)) 0 in
    (for i = 0 to (Array.length tbl) - 1 do
      for j = 0 to (Array.length tbl.(0)) -1 do
        if String.length tbl.(i).(j) > size.(j) then
          size.(j) <- (String.length tbl.(i).(j) + 4)
        else ()
      done
    done);
  size

(* precondition: the number of columns of tbl and size must be the same
 * postcondition: format_array returns a matrix where all values are the same size*)
let format_array tbl size =
  (for i = 0 to (Array.length tbl) - 1 do
    for j = 0 to (Array.length tbl.(0)) -1 do
      tbl.(i).(j) <- (String.make (size.(j) - (String.length tbl.(i).(j))) ' ') ^ (tbl.(i).(j))
    done
  done); tbl

let print_tbl_helper tbl =
  let matrix = format_array tbl (row_size tbl) in
    for i = 0 to (Array.length matrix) - 1 do
      (for j = 0 to (Array.length matrix.(0)) -1 do
        Printf.printf "%s" tbl.(i).(j)
      done);
      (Printf.printf "\n")
    done

let rec col_to_string (clst:column list) acc : string list=
  match clst with
  | [] -> acc
  | h::t -> col_to_string t (acc @ [h])

let print_tbl (tbl:Table.t) =
  (*print_tbl_helper (Array.of_list
    (row_to_array ((col_to_string (Table.strip_col tbl []) []) :: *)
      (tbl_val (Table.convert_matrix tbl) []) (*))))*)

let print_commands (q: Table.t option) e = failwith "Error"
  (*
  match q with
  | None -> Printf.printf "Table does not exist"
  | Some x -> match e with
              | Select _ -> *)

