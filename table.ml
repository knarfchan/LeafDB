(* table.ml *)

type 'a node = {mutable prev: 'a node option; mutable next: 'a node option;
                value: 'a}

type 'a dlist = {mutable first: 'a node option; mutable last: 'a node option;}

type t = 'a dlist

(* a match between columns for relating tables *)
type on = column * column