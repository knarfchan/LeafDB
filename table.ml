(* table.ml *)
open Types

type 'a node = {mutable prev: 'a node option; mutable next: 'a node option;
                value: 'a}

type 'a dlist = {mutable first: 'a node option; mutable last: 'a node option}

type t = (string * value Maps.t) list

let lookup = failwith "unimplemented"

let select = failwith "unimplemented"

let insert = failwith "unimplemented"

let insertAll = failwith "unimplemented"

let update = failwith "unimplemented"

let updateAll = failwith "unimplemented"

let delete = failwith "unimplemented"

let union = failwith "unimplemented"

let join = failwith "unimplemented"