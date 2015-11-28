open Table

(* type representing our database *)
type t = Table.t list

(* takes the name of the database and returns the database *)
let use str = failwith "unimplemented"

(* create a table in a database and return the database *)
let create cdl str = failwith "unimplemented"

(* takes a table, removes it from the database, and return the database *)
let drop tabl db  = failwith "unimplemented"

(* finds a table in the database based on its name *)
let lookup str  = failwith "unimplemented"