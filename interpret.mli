(* interpret.mli *)
open Typs
open Ast

(* represents the result returned when trying to evaluate a table command
 * Table.t option is Some table when the command properly accesses a table
 *                       with table being the new table or query
 *                   None otherwise
 * bool is true when the command ran without error
 *         false when the command references something invalid *)
type evaluated = Table.t option * bool

(* represents the result returned when trying to evaluate a database command
 * Database.t option is Some db when the command properly accesses the database
 *                          with db being the db accessed
 *                      None otherwise
 * bool is true when the command ran without error
 *         false when the command references something invalid *)
type dbresult = Database.t option * string option * bool

(* precondition: none
 * postcondition: return an evaluated based on the expr run *)
val eval : Database.t -> expr -> evaluated

(* precondition: none
 * postcondition: return an evaluated based on the expr run *)
val eval_dbms : Dbms.t -> expr -> dbresult