open Typs
open Ast
open Assertions

type evaluated = Table.t option * bool
type dbresult = (Database.t option) * string option * bool

let attempt_op db tbl op =
  match Database.lookup db tbl with
  | None -> (None, false)
  | Some x -> (Some (op x), true)

let attempt_join p1 p2 o =
  match p1, p2 with
  | Some x, Some y -> (Some (Table.join x y o), true)
  | _ -> (None, false)

let eval_select (db: Database.t) (e: expr) : Table.t option =
  match e with
  | Select (lst, tbl, w) ->
      (match Database.lookup db tbl with
        | None -> None
        | Some x -> Some(Table.select lst x w))
  | SelectAll (tbl, w) ->
      (match Database.lookup db tbl with
        | None -> None
        | Some x -> Some(Table.selectAll x w))
  | _ -> None

let eval (db : Database.t) (e : expr): evaluated =
  match e with
  | Select (lst, tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.select lst x w)
  | SelectAll (tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.selectAll x w)
  | Insert (tbl, clst, vlst) ->
      attempt_op(db)(tbl)(fun x -> Table.insert x clst vlst)
  | InsertAll (tbl, vlst) ->
      (attempt_op(db)(tbl)(fun x -> Table.insertAll x (List.rev vlst)))
  | Delete (tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.delete x w)
  | Update (tbl, cvlst, w) ->
      attempt_op(db)(tbl)(fun x -> Table.update x cvlst w)
  | JoinTables (str1, str2, o) ->
      attempt_join (Database.lookup db str1) (Database.lookup db str2) o
  | JoinTabQuer (str, e, o) ->
      attempt_join (Database.lookup db str) (eval_select db e) o
  | JoinQuerTab (e, str, o) ->
      attempt_join (eval_select db e) (Database.lookup db str) o
  | JoinQueries (e1, e2, o) -> attempt_join (eval_select db e1) (eval_select db e2) o
  | CreateTable(str, cdl) -> (None, Database.add_table db str (Table.create cdl))
  | DropTable(str) -> (None, Database.drop db str)
  | _ -> (None, false)

let eval_dbms (dbs : Dbms.t) (e) : dbresult =
  match e with
  | CreateDb(str) -> (None, None, Dbms.add_database dbs str)
  | DropDb(str) -> (None, None, Dbms.drop dbs str)
  | Use(str) -> (match Dbms.use dbs str with
                | Some d -> (Some d, Some str, true)
                | None -> (None, Some str, false))
                (*(Dbms.use dbs str, Some str, true)*)
  | ExitDb -> exit 0
  | _ -> (None, None, false)

(*type evaluated = Table.t option * bool
type dbresult = (Database.t option) * string option * bool*)

(*TEST_MODULE "eval tests" = struct

  let leafDB = Dbms.create ()

  TEST_UNIT "make db" = eval_dbms leafDB (CreateDb ("db")) === (None, None, true)

  let db = Database.create ()
  let add_db = Dbms.add_database leafDB "db"

  TEST_UNIT "make same db" = eval_dbms leafDB (CreateDb ("db")) === (None, None, false)
  TEST_UNIT "drop db" = eval_dbms leafDB (DropDb ("db")) === (None, None, true)

  let drop_db = Dbms.drop leafDB "db"

  TEST_UNIT "drop db again" = eval_dbms leafDB (DropDb ("db")) === (None, None, false)
  TEST_UNIT "use 2" = eval_dbms leafDB (Use ("db2")) === (None , Some "db2" , false)

  let add_db1 = Dbms.add_database leafDB "db1"
  let add_db2 = Dbms.add_database leafDB "db2"
  let add_db3 = Dbms.add_database leafDB "db3"
  let db2 = match Dbms.use leafDB "db2" with Some d -> d | None -> failwith "never reached"

  TEST_UNIT "use not exist" = eval_dbms leafDB (Use ("db4")) === (None, Some "db4", false)
  TEST_UNIT "use exist" = eval_dbms leafDB (Use ("db2")) === (Some db2 , Some "db2", true)

  TEST_UNIT "create t" = eval db2 (CreateTable("t",["name", VString ""])) === (None, true)

  let t1 = Table.create [("name",VString "");("age",VInt 0);("height",VFloat 0.)]
  let db21 = Database.add_table db2 "t1" t1

  TEST_UNIT "add t" = Database.lookup db2 "t1" === Some t1
  TEST_UNIT "drop t1" = eval db2 (DropTable ("t1")) === (None, true)

  let drop_t = eval db2 (DropTable ("t1"))

  TEST_UNIT "drop t2" = Database.lookup db2 "t1" === None

  let tb1 = Table.create [("name",VString "");("age",VInt 0);("height",VFloat 0.)]
  let tb2 = Table.create [("town",VString "");("yrs",VInt 0);("sleep",VBool true)]
  let tb3 = Table.create [("integer",VInt 0);("decimal",VFloat 0.)]

  TEST_UNIT "create t1" = eval db2 (CreateTable("t1",[("name",VString "");("age",VInt 0);("height",VFloat 0.)])) === (None, true)
  TEST_UNIT "create t2" = eval db2 (CreateTable("t2",[("town",VString "");("yrs",VInt 0);("sleep",VBool true)])) === (None, true)
  TEST_UNIT "create t3" = eval db2 (CreateTable("t3",[("integer",VInt 0);("decimal",VFloat 0.)])) === (None, true)

  let db22 = Database.add_table db2 "t1" t1
  let db23 = Database.add_table db2 "t2" t2
  let db24 = Database.add_table db2 "t3" t3

  TEST_UNIT "add t1" = Database.lookup db2 "t1" === Some t1
  TEST_UNIT "add t2" = Database.lookup db2 "t2" === Some t2
  TEST_UNIT "add t3" = Database.lookup db2 "t3" === Some t3

    | Select (lst, tbl, w) ->
      attempt_op(db)(tbl)(fun x -> Table.select lst x w)

  let s1 = eval db2 (Select ())

end*)

(*JoinQueries(SelectAll(t1, Null), SelectAll(t2, Null), (name, word))
 --------------------------------
 |  name |  age |  sleep |  age |
 --------------------------------
 |   "b" |    3 |   true |    3 |
 |   "c" |    4 |   true |    4 |
 --------------------------------
LeafDB>d>select * from t2
SelectAll(t2, Null)
 -------------------------
 |  word |  sleep |  age |
 -------------------------
 |   "b" |   true |    3 |
 |   "b" |  false |    4 |
 |   "c" |   true |    2 |
 -------------------------
LeafDB>d>select * from t1 join select * from t2 on age = age
JoinQueries(SelectAll(t1, Null), SelectAll(t2, Null), (age, age))
 ---------------------------------
 |  name |  age |  word |  sleep |
 ---------------------------------
 |   "a" |    2 |   "c" |   true |
 |   "b" |    3 |   "b" |   true |
 |   "c" |    4 |   "b" |  false |
*)