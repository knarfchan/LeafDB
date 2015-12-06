open Typs
open Ast
open Assertions
open Maps
open Table

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
  | CreateTable(str, cdl) -> let new_t = Table.create cdl in
                             if(Database.add_table db str new_t)
                             then (Some new_t, true)
                             else (None, false)
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


(*
TEST_MODULE "eval tests 1" = struct

  let leafDB = Dbms.create ()

  TEST_UNIT "make db" = eval_dbms leafDB (CreateDb ("db")) === (None, None, true)

  let db = Database.create ()
  let add_db = Dbms.add_database leafDB "db"

  TEST_UNIT "make same db" = eval_dbms leafDB (CreateDb ("db")) === (None, None, false)
  TEST_UNIT "drop db" = eval_dbms leafDB (DropDb ("db")) === (None, None, true)

  let drop_db = Dbms.drop leafDB "db"

  TEST_UNIT "drop db again" = eval_dbms leafDB (DropDb ("db")) === (None, None, false)
  TEST_UNIT "use 2" = eval_dbms leafDB (Use ("db2")) === (None, Some "db2", false)

  let add_db1 = Dbms.add_database leafDB "db1"
  let add_db2 = Dbms.add_database leafDB "db2"
  let add_db3 = Dbms.add_database leafDB "db3"
  let db2 = match Dbms.use leafDB "db2" with Some d -> d | None -> failwith "never reached"

  TEST_UNIT "use not exist" = eval_dbms leafDB (Use ("db4")) === (None, Some "db4", false)
  TEST_UNIT "use exist" = eval_dbms leafDB (Use ("db2")) === (Some db2, Some "db2", true)

  TEST_UNIT "create t" = eval db2 (CreateTable("t",["name", VString ""])) === (None, true)

  let t1 = Table.create [("name",VString "");("age",VInt 0);("height",VFloat 0.)]
  let db21 = Database.add_table db2 "t1" t1

  TEST_UNIT "add t" = Database.lookup db2 "t1" === Some t1
  TEST_UNIT "drop t1" = eval db2 (DropTable ("t1")) === (None, true)

  let drop_t1 = eval db2 (DropTable ("t1"))
  let drop_t2 = eval db2 (DropTable ("t2"))

  TEST_UNIT "drop t1" = Database.lookup db2 "t1" === None
  TEST_UNIT "drop t2" = Database.lookup db2 "t2" === None

  let tb1 = Table.create [("name",VString "");("age",VInt 0);("height",VFloat 0.)]
  let tb2 = Table.create [("integer",VInt 0);("decimal",VFloat 0.)]

  TEST_UNIT "create t1" = eval db2 (CreateTable("t1",[("name",VString "");("age",VInt 0);("height",VFloat 0.)])) === (None, true)
  TEST_UNIT "create t2" = eval db2 (CreateTable("t2",[("integer",VInt 0);("decimal",VFloat 0.)])) === (None, true)

  let db22 = Database.add_table db2 "t1" tb1
  let db23 = Database.add_table db2 "t2" tb2

  TEST_UNIT "add t1" = Database.lookup db2 "t1" === Some tb1
  TEST_UNIT "add t2" = Database.lookup db2 "t2" === Some tb2

  let ins1 = eval db2 (Insert ("t1", ["name"; "age"; "height"], [VString("erin"); VInt(19); VFloat(5.8)]))
  let tb12 = match ins1 with
              | Some t,_ -> t
              | _ -> failwith "never reached"

  let ins2 = eval db2 (InsertAll ("t2", [VInt(19); VFloat(7.8)]))
  let tb22 = match ins2 with
            | Some t,_ -> t
            | _ -> failwith "never reached"

  TEST_UNIT "size1" = Table.get_size tb12 === 1
  TEST_UNIT "size2" = Table.get_size tb22 === 1

  let del1 = eval db2 (Delete("t2", (Condition ("decimal", Eq, VFloat 7.8))))
  let tb23 = match del1 with
              | Some t, _ -> t
              | _ -> failwith "never reached"

  TEST_UNIT "size3" = Table.get_size tb23 === 0

  let ins3 = eval db2 (Insert("t2", ["integer"], [VInt(19)]))
  let tb24 = match ins3 with
              | Some t, _ -> t
              | _ -> failwith "never reached"

  TEST_UNIT "size4" = Table.get_size tb24 === 1

  (*UPDATING A VNULL DOESN'T WORK YET
  let up1 = eval db2 (Update ("t2", ["decimal",VFloat 1.2], Condition ("integer",Eq, VInt 19)))
  let tb25 = match up1 with
              | Some t, _ -> t
              | _ -> failwith "never reached"

  let check = Maps.lookup 1 (Table.get_one_map "decimal" tb25)

  TEST_UNIT "up1" = check === VFloat 1.2*)

  (*let up2 = eval db2 (Update ("t1", ["name",VString "annie"], Condition ("age",Eq,VInt 19)))

  let tb13 = match up2 with
              | Some t, _ -> t
              | _ -> failwith "never reached"
  let rowid = match (Maps.get_rows (Table.get_one_map "name" tb13)) with
              | h::t -> h
              | [] -> 0
  let print lst = match rowid with
                  | h::t -> Printf.printf "%d" h; print t
                  | [] -> Printf.printf ""
  TEST_UNIT "up2" = Maps.lookup rowid (Table.get_one_map "name" tb13) === VString "annie"*)

end

TEST_MODULE "eval tests 2" = struct

  let leafDB = Dbms.create ()

  TEST_UNIT "make db" = eval_dbms leafDB (CreateDb ("db")) === (None, None, true)

  let db = Database.create ()
  let add_db = Dbms.add_database leafDB "db"

  let tbl = Table.to_table [("Name", Maps.create (VString "")); ("Age", Maps.create (VInt 0));
             ("Height", Maps.create (VFloat 0.0))]

  let tbl' = Table.insert tbl ["Name"; "Age"; "Height"] [VString "Annie"; VInt 19; VFloat 5.3]

  let tbl'' = Table.insert tbl' ["Name"; "Age"; "Height"] [VString "Erin"; VInt 19; VFloat 5.8]

  let tbl''' = Table.insert tbl'' ["Name"; "Height"] [VString "Frank"; VFloat 6.0]

  let tibble = Table.to_table [("Name", Maps.create (VString "")); ("Hair Color", Maps.create (VString ""));
                ("Male?", Maps.create (VBool true))]

  let tibble' = Table.insert tibble ["Name";"Hair Color"; "Male?"] [VString "Louis"; VString "Black"; VBool true]

  let tibble'' = Table.insert tibble' ["Name";"Hair Color"; "Male?"] [VString "Frank"; VString "Black"; VBool true]

  let tibble''' = Table.insert tibble'' ["Name";"Hair Color"; "Male?"] [VString "Erin"; VString "Brown/Black"; VBool false]

  let _ = Table.print_tbl tbl'''
  let _ = Table.print_tbl tibble'''

  let add1 = Database.add_table db "tbl" tbl'''
  let add2 = Database.add_table db "tibble" tibble'''

  let s1 = eval db (Select(["Name"; "Age"], "tbl", Condition ("Height",Gt,VFloat 5.)))
  let s2 = eval db (SelectAll("tibble", Null))

  let sel1 = match s1 with
            | Some t, _ -> t
            | _ -> failwith "never reached"
  let sel2 = match s2 with
            | Some t, _ -> t
            | _ -> failwith "never reached"



<<<<<<< HEAD
end*)

