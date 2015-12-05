(*maps.ml*)
open Typs
open Str
open Assertions

module Maps = struct

let pair_compare (a,a') (b,b') = if a' = b' then Pervasives.compare a b
                                 else Pervasives.compare a' b'

module Int: Map.OrderedType with type t = (int*int) = struct
  type t = (int*int)
  let compare = pair_compare
end

module String: Map.OrderedType with type t = (int*string) = struct
  type t = (int*string)
  let compare = pair_compare
end

module Bool: Map.OrderedType with type t = (int*bool) = struct
  type t = (int*bool)
  let compare = pair_compare
end

module Float: Map.OrderedType with type t = (int*float) = struct
  type t = (int*float)
  let compare = pair_compare
end


module IntMap    = Map.Make (Int)
module StringMap = Map.Make (String)
module BoolMap   = Map.Make (Bool)
module FloatMap  = Map.Make (Float)

(* Abstraction function:
   The map containing the keys (row,value) and values (row) represent the
   a column in a table where row is the id of a certain value and value is the
   data stored in the column
*)

type t =
  | Smap of int StringMap.t
  | Bmap of int BoolMap.t
  | Imap of int IntMap.t
  | Fmap of int FloatMap.t

(* [to_string map] returns a byte list containing the "key*value" of every
   item map *)
let to_string (map:t) : bytes list =
  match map with
  | Imap m ->
     IntMap.fold
       (fun (r,v) a acc -> ((string_of_int r)^"*"^(string_of_int v))::acc)
       m []
  | Smap m ->
     StringMap.fold
       (fun (r,v) a acc -> ((string_of_int r)^"*"^v)::acc)
       m []
  | Bmap m ->
     BoolMap.fold
       (fun (r,v) a acc -> ((string_of_int r)^"*"^(string_of_bool v))::acc)
       m []
  | Fmap m ->
     FloatMap.fold
       (fun (r,v) a acc -> ((string_of_int r)^"*"^(string_of_float v))::acc)
       m []

(* [build_col col map] returns a bytes list representation of a table column*)
let build_col (col:string) (map:t) : bytes list =
  match map with
  | Imap m -> col::"VInt"::(to_string map)
  | Bmap m -> col::"VBool"::(to_string map)
  | Fmap m -> col::"VFloat"::(to_string map)
  | Smap m -> col::"VString"::(to_string map)

(*precondition: r is a unique key *in* the map
  postcondition: returns the value associated with the row key r*)
let lookup (r:int) (m:t) = match m with
  | Fmap map ->
    VFloat(snd (fst (FloatMap.choose(FloatMap.filter(fun (row,v) value -> row = r) map))))
  | Smap map ->
    VString(snd (fst (StringMap.choose(StringMap.filter (fun (row,v) value -> row = r) map))))
  | Imap map ->
    VInt(snd (fst (IntMap.choose(IntMap.filter (fun (row,v) value -> row = r) map))))
  | Bmap map ->
    VBool(snd (fst (BoolMap.choose(BoolMap.filter(fun (row,v) value -> row = r) map))))

(*[joiner value map] returns *)
let joiner (vu:value) (m:t) : int  = match vu,m with
  | VFloat v, Fmap map ->
    fst (fst (FloatMap.choose(FloatMap.filter(fun (row,v') value -> v = v') map)))
  | VString v,Smap map ->
    fst (fst (StringMap.choose(StringMap.filter (fun (row,v') value -> v = v') map)))
  | VInt v, Imap map ->
    fst (fst (IntMap.choose(IntMap.filter (fun (row,v') value -> v = v') map)))
  | VBool v, Bmap map ->
    fst (fst (BoolMap.choose(BoolMap.filter(fun (row,v') value -> v = v') map)))
  | _ -> raise (Failure "Unable to join tables")

(* [has_value val map] returns true IFF a key with (_,value) exist in map*)
let has_value (vu:value) (map:t) : bool =
  match vu,map with
  | VInt v, Imap m ->
    not (IntMap.is_empty (IntMap.filter(fun (row,v') value -> v = v') m))
  | VString v, Smap m ->
    not (StringMap.is_empty (StringMap.filter(fun (row,v') value -> v = v') m))
  | VBool v, Bmap m ->
    not (BoolMap.is_empty (BoolMap.filter(fun (row,v') value -> v = v') m))
  | VFloat v, Fmap m ->
    not (FloatMap.is_empty (FloatMap.filter(fun (row,v') value -> v = v') m))
  | _ -> raise (Failure "Value is not found in column")

(* [is_member row map] returns true IFF a key with (row,_) exist in map*)
let is_member r map = match map with
  | Imap m ->
    not (IntMap.is_empty(IntMap.filter(fun (row,v) value -> row = r) m))
  | Smap m ->
    not (StringMap.is_empty(StringMap.filter(fun (row,v) value -> row = r) m))
  | Bmap m ->
    not (BoolMap.is_empty(BoolMap.filter(fun (row,v) value -> row = r) m))
  | Fmap m ->
    not (FloatMap.is_empty(FloatMap.filter(fun (row,v) value -> row = r) m))

(* [get_rows map] returns a list [r1,r2...r3] that contains all they
   row keys in map *)
let get_rows map = match map with
  | Imap m -> IntMap.fold (fun (r,v) a b -> a::b) m []
  | Smap m -> StringMap.fold (fun (r,v) a b -> a::b) m []
  | Bmap m -> BoolMap.fold (fun (r,v) a b -> a::b) m []
  | Fmap m -> FloatMap.fold (fun (r,v) a b -> a::b) m []

(* [empty map] returns an empty Maps.t with the same type as map *)
let empty map =
  match map with
  | Smap _ -> Smap (StringMap.empty)
  | Bmap _ -> Bmap (BoolMap.empty)
  | Imap _ -> Imap (IntMap.empty)
  | Fmap _ -> Fmap (FloatMap.empty)

(* [create value] creates an empty map based on the value type *)
let create (v : value) =
  match v with
  | VInt _ -> Imap (IntMap.empty)
  | VString _ -> Smap (StringMap.empty)
  | VFloat _ -> Fmap (FloatMap.empty)
  | VBool _ -> Bmap (BoolMap.empty)
  | _ -> raise (Failure "Error: Changes to column may not be made")

(* [like_compare comp key condition] returns 0 if condition
   is satisfied by comparing key to comp *)
let like_compare comp key condition =
  match condition with
  | LikeBegin -> string_match (regexp (key^".*")) comp 0
  | LikeEnd -> string_match (regexp (".*"^key^"$")) comp 0
  | LikeSubstring -> string_match (regexp (".*"^key^".*")) comp 0
  | NotLikeBegin -> not (string_match (regexp (key^".*")) comp 0)
  | NotLikeEnd -> not (string_match (regexp (".*"^key^"$")) comp 0)
  | NotLikeSubstring -> not (string_match (regexp (".*"^key^".*")) comp 0)
  | _ -> false

(* [size map] returns the cardinality of map *)
let size (map: t) : int = match map with
  | Smap m -> StringMap.cardinal m
  | Bmap m -> BoolMap.cardinal m
  | Imap m -> IntMap.cardinal m
  | Fmap m -> FloatMap.cardinal m


let rec get_longest (map_list:t list) (lsize:int) (lmap:t) : t =
  match map_list with
  | [] -> lmap
  | (Smap s)::t ->
     if (StringMap.cardinal s) > lsize then
       get_longest t (StringMap.cardinal s) (Smap s)
     else get_longest t lsize lmap
  | (Bmap b)::t ->
     if (BoolMap.cardinal b) > lsize then
       get_longest t (BoolMap.cardinal b) (Bmap b)
     else get_longest t lsize lmap
  | (Imap i)::t ->
     if (IntMap.cardinal i) > lsize then
       get_longest t (IntMap.cardinal i) (Imap i)
     else get_longest t lsize lmap
  | (Fmap f)::t ->
     if (FloatMap.cardinal f) > lsize then
       get_longest t (FloatMap.cardinal f) (Fmap f)
     else get_longest t lsize lmap

(* [does_stasify condition comp (row,key)] returns 0 if
   the condition is satisfied when comparing the key to comp *)
let does_satisfy condition comp (c,key) =
  let var = Pervasives.compare key comp in
  match condition with
  | Gt -> var > 0
  | Lt -> var < 0
  | Eq -> var = 0
  | GtEq -> var > 0 || var = 0
  | LtEq -> var < 0 || var = 0
  | NotEq -> var <> 0
  | _ -> raise (Failure "Incorrect compare operator")

(* [does_stasify condition comp (row,key)] returns 0 if
   the condition is satisfied when comparing the key to comp.
   Key is a string*)
let does_satisfy' condition comp (c,key) =
  let var = Pervasives.compare key comp in
  match condition with
  | Gt -> var > 0
  | Lt -> var < 0
  | Eq -> var = 0
  | GtEq -> var > 0 || var = 0
  | LtEq -> var < 0 || var = 0
  | NotEq -> var <> 0
  | _ -> like_compare key comp condition

(* [select map condition comp] returns a Maps.t that is a subset of
   map which all they keys satisfy the condition when compared to
   comp *)
let select map condition comp =
  match comp,map with
  | VInt i,Imap m ->
     Imap(IntMap.filter (fun key value -> does_satisfy condition i key) m)
  | VString s,Smap m  ->
     Smap(StringMap.filter (fun key value -> does_satisfy' condition s key) m)
  | VBool b,Bmap m ->
     Bmap(BoolMap.filter (fun key value -> does_satisfy condition b key) m)
  | VFloat f,Fmap m ->
     Fmap(FloatMap.filter (fun key value -> does_satisfy condition f key) m)
  | _ -> raise (Failure "Incompatable value and column types while selecting")

(* [insert value row map] adds an item with key = (row*value) and value (row) to
   map *)
let insert value row m = match value, m with
  | VInt i, Imap map -> Imap(IntMap.add (row,i) row map)
  | VString s, Smap map -> Smap(StringMap.add (row,s) row map)
  | VBool b, Bmap map -> Bmap(BoolMap.add (row,b) row map)
  | VFloat f, Fmap map -> Fmap(FloatMap.add (row,f) row map)
  | _ -> raise (Failure "Incompatable value and column types while inserting")

(* [update map newv] replaces each item in map with the value newv*)
let update (map:t)(newv:value) =
  match map,newv with
  | Imap m, VInt i' ->
     Imap(IntMap.fold
            (fun (c,k) a map ->
             IntMap.add (c,i')(c)(IntMap.remove (c,k) map))
            m m)
  | Smap m, VString s'->
     Smap(StringMap.fold
            (fun (c,k) a map ->
             StringMap.add (c,s')(c)(StringMap.remove (c,k) map))
            m m)
  | Bmap m, VBool b' ->
     Bmap(BoolMap.fold
            (fun (c,k) a map ->
             BoolMap.add (c,b')(c)(BoolMap.remove (c,k) map))
            m m)
  | Fmap m, VFloat f' ->
     Fmap(FloatMap.fold
            (fun (c,k) a map ->
             FloatMap.add (c,f')(c)(FloatMap.remove (c,k) map))
            m m)
  | _ -> raise (Failure "Incompatable value and column types while updating")


(*precondition: the key (r,_) is in the map m
  postcondition: the they key (r,_) is replaced with
  the (r,newv) in the map m *)
let replace' (row:int) (v:value) (map:t) =
  match map,v with
  | Imap m,VInt newv ->
     Imap(IntMap.fold
            (fun (r,v) a mp ->
             if r = row then
               (IntMap.add(r,newv) r (IntMap.remove (r,v) mp))
             else mp)
            m m)
  | Bmap m,VBool newv->
     Bmap(BoolMap.fold
            (fun (r,v) a mp ->
             if r = row then
               (BoolMap.add(r,newv) r  (BoolMap.remove (r,v) mp))
             else mp)
            m m)
  | Smap m,VString newv ->
     Smap(StringMap.fold
            (fun (r,v) a mp ->
             if r = row then
               (StringMap.add(r,newv) r (StringMap.remove (r,v) mp))
             else mp) m m)
  | Fmap m,VFloat newv ->
     Fmap(FloatMap.fold
            (fun (r,v) a mp ->
             if r = row then
               (FloatMap.add(r,newv) r (FloatMap.remove (r,v) mp))
             else mp)
            m m)
  | _ -> raise (Failure "Incompatable value and column types while updating")

(* [replace newm oldm] replaces all values in oldm with newm values for
   all occurances of matching rows*)
let replace (newm:t) (oldm':t) =
  match newm,oldm' with
  | Imap m, Imap oldm ->
     IntMap.fold
       (fun (c,k) a map ->
        if is_member c (Imap m)
        then (replace' c (VInt k) map) else map)
       oldm newm
  | Smap m, Smap oldm ->
     StringMap.fold
       (fun (c,k) a map ->
        if is_member c (Smap m)
        then (replace' c (VString k) map) else map)
       oldm newm
  | Bmap m, Bmap oldm ->
     BoolMap.fold
       (fun (c,k) a map ->
        if is_member c (Bmap m)
        then (replace' c (VBool k) map) else map)
       oldm newm
  | Fmap m, Fmap oldm ->
     FloatMap.fold
       (fun (c,k) a map ->
        if is_member c (Fmap m)
        then (replace' c (VFloat k) map) else map)
       oldm newm
  | _ -> raise (Failure "Incompatable value and column types while updating")

(* [joing m1 m2] returns the list of paired rows of each map
   having equal values*)
let join (m1:t) (m2:t) : (int*int) list =
  match m1,m2 with
  |Imap m,Imap m' ->
    (IntMap.fold
       (fun (r,v) a acc ->
        if (has_value (VInt v) m2) then
          (r, joiner (VInt v) m2)::acc
        else acc)
       m [])
  |Smap m,Smap m' ->
    (StringMap.fold
       (fun (r,v) a acc ->
        if (has_value (VString v) m2) then
          (r, joiner (VString v) m2)::acc
        else acc)
       m [])
  |Bmap m,Bmap m' ->
    (BoolMap.fold
       (fun (r,v) a acc ->
        if (has_value (VBool v) m2) then
          (r, joiner (VBool v) m2)::acc else
          acc)
       m [])
  |Fmap m,Fmap m' ->
    (FloatMap.fold
       (fun (r,v) a acc ->
        if (has_valuef(VFloat v) m2) then
          (r, joiner (VFloat v) m2)::acc else
          acc)
       m [])
  | _ -> raise (Failure "Incompatable value and column types while joining")

(* [delete ids map] removes each item in the map with
    rows that are in the list ids *)
let delete ids map = match map with
  | Imap m -> Imap(IntMap.filter (fun (r,v) a -> (not)(List.mem r ids)) m)
  | Smap m -> Smap(StringMap.filter (fun (r,v) a -> (not)(List.mem r ids)) m)
  | Bmap m -> Bmap(BoolMap.filter (fun (r,v) a -> (not)(List.mem r ids)) m)
  | Fmap m -> Fmap(FloatMap.filter (fun (r,v) a -> (not)(List.mem r ids)) m)

(* [get_type map] returns the value type of a map *)
let get_type map = match map with
  | Imap _ -> VInt 0
  | Smap _ -> VString ""
  | Bmap _ -> VBool false
  | Fmap _ -> VFloat 0.0



(* A NOTE ON TEST CASES: These test cases are deliberately commented out
   because the TEST_MODULE macro is not compatiable with our menhir build
   as explained in the README.txt. To compile and run the test cases individually
   one must uncomment the code and compile each module with the cs3110 compiler.*)

(*TEST_MODULE "TEST" = struct
  let imap = create (VInt 0)
  TEST "test_size and create" = (size imap = 0)
  TEST "test_empty" = (size (empty imap) = 0)

  let rec insert_tester (i:int) m =
    if i = 0 then m
    else insert (VInt i) i (insert_tester (i-1) m)

  let make_string (i:int) : string =
    (string_of_int i) ^ " words & letters ! " ^ (string_of_int i)

  let rec insert_tester_string (i:int) m =
    if i = 0 then m
    else insert (VString(make_string i)) i (insert_tester_string (i-1) m)

  let imap_hundred = insert_tester 100 imap
  (*represents the map containing (1,1) to (100,100)*)
  TEST "test_insert" = (size imap_hundred = 100)
  (* Getting all values greater than 50*)
  TEST "test_select_int" = (size (select imap_hundred Gt (VInt 50)) = 50)


  let smap = create (VString "")
  let smap_hundred = insert_tester_string 100 smap
  TEST "test_select_string" = (size (select smap_hundred LikeSubstring (VString "word")) = 100 )
  TEST "test_select_string" = (size (select smap_hundred LikeBegin (VString "1")) = 12 )
  TEST "test_select_string" = (size (select smap_hundred LikeEnd (VString "0")) = 10)


   let m = create (VInt 0)
   let m' = insert (VInt 5) 5 m
   let m'' = insert (VInt 8) 6 m'
   let m''' = insert (VInt 9) 7 m''

   let n = create (VInt 0)
   let n' = insert (VInt 9) 10 n
   let n'' = insert (VInt 8) 20 n'
   let n''' = insert (VInt 10) 30 n''

   let j = join (m''') (n''') === [(7,10); (6,20)]

  end *)
end
