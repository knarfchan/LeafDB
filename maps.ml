(*maps.ml*)
open Table

module Int = struct
  type t = VInt of int
  let compare = Pervasives.compare
end

module String = struct
  type t = VString of string
  let compare = Pervasives.compare
end

module Bool = struct
  type t = VBool of bool
  let compare = Pervasives.compare
end

module Float = struct
  type t = VFloat of float
  let compare = Pervasives.compare
end

module Date = struct
  type t = VDate of Table.date
  let compare d1 d2 : int = match d1, d2 with
    | VDate (y1,m1,d1), VDate (y2,m2,d2) -> (y1*365 + m1*12 + d1*30) -
                                            (y2*365 + m2*12 + d2*30)
    | _, _ -> failwith "Error comparing dates"
end

module IntMap    = Map.Make (Int)
module StringMap = Map.Make (String)
module BoolMap   = Map.Make (Bool)
module FloatMap  = Map.Make (Float)
module DateMap   = Map.Make (Date)

type t =
  | Smap of 'a StringMap.t
  | Bmap of 'a BoolMap.t
  | Imap of 'a IntMap.t
  | Fmap of 'a FloatMap.t
  | Dmap of 'a DateMap.t

let lookup x m = match m.t with
  | VInt _ -> IntMap.find x m
  | VString _ -> StringMap.find x
  | VBool _ -> BoolMap.find x m
  | VFloat _ -> FloatMap.find x m
  | VDate _ -> DateMap.find x m

let like_compare value element typ = failwith "Unimplemented"
  (*
  open Str in
  match typ with
  | LikeBegin -> string_match (regex (".*"^element) value)
  | LikeEnd -> string_match (regex (element^".*") value)
  | LikeSubstring -> string_match (regex (".*"^element^".*") value)
  | NotLikeBegin -> not (string_match (regex (".*"^element) value))
  | NotLikeEnd -> not (string_match (regex (element^".*") value))
  | NotLikeSubstring -> not (string_match (regex (".*"^element^".*") value))*)

let compare element value typ = failwith "Unimplemented"
  (*
  match typ with
  | Date.t -> Date.compare element value
  | _ -> Pervasives.compare element value*)

let does_satisfy condition value element typ =
  let var = compare element value typ in
  match condtion with
  | Gt -> var > 0
  | Lt -> var < 0
  | Eq -> var = 0
  | GtEq -> var > 0 || var = 0
  | LtEq -> var < 0 || var = 0
  | NotEq -> var <> 0
  | _ -> like_compare value element typ

let get = function
  | VInt i -> i
  | VString s -> s
  | VBool b -> b
  | VFloat f -> f
  | VDate d -> d

let select map condition value =
  match value with
  | VInt i -> IntMap.filter (fun key e -> does_satisfy condition i (get e) map.t) map
  | VString s -> StringMap.filter (fun key e -> does_satisfy condition s (get e) map.t) map
  | VBool b -> BoolMap.filter (fun key e -> does_satisfy condition b (get e) map.t) map
  | VFloat f -> FloatMap.filter (fun key e -> does_satisfy condition f (get e) map.t) map
  | VDate d -> DateMap.filter (fun key e -> does_satisfy condition d (get e) map.t) map

let insert x y m = match m.t with
  | VInt _ -> IntMap.add x y m
  | VString _ -> StringMap.add x y m
  | VBool _ -> BoolMap.add x y m
  | VFloat _ -> FloatMap.add x y m
  | VDate _ -> DateMap.find x m

let update = failwith "Unimplemented"

let delete x m = match m.t with
  | VInt _ -> IntMap.remove x m
  | VString _ -> StringMap.remove x m
  | VBool _ -> BoolMap.remove x m
  | VFloat _ -> FloatMap.remove x m
  | VDate _ -> DateMap.remove x m
