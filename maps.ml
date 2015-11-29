(*maps.ml*)
open Types
open Str

module Int: Map.OrderedType with type t = int = struct
  type t = int
  let compare = Pervasives.compare
end

module String: Map.OrderedType with type t = string = struct
  type t = string
  let compare = Pervasives.compare
end

module Bool: Map.OrderedType with type t = bool = struct
  type t = bool
  let compare = Pervasives.compare
end

module Float: Map.OrderedType with type t = float = struct
  type t = float
  let compare = Pervasives.compare
end


module IntMap    = Map.Make (Int)
module StringMap = Map.Make (String)
module BoolMap   = Map.Make (Bool)
module FloatMap  = Map.Make (Float)


type t =
  | Smap of int StringMap.t
  | Bmap of int BoolMap.t
  | Imap of int IntMap.t
  | Fmap of int FloatMap.t

let lookup x m = match x, m with
  | VInt i, Imap map -> IntMap.find i map
  | VString s, Smap map -> StringMap.find s map
  | VBool b, Bmap map -> BoolMap.find b map
  | VFloat f, Fmap map -> FloatMap.find f map
  | _, _ -> failwith "Error"

let like_compare key comp condition =
  match condition with
  | LikeBegin -> string_match (regexp (".*"^key)) comp 0
  | LikeEnd -> string_match (regexp (key^".*")) comp 0
  | LikeSubstring -> string_match (regexp (".*"^key^".*")) comp 0
  | NotLikeBegin -> not (string_match (regexp (".*"^key)) comp 0)
  | NotLikeEnd -> not (string_match (regexp (key^".*")) comp 0)
  | NotLikeSubstring -> not (string_match (regexp (".*"^key^".*")) comp 0)
  | _ -> false


let does_satisfy condition comp key =
  let var = Pervasives.compare key comp in
  match condition with
  | Gt -> var > 0
  | Lt -> var < 0
  | Eq -> var = 0
  | GtEq -> var > 0 || var = 0
  | LtEq -> var < 0 || var = 0
  | NotEq -> var <> 0
  | _ -> false

let does_satisfy' condition comp key =
  let var = Pervasives.compare key comp in
  match condition with
  | Gt -> var > 0
  | Lt -> var < 0
  | Eq -> var = 0
  | GtEq -> var > 0 || var = 0
  | LtEq -> var < 0 || var = 0
  | NotEq -> var <> 0
  | _ -> like_compare key comp condition

let select map condition comp =
  match comp,map with
  | VInt i,Imap m -> Imap(IntMap.filter (fun key value -> does_satisfy condition i key) m)
  | VString s,Smap m  -> Smap(StringMap.filter (fun key value -> does_satisfy' condition s key) m)
  | VBool b,Bmap m -> Bmap(BoolMap.filter (fun key value -> does_satisfy condition b key) m)
  | VFloat f,Fmap m -> Fmap(FloatMap.filter (fun key value -> does_satisfy condition f key) m)
  | _ -> failwith "Error"

let insert x y m = match x, m with
  | VInt i, Imap map -> Imap(IntMap.add i y map)
  | VString s, Smap map -> Smap(StringMap.add s y map)
  | VBool b, Bmap map -> Bmap(BoolMap.add b y map)
  | VFloat f, Fmap map -> Fmap(FloatMap.add f y map)
  | _ -> failwith "Error"

let update map condition comp newv = failwith "Unimplemented"

let delete x m = match x, m with
  | VInt i, Imap map -> Imap(IntMap.remove i map)
  | VString s, Smap map -> Smap(StringMap.remove s map)
  | VBool b, Bmap map -> Bmap(BoolMap.remove b map)
  | VFloat f, Fmap map -> Fmap(FloatMap.remove f map)
  | _ -> failwith "Error"