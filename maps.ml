open table

(* maps.ml *)
module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module String = struct
  type t = string
  let compare = Pervasives.compare
end

module Bool = struct
  type t = bool
  let compare = Pervasives.compare
end

module Float = struct
  type t = float
  let compare = Pervasives.compare
end

module Date : Map.OrderedType = struct
  type t = Core.Date.t
  let compare = Core.Date.compare
end

module IntMap    = Map.Make (Int)
module StringMap = Map.Make (String)
module BoolMap   = Map.Make (Bool)
module FloatMap  = Map.Make (Float)
module DateMap   = Map.Make (Date)

let lookup map k = failwith "Unimplemeted"

let like_compare value element typ =
  open Str in
  match typ with
  | LikeBegin -> string_match (regex (".*"^element) value)
  | LikeEnd -> string_match (regex (element^".*") value)
  | LikeSubstring -> string_match (regex (".*"^element^".*") value)
  | NotLikeBegin -> not (string_match (regex (".*"^element) value))
  | NotLikeEnd -> not (string_match (regex (element^".*") value))
  | NotLikeSubstring -> not (string_match (regex (".*"^element^".*") value))

let compare element value typ =
  match typ with
  | Date.t -> Date.compare element value
  | _ -> Pervasives.compare element value

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

let insert (lst: value list) = failwith "Unimplemented"

let update (w: where) (v: value) = failwith "Unimplemented"

let delete (lst: value list) = failwith "Unimplemented"

let join = failwith "Unimplemented"
