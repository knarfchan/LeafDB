(*maps.ml*)
open Types
open Str


module Int: Map.OrderedType with type t = (int*int) = struct
  type t = (int*int)
  let compare a b = 0
end

module String: Map.OrderedType with type t = (int*string) = struct
  type t = (int*string)
  let compare = Pervasives.compare
end

module Bool: Map.OrderedType with type t = (int*bool) = struct
  type t = (int*bool)
  let compare = Pervasives.compare
end

module Float: Map.OrderedType with type t = (int*float) = struct
  type t = (int*float)
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

(*let lookup x m = match x, m with
  | VInt i, Imap map -> IntMap.find i map
  | VString s, Smap map -> StringMap.find s map
  | VBool b, Bmap map -> BoolMap.find b map
  | VFloat f, Fmap map -> FloatMap.find f map
  | _, _ -> failwith "Error"*)

let like_compare key comp condition =
  match condition with
  | LikeBegin -> string_match (regexp (".*"^key)) comp 0
  | LikeEnd -> string_match (regexp (key^".*")) comp 0
  | LikeSubstring -> string_match (regexp (".*"^key^".*")) comp 0
  | NotLikeBegin -> not (string_match (regexp (".*"^key)) comp 0)
  | NotLikeEnd -> not (string_match (regexp (key^".*")) comp 0)
  | NotLikeSubstring -> not (string_match (regexp (".*"^key^".*")) comp 0)
  | _ -> false


let does_satisfy condition comp (c,key) =
  let var = Pervasives.compare key comp in
  match condition with
  | Gt -> var > 0
  | Lt -> var < 0
  | Eq -> var = 0
  | GtEq -> var > 0 || var = 0
  | LtEq -> var < 0 || var = 0
  | NotEq -> var <> 0
  | _ -> false

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


let select map condition comp =
  match comp,map with
  | VInt i,Imap m -> Imap(IntMap.filter (fun key value -> does_satisfy condition i key) m)
  | VString s,Smap m  -> Smap(StringMap.filter (fun key value -> does_satisfy' condition s key) m)
  | VBool b,Bmap m -> Bmap(BoolMap.filter (fun key value -> does_satisfy condition b key) m)
  | VFloat f,Fmap m -> Fmap(FloatMap.filter (fun key value -> does_satisfy condition f key) m)
  | _ -> failwith "Error"

let insert x y m = match x with
  | VInt i, Imap map -> Imap(IntMap.add (y,i) y map)
  | VString s, Smap map -> Smap(StringMap.add (y,s) y map)
  | VBool b, Bmap map -> Bmap(BoolMap.add (y,b) y map)
  | VFloat f, Fmap map -> Fmap(FloatMap.add (y,f) y map)
  | _ -> failwith "Error"

let update map condition comp newv =
  match comp,map,newv with
  | VInt i, Imap m, VInt i' ->
     Imap(IntMap.fold
            (fun (c,k) a map -> if (does_satisfy condition i (c,k)) then
                                  IntMap.add (c,i')(c)(IntMap.remove (c,k) map)
                                else map) m m)
  | VString s,Smap m, VString s'->
          Smap(StringMap.fold
            (fun (c,k) a map -> if (does_satisfy condition s (c,k)) then
                                  StringMap.add (c,s')(c)(StringMap.remove (c,k) map)
                                else map) m m)
  | VBool b, Bmap m, VBool b' ->
          Bmap(BoolMap.fold
            (fun (c,k) a map -> if (does_satisfy condition b (c,k)) then
                                  BoolMap.add (c,b')(c)(BoolMap.remove (c,k) map)
                                else map) m m)
  | VFloat f, Fmap m, VFloat f' ->
     Fmap(FloatMap.fold
            (fun (c,k) a map -> if (does_satisfy condition f (c,k)) then
                                  FloatMap.add (c,f')(c)(FloatMap.remove (c,k) map)
                                else map) m m)
  | _ -> failwith "Does not follow schema"

(*let delete x m = match x, m with
  | VInt i, Imap map -> Imap(IntMap.remove i map)
  | VString s, Smap map -> Smap(StringMap.remove s map)
  | VBool b, Bmap map -> Bmap(BoolMap.remove b map)
  | VFloat f, Fmap map -> Fmap(FloatMap.remove f map)
  | _ -> failwith "Error"*)

let delete map op v = match v,map with
  | VInt i,Imap m -> (Imap(IntMap.filter (fun key value -> (not)(does_satisfy op i key)) m))
  | VString s,Smap m  -> (Smap(StringMap.filter (fun key value -> (not)(does_satisfy' op s key)) m))
  | VBool b,Bmap m -> (Bmap(BoolMap.filter (fun key value -> (not)(does_satisfy op b key)) m))
  | VFloat f,Fmap m -> (Fmap(FloatMap.filter (fun key value -> (not)(does_satisfy op f key)) m))
  | _ -> failwith "Error"