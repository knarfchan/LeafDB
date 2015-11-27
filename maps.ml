(*maps.ml*)
open Query
open Core.Date


let compare_dates d1 d2 = 0

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

module Dates = struct
  type t = Core.Date.t
  let compare = compare_dates
end

let type_map (v: Query.value) = match v with
  | VInt _    -> Map.Make (Int)
  | VString _ -> Map.Make (String)
  | VBool _   -> Map.Make (Bool)
  | VFloat _  -> Map.Make (Float)
  | VDate _   -> Map.Make (Dates)
