(*maps.ml*)
open Query

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

type date = int * int * int (* (year, month, day) *)

module Date : Map.OrderedType = struct
  type t = date
  let compare = Pervasives.compare
end

module IntMap    = Map.Make (Int)
module StringMap = Map.Make (String)
module BoolMap   = Map.Make (Bool)
module FloatMap  = Map.Make (Float)
module DateMap   = Map.Make(Date)

let lookup = failwith "Unimplemeted"

let select (w: where) = failwith "Unimplemented"

let insert (lst: value list) = failwith "Unimplemented"

let update (w: where) (v: value) = failwith "Unimplemented"

let updateAll (lst: value list) = failwith "Unimplemented"

let delete (lst: value list) = failwith "Unimplemented"

let join = failwith "Unimplemented"