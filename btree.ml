open Query
open Table

type key = Query.value

type branch =
  | Leaf of 'a
  | Two of branch * key * branch
  | Three of branch * key * branch * key * branch
  | Four of branch * key * branch * key * branch * key * branch

(*might need more kicked sizes*)
type kicked =
  | Up of branch * key * branch
  | Done of branch
(*might need more hole types*)
type hole =
  | Hole of key option * branch
  | Absorbed of key option * branch
(*might need more direction types*)
type direction2 =
  | Left2
  | Right2

type direction3 =
  | Left3
  | Mid3
  | Right3

let empty : branch = Leaf

let insert_upward_two (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (x_other: dict) : kicked =

let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =

let rec insert_downward (d: dict) (k: key) (v: value) : kicked =

and insert_downward_two ((k,v): pair) ((k1,v1): pair)(left: dict)(right: dict)
     : kicked =

and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =

let insert (d: dict) (k: key) (v: value) : dict =

let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =

let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =

let rec remove_downward (d: dict) (k: key) : hole =

and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =

and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =

and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =

and remove_min (d: dict) : hole =

let remove (d: dict) (k: key) : dict =

let rec lookup (d: dict) (k: key) : value option =

let rec member (d: dict) (k: key) : bool =

let choose (d: dict) : (key * value * dict) option =

let rec balanced_helper (d: dict) : bool * int=

let rec balanced (d : dict) : bool =