module CharSet = Set.Make(struct 
    type t = char 
    let compare = compare 
end)

type loc = int * int

module LocSet = Set.Make(struct
  type t = loc
  let compare = compare
end)

type piece = { label : char ; locs : LocSet.t }

let piece_compare p1 p2 =
  let c = compare p1.label p2.label in
    if c = 0 
      then LocSet.compare p1.locs p2.locs
      else c

module PieceSet = Set.Make(struct
  type t = piece
  let compare = piece_compare
end)

type board = 
  { 
    nrows  : int ; 
    ncols  : int ; 
    member : loc -> bool   (* is a location part of this board? *)
  }

type pconstraint =
  | Loc of loc
  | Label of char

(* Map using pconstraints as keys. *)
module PconstraintMap = Map.Make(struct
  type t = pconstraint
  let compare = compare
end)

(* Map using locs as keys. *)
module LocMap = Map.Make(struct
  type t = loc
  let compare = compare
end)

