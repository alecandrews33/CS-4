(** (Row, column) location on a board. *)
type loc = int * int

(** Sets of characters. *)
module CharSet : Set.S with type elt = char

(** Sets of locations. *)
module LocSet : Set.S with type elt = loc

(** Pentomino pieces. *)
type piece = { label : char; locs : LocSet.t; }

(** Sets of pentomino pieces. *)
val piece_compare : piece -> piece -> int
module PieceSet : Set.S with type elt = piece

(** Board on which pieces are placed. *)
type board = { nrows : int; ncols : int; member : loc -> bool; }

(** Pentomino problem constraints.
 *  Each solution must occupy each location on the board exactly once
 *  and have a piece of each label represented exactly once. *)
type pconstraint = Loc of loc | Label of char

(** Map using pconstraints as keys. *)
module PconstraintMap : Map.S with type key = pconstraint

(** Map using locs as keys. *)
module LocMap : Map.S with type key = loc
