open Pentomino_types

(* 3x3 rectangle, missing some locations.
 * Big enough for one piece. *)
let board0 =
  { nrows = 3;
    ncols = 3;
    member = fun loc -> not (List.mem loc [(0, 0); (0, 2); (2, 0); (2, 2)]) }

(* 2x5 rectangle. *)
let board1 =
  { nrows = 2;
    ncols = 5;
    member = fun _ -> true }

(* 3x4 rectangle, missing two locations. *)
let board2 =
  { nrows = 3;
    ncols = 4;
    member = fun loc -> not (List.mem loc [(0, 3); (2, 3)]) }

(* 3x5 rectangle. *)
let board3 =
  { nrows = 3;
    ncols = 5;
    member = fun _ -> true }

(* 4x5 rectangle. *)
let board4 =
  { nrows = 4;
    ncols = 5;
    member = fun _ -> true }

(* 5x12 rectangle. *)
let board5 =
  { nrows = 5;
    ncols = 12;
    member = fun _ -> true }

(* 6x10 rectangle. *)
let board6 =
  { nrows = 6;
    ncols = 10;
    member = fun _ -> true }

(* 4x15 rectangle. *)
let board7 =
  { nrows = 4;
    ncols = 15;
    member = fun _ -> true }
  
(* 3x20 rectangle. *)
let board8 =
  { nrows = 3;
    ncols = 20;
    member = fun _ -> true }
  
let noneOf item lst = not (List.mem item lst)

(* 8x8 square with the center four locations removed. *)
let board9 =
  { nrows = 8;
    ncols = 8;
    member = fun loc -> noneOf loc [(3, 3); (3, 4); (4, 3); (4, 4)] }

(* 8x8 square with the four corner locations removed. *)
let board10 =
  { nrows = 8;
    ncols = 8;
    member = fun loc -> noneOf loc [(0, 0); (0, 7); (7, 0); (7, 7)] }

(* 8x8 square with four internal locations removed. *)
let board11 =
  { nrows = 8;
    ncols = 8;
    member = fun loc -> noneOf loc [(2, 2); (2, 5); (5, 2); (5, 5)] }

(* 8x8 square with four internal locations removed. *)
let board12 =
  { nrows = 8;
    ncols = 8;
    member = fun loc -> noneOf loc [(1, 1); (1, 6); (6, 1); (6, 6)] }

