open Pentomino_types
open Pentominoes

(*
 * Validating solutions.
 *)

(*
 * Solution criteria:
 *   -- no labels are repeated
 *   -- the union of all the piece locations covers the entire board
 *   -- no pieces overlap (no repeated piece locations)
 *)
let validate_solution board pieces is =
  (* Return true if the set covers all locations on the board. *)
  let covers board set =
    let rec iter row col =
      match () with
        | _ when row = board.nrows -> true
        | _ when col = board.ncols -> iter (row + 1) 0
        | _ when board.member (row, col) ->
          if LocSet.mem (row, col) set
            then iter row (col + 1)
            else false
        | _ -> iter row (col + 1) 
    in iter 0 0
  in
  let ps            = List.map (Array.get pieces) is in
  let locs          = List.map (fun p -> p.locs) ps in
  let all_locs      = List.fold_left LocSet.union LocSet.empty locs in
  let all_locs_list = List.concat (List.map LocSet.elements locs) in
  let labels        = List.map (fun p -> p.label) ps in
  let labelset      = CharSet.of_list labels in
    List.length labels = CharSet.cardinal labelset
      && List.length all_locs_list = LocSet.cardinal all_locs
      && covers board all_locs

(* Convert a solution to a string suitable for printing. *)
let string_of_solution board pieces is =
  (* Get the pieces which are part of the solution. *)
  let solution_pieces = List.map (Array.get pieces) is in
  (* Convert the solution pieces list to a map between locations and labels. *)
  let map =
    let convert_piece m p =
      let locs = LocSet.elements p.locs in
        List.fold_left (fun m l -> LocMap.add l p.label m) m locs
    in
      List.fold_left convert_piece LocMap.empty solution_pieces
  in
  let rec iter row col s =
    match () with
      | _ when row = board.nrows -> s
      | _ when col = board.ncols -> iter (row + 1) 0 (s ^ "\n")
      | _ when board.member (row, col) ->
        (* This should never fail (throw a Not_found exception). 
         * N.B. Char.escaped converts the label char to a 
         * one-character string (for all labels we use). *)
        let label = LocMap.find (row, col) map in
          iter row (col + 1) (s ^ Char.escaped label)
      | _ -> iter row (col + 1) (s ^ " ")
  in iter 0 0 ""

