open Pentomino_pieces
open Pentomino_types
open Pentominoes
open Pentomino_utils
open Pentomino_boards
open Binmat
open Solver

module FindSolutions(B: BinaryMatrix) =
 struct
   module Alg = AlgorithmX(B)

   let get_solution piece_string_list board =
     let nrows = Array.length (make_piece_array piece_string_list board) in
     let ncols = Array.length (make_constraints_array piece_string_list board) in
     let coords_set = make_binary_matrix_locs piece_string_list board in
     let coords = LocSet.elements coords_set in
     let binmat = B.make nrows ncols coords in
       Alg.solve binmat

   let show_solution piece_string_list board =
     match get_solution piece_string_list board with
       | None -> Printf.printf "No solution found.\n"
       | Some sol ->
           let pieces = make_piece_array piece_string_list board in
           let indices = IntSet.elements sol in
             if validate_solution board pieces indices
               then Printf.printf "%s" (string_of_solution board pieces indices)
               else Printf.printf "%s\n" "solution is not valid"
 end

module FS = FindSolutions(ImplBinaryMatrix)
open FS

let solve0 () = 
  let pieces = [('x', px_s)] in
    show_solution pieces board0

let solve1 () = 
  let pieces = [('l', pl_s); ('L', pl_s)] in
    show_solution pieces board1

let solve2 () = 
  let pieces = [('u', pu_s); ('x', px_s)] in
    show_solution pieces board2

let solve3 () =
  let pieces = [('l', pl_s); ('t', pt_s); ('y', py_s)] in
    show_solution pieces board3

let solve4 () =
  let pieces = [('f', pf_s); ('l', pl_s); ('p', pp_s); ('u', pu_s)] in
    show_solution pieces board4

let solve5  () = show_solution all_pieces board5
let solve6  () = show_solution all_pieces board6
let solve7  () = show_solution all_pieces board7
let solve8  () = show_solution all_pieces board8
let solve9  () = show_solution all_pieces board9
let solve10 () = show_solution all_pieces board10
let solve11 () = show_solution all_pieces board11
let solve12 () = show_solution all_pieces board12

let solve n =
  match n with
    |  0 -> solve0  ()
    |  1 -> solve1  ()
    |  2 -> solve2  ()
    |  3 -> solve3  ()
    |  4 -> solve4  ()
    |  5 -> solve5  ()
    |  6 -> solve6  ()
    |  7 -> solve7  ()
    |  8 -> solve8  ()
    |  9 -> solve9  ()
    | 10 -> solve10 ()
    | 11 -> solve11 ()
    | 12 -> solve12 ()
    |  _ -> failwith "invalid problem number"

let _ = 
  if Array.length (Sys.argv) <> 2
    then 
      begin
        Printf.fprintf stderr "usage: pentomino n\n";
        Printf.fprintf stderr "  (where n is between 0 and 12)\n";
        exit 1
      end
    else
      try
        let n = int_of_string (Sys.argv.(1)) in
          solve n
      with (Failure msg) ->
        Printf.fprintf stderr "ERROR: %s\n" msg



