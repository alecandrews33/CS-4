(* Student name: Alec Andrews *)
(* CMS cluster login name: avandrew *)
open Pentomino_types

(*
 * Pieces.
 *)

let make_piece (lbl, ls) =
  { label = lbl; locs = LocSet.of_list ls }

let make_piece_from_string (label, s) =
  let st     = String.trim s in
  let lines  = String.split_on_char '\n' st in
  let coords = ref LocSet.empty in
    begin
      List.iteri
        (fun i s ->
           String.iteri 
             (fun j c -> 
                if c = 'X' then 
                   coords := LocSet.add (i, j) !coords)
             s)
        lines;
      { label; locs = !coords }
    end

let show_piece { label; locs } =
  Printf.printf "LABEL: '%c'; " label;
  Printf.printf "LOCS: ";
  LocSet.iter (fun (i, j) -> Printf.printf "(%d, %d) " i j) locs;
  print_newline ()

let translate_piece { label; locs } drow dcol = 
  {label = label; 
  (* Simple redefinition of the given piece. Adjust all locations
  by adding the change in row and change in column *)
  locs = (LocSet.map (fun (x, y) -> (x + drow, y + dcol)) locs)}
 
let normalize_piece { label; locs } = 
  (* This recursive helper function will iterate through all of the 
  locations to determine what the minimum row and minimum column are *)
  let rec iter elements min_row min_col = 
  match elements with 
  (* Stopping condition, return tuple of minimums. Negative because we 
  want these values to be 0 in our new piece.  *)
  | [] -> ((-min_row), (-min_col))
  | h :: t -> 
  begin
    (* Redefine minimums if we have found something smaller *)
  match h with
  | (x, y) when x < min_row && y < min_col -> iter t x y
  | (x, y) when x < min_row && y > min_col -> iter t x min_col
  | (x, y) when x > min_row && y < min_col -> iter t min_row y
  | (_, _) -> iter t min_row min_col
  end
  (* Run iter with arbitrary minimums initialized. All terms
  are iterated through, so this function guarantees the minimums
will be returned regardless of what we start with. *)
in let mins = iter (LocSet.elements locs)
 (fst (LocSet.min_elt locs)) (snd (LocSet.min_elt locs)) 
 (* Call previous function given our minimums. This will move
 all locations this distance, guaranteeing that the minimums 
  are at 0. *)
in translate_piece {label; locs} (fst mins) (snd mins)

let reflect_piece { label; locs } = 
  (* Initialize new LocSet *)
  let new_locs = LocSet.empty in 
  (* Iterate through all locations, add the locations with
  the vertical components (columns) negated to our new LocSet *)
  let final_locs = LocSet.fold
   (fun (x, y) locs_ -> LocSet.add (x, (-y)) locs_)
   locs new_locs in
   (* Normalize the new piece after negation *)
   normalize_piece {label = label; locs = final_locs}

let rotate_piece { label; locs } = 
  let new_locs = LocSet.empty in 
  (* Redefine locations by negating the row coordinates and swapping 
  the row/column coordinates in the new LocSet. *)
  let final_locs = LocSet.fold
   (fun (x, y) locs_ -> LocSet.add (y, (-x)) locs_)
   locs new_locs in
   (* Normalize the piece after the changes have been made. *)
   normalize_piece {label = label; locs = final_locs}
let piece_in_all_orientations p = 
  let p1  = normalize_piece p in
  let p2  = rotate_piece p1 in
  let p3  = rotate_piece p2 in
  let p4  = rotate_piece p3 in
  let p1_ = reflect_piece p1 in
  let p2_ = reflect_piece p2 in
  let p3_ = reflect_piece p3 in
  let p4_ = reflect_piece p4 in
  let piece_list = [p1; p2; p3; p4; p1_; p2_; p3_; p4_] in
    PieceSet.of_list piece_list

let all_normalized_pieces piece_string_list = 
  let all_pieces   = List.map make_piece_from_string piece_string_list in
  let orientations = List.map piece_in_all_orientations all_pieces in
    List.fold_left PieceSet.union PieceSet.empty orientations

(*
 * Boards.
 *)

let on_board p b = 
  let test (row, col) =
    row >= 0 && 
    col >= 0 && 
    row < b.nrows && 
    col < b.ncols &&
    b.member (row, col)
  in
    LocSet.for_all test p.locs

let translate_piece_on_board b p = 
  (* This helper function iterates from 0 to the number of rows, 
  and attempts to translate the piece with this change in row each 
  time. *)
  let rec iter_row b p s dr = 
  if dr = b.nrows then s else
    (* This helper function iterates from 0 to the number of cols and
    attempts to translate the piece with this change in column each time. *)
    let rec iter_col b p s dr dc = 
    if dc = b.ncols then s else
    (* The nested iter functions allow us to get all possible changes of
    row with all possible changes of column. *)
      let new_piece = translate_piece p dr dc in 
      (* Check to see if the newly translated piece is on the board.*)
      if on_board new_piece b 
      (* If on the board, add it to the PieceSet, otherwise do not *)
      (* Recursive iter cols *)
      then iter_col b p (PieceSet.add new_piece s) dr (dc + 1)
    else iter_col b p s dr (dc + 1)
  in iter_row b p (iter_col b p s dr 0) (dr + 1)
  (* Start with an empty PieceSet *)
in iter_row b p PieceSet.empty 0


let all_pieces_on_board piece_string_list b =
  let pieces     = all_normalized_pieces piece_string_list in
  let elems      = PieceSet.elements pieces in
  let translated = List.map (translate_piece_on_board b) elems in
    List.fold_left PieceSet.union PieceSet.empty translated

let make_piece_array piece_string_list b = 
  (* Get a set of all possible placements of all pieces on the board. *)
  let pieces = all_pieces_on_board piece_string_list b in 
  let lst = [] in 
  (* Iterate through the set, accumulating a list of all of the possible
  placements. *)
  let piece_lst = PieceSet.fold (fun p l -> ([p] @ l)) pieces lst in 
  (* Turn this list into an array. (Reverse to pass test) *)
  Array.of_list (List.rev piece_lst)

let all_locs_on_board b =
  let rec iter row col lst =
    match () with
      | _ when row = b.nrows -> lst
      | _ when col = b.ncols -> iter (row + 1) 0 lst
      | _ when b.member (row, col) -> iter row (col + 1) ((row, col) :: lst)
      | _ -> iter row (col + 1) lst
  in iter 0 0 []

(*
 * Constraints.
 *)

let make_constraints_array piece_string_list b = 
  let locs = all_locs_on_board b in 
  (* Turn all locations into Loc values in a new list *)
  let loc_lst = List.map (fun x -> Loc x) locs in  
  (* Turn all piece labels into Label values in a new list *)
  let labels = List.map (fun x -> Label (fst x)) piece_string_list in 
  (* Append these two lists as they comprise all of the constraints. *)
  let arr = loc_lst @ labels in 
  (* Turn this list into an array and return it *)
  Array.of_list arr

let make_constraints_map piece_string_list b = 
  (* Gather the constraints array *)
  let arr = make_constraints_array piece_string_list b in 
  (* Initialize the new map. *)
  let new_map = PconstraintMap.empty in 
  (* Iterate through the constraints array and the indices of the array. *)
  let rec iter arr i map_ = 
  (* Stopping condition to return the map *)
    if i = Array.length arr then map_
    (* add new binding to the map, flipping index with constraint *)
  else iter arr (i + 1) (PconstraintMap.add (arr.(i)) i map_)
  in iter arr 0 new_map 

(*
 * Inputs to algorithm X.
 *)

let piece_constraints arr i map_ = 
  (* Given the array, the index, and the map, we get all
  of the constraints for that piece. We make the label a Label value,
  we make all locs a Loc value and we append all of these into a list.
  We use this list to get column indices where there are 1s for this
  row index. We create a new list with tuples of (row, column) coords. *)
  let lab_constraint = [Label arr.(i).label] in
  let total_constraints = LocSet.fold 
  (fun x l -> l @ [Loc x]) arr.(i).locs lab_constraint in 
  List.map (fun x -> (i, (PconstraintMap.find x map_))) total_constraints

let make_binary_matrix_locs piece_string_list b = 
  (* Make a piece array *)
  let arr = make_piece_array piece_string_list b in 
  (* Make a constraints map *)
  let new_map = make_constraints_map piece_string_list b in 
  let rec iter arr i map_ l = 
    if i = Array.length arr then l 
    (* Iterate through array adding the tuples from each piece to the same
    list. *)
  else iter arr (i + 1) map_ ((piece_constraints arr i map_) @ l)
  (* Make a LocSet of these locations from iter *)
in LocSet.of_list (iter arr 0 new_map [])


