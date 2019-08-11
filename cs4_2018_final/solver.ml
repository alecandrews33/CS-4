(* Student name: Alec Andrews *)
(* CMS cluster login name: avandrew *)
open Binmat

(* Algorithm X:

    If the matrix A has no columns, the current partial solution is a valid
      solution; terminate successfully.

    Otherwise choose a column c (deterministically).  Ideally this should be
    the column with the fewest 1s in it.
   
    Choose a row r such that A(r, c) = 1 (nondeterministically).  If there is no
    such row (i.e. column c is all 0s), the algorithm terminates unsuccessfully.
    Otherwise, include row r in the partial solution.

    Given row r:
      for each column c such that m(r, c) = 1,
        for each row r' such that m(r', c) = 1,
          delete row r from matrix A.
        delete column c from matrix A.

    Repeat this algorithm recursively on the reduced matrix A.
 *)




module AlgorithmX(B : BinaryMatrix) =
  struct
    (* If the algorithm is successful, return a set of integers.
     * Otherwise, return nothing. *)
    

    let remover mat r =  
    (* Helper function that returns a new matrix after removing
    all rows in the matrix which have 1s in any of the columns
    that the selected row has 1s in (including r itself) and
    removing all columns which have 1s in r *)

    (* Get the columns with a 1 in this row. They will be removed *)
      let cols = B.cols_for_row mat r in 
    (* We must gather the rows that have 1s in the columns in cols. 
    This code iterates through each row with a 1 in each column and adds
    them to the accumulated list. It makes sure not to add duplicates
    to the accumulated list. *)
      let rows = (List.fold_left (fun l c -> 
        let rec iter lst r_lst = match r_lst with
        | [] -> lst
        (* Don't add duplicates *)
        | h :: t when List.mem h lst -> iter lst t
        | h :: t -> iter ([h] @ lst) t
      in iter l (B.rows_for_col mat c)) [] cols) in 
      (* Redefine the matrix by deleting each of the columns and rows
      that we found above. *)
      let new_mat =  List.fold_left (fun _mat c -> B.delete_col _mat c)
      mat cols in 
      List.fold_left (fun _mat r -> B.delete_row _mat r)
      new_mat rows




    



    let solve (matrix : B.t) : IntSet.t option = 
    (* Initialize our empty set for our solution *)
    let emp = IntSet.empty in 
    let rec algo_helper mat s  =
    (* This is step 1 in the algorithm outline. Return the set
    of row indices if no columns remain *)
    if B.cols mat = [] then Some s 
    (* Ger the column with the lowest number of 1s. *)
    else let min_col = B.min_col_sum_index mat in 
    (* This line represents unsuccessful termination. (No columns
    with any 1s) *)
    if B.rows_for_col mat min_col = [] then None else
    (* Get the rows at this min_col *)
    let rows = B.rows_for_col mat min_col in 
    (* This function does Step 4 in the outline. It 
    recursively runs this function to see if a selected row
    will yield a solution. If not, it checks the next possible row.
  If it runs out of rows, it returns unsuccessfully. *)
    let rec iter matr s rows_ = 
    match rows_ with
    | [] -> None
    | h::t -> 
    begin
      (* See if the given row will yield a solution. Keep track
      of row indices in the IntSet s. *)
      match algo_helper (remover matr h) (IntSet.add h s) with 
      | None -> iter matr s t
      | Some sol -> Some sol
    end
    in iter mat s rows
      in algo_helper matrix emp
  end

