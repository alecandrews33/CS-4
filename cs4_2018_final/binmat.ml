(* Student name: Alec Andrews *)
(* CMS cluster login name: avandrew *)

(* Set of integers. *)
module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

(* Map using ints as keys. *)
module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

module type BinaryMatrix =
  sig
    type t

    val make : int -> int -> (int * int) list -> t  

    val nrows : t -> int
    val ncols : t -> int
    val rows  : t -> int list
    val cols  : t -> int list

    val get : t -> int -> int -> bool  

    val dump : t -> unit

    val rows_for_col : t -> int -> int list
    val cols_for_row : t -> int -> int list

    val delete_row : t -> int -> t
    val delete_col : t -> int -> t

    val min_col_sum_index : t -> int  
  end

(*
 * We represent binary matrices as a pair of maps between integer
 * indices and sets of integers.  One map maps row indices to a set of
 * column indices and the other maps column indices to a set of row indices.
 *)

(* Helper function to get the map from row indices to columns with
a 1 in that row. *)

let get_rows nrows lst = 
  let _rows = IntMap.empty in
  (* Helper function to iterate through all of the row indices. Looks through
  all (x, y) pairs in the given list, and it adds the column index to the set
  if the current index has a 1 at that column *)
  let rec iter curr nrows lst _rows = 
    (* This signifies when we have gone through all columns *)
    if curr = nrows then _rows
    else match (List.fold_left 
      (fun acc (x, y) -> if x = curr then (y :: acc) else acc) [] lst) with
    | [] -> iter (curr + 1) nrows lst _rows
    (* If a list is returned, we update our map. *)
    | l -> iter (curr + 1) nrows lst (IntMap.add curr 
      (IntSet.of_list l) _rows)
  in iter 0 nrows lst _rows 

(* Helper function to get the map from column indices to rows with
a 1 in that column. *)

let get_cols ncols lst = 
  let _cols = IntMap.empty in 
  (* Helper function to iterate through all of the column indices.
  Looks through all (x, y) pairs in the given list, and it adds the
   row index to the set if the current index has a 1 at that row. *)
  let rec iter curr ncols lst _cols =
    (* This signifies when we have gone through all columns *)
    if curr = ncols then _cols
    else match (List.fold_left
     (fun acc (x, y) -> if y = curr then (x :: acc) else acc) [] lst) with
    | [] -> iter (curr + 1) ncols lst _cols
    (* If a list is returned, we update our map. *)
    | l -> iter (curr + 1) ncols lst (IntMap.add curr
      (IntSet.of_list l) _cols)
  in iter 0 ncols lst _cols


module ImplBinaryMatrix : BinaryMatrix =
  struct
    type t = 
      { 
        _nrows : int; 
        _ncols : int; 

        (* Map between row indices and sets of column indices. *)
        _rows : IntSet.t IntMap.t;

        (* Map between columns indices and sets of row indices. *)
        _cols : IntSet.t IntMap.t;
      }

    let make nrows ncols lst = 
    (* Check for bounds errors from the (x, y) pairs *)
    if nrows <= 0
    then invalid_arg("make: nrows must be at least 1")
    else if ncols <= 0 then invalid_arg("make: ncols must be at least 1")
    else begin
    List.iter (fun (x, y) -> match (x, y) with
    | (r, c) when r >= nrows -> 
    invalid_arg(Printf.sprintf "make: invalid row/column coordinates: (%d, %d)" r c)
    | (r, c) when c >= ncols -> 
    invalid_arg(Printf.sprintf "make: invalid row/column coordinates: (%d, %d)" r c)
    | (r, c) when r < 0 ->
    invalid_arg(Printf.sprintf "make: invalid row/column coordinates: (%d, %d)" r c)
    | (r, c) when c < 0 ->
    invalid_arg(Printf.sprintf "make: invalid row/column coordinates: (%d, %d)" r c)
    | (_, _) -> ()) lst;
    (* Return the binary matrix *)
    {_nrows = nrows; _ncols = ncols; _rows = get_rows nrows lst;
    _cols = get_cols ncols lst};
    end
    

    let nrows m = m._nrows
    let ncols m = m._ncols

    (* Extract the rows that have 1s in them from the binary matrix *)
    let rows m = let imap = IntMap.bindings m._rows in
    fst (List.split imap)

    (* Extract the columns that have 1s in them from the binary matrix *)
    let cols m = let imap = IntMap.bindings m._cols in
    fst (List.split imap)

    let get m r c =
      try
        IntSet.mem c (IntMap.find r m._rows)
      with 
        Not_found -> false

    let dump m =
      let dump_map label1 label2 map =
        IntMap.iter (fun k v -> 
            begin
              Printf.printf "%s: %4d; %s: " label1 k label2;
              IntSet.iter (fun e -> Printf.printf "%d " e) v;
              Printf.printf "\n"
            end)
          map
      in
        begin
          Printf.printf "\n-----\n";
          Printf.printf "IMPL_BINARY_MATRIX: nrows = %d; ncols = %d\n"
            m._nrows m._ncols;
          Printf.printf "\nROW -> COLUMN SET MAP:\n";
          dump_map "row" "columns" m._rows;
          Printf.printf "\nCOLUMN -> ROW SET MAP:\n";
          dump_map "column" "rows" m._cols;
          Printf.printf "-----\n\n";
        end
    
    (* Checks if the column is a member of the binary matrix. Then
    finds the IntSet corresponding to that column and returns a list
    of its elements. *)    
    let rows_for_col m c = if IntMap.mem c m._cols
    then IntSet.elements (IntMap.find c m._cols)
    else raise (Failure 
      (Printf.sprintf "rows_for_col: missing column index: %d" c))

    (* Checks if the row is a member of the binary matrix. Then
    finds the IntSet corresponding to that row and returns a list
    of its elements. *) 
    let cols_for_row m r = if IntMap.mem r m._rows
    then IntSet.elements (IntMap.find r m._rows)
    else raise (Failure 
      (Printf.sprintf "cols_for_row: missing row index: %d" r))


    let delete_row m r = 
    (* Get all columns with a 1 in this row *)
    let cols = cols_for_row m r in 
    (* Redefine the binary matrix, removing the desired row and
    adjusting the columns that this deletion affected. *)
      {_nrows = m._nrows;
      _ncols = m._ncols; _rows = IntMap.remove r m._rows;
      (* Cycle through everything in the column list for this row. 
      Redefine the binding in the binary matrix's column map by 
      adding the same set with just that single row removed. *)
      _cols = List.fold_left 
      (fun _cols x -> IntMap.add x 
        (IntSet.remove r (IntMap.find x m._cols)) _cols) m._cols cols}

    
    

    let delete_col m c = 
    (* Get all rows with a 1 in this column *)
    let rows = rows_for_col m c in
    (* Redefine the binary matrix, removing the desired column and 
    adjusting the rows that this deletion affected *) 
      {_nrows = m._nrows;
      _ncols = m._ncols;
      (* Cycle through everything in the row list for this column. 
      Redefine the binding in the binary matrix's row map by adding 
      the same set with just that single column removed. *)
       _rows = List.fold_left 
      (fun _rows x -> IntMap.add x 
      (IntSet.remove c (IntMap.find x m._rows)) _rows) m._rows rows;
      _cols = IntMap.remove c m._cols }


    let min_col_sum_index m =
    (* Helper function that keeps track of which index has the 
    minimum column sum (fewest number of 1s). *)
    let rec iter m curr best = 
    (* Stopping condition -> return current best *)
    if curr = m._ncols then best
    (* Check that the current index is even in the column map *)
    else if IntMap.mem curr m._cols then
    (* Check the number of elements in the IntSet corresponding to
    the current index *)
     match (IntSet.cardinal (IntMap.find curr m._cols) < 
      IntSet.cardinal (IntMap.find best m._cols)) with
    (* If the current one has less 1s than the best one, 
    call iter again with the current one set as the best.
    Otherwise keep the current best as the best and run iter
    on the next iteration. *)
    | true -> iter m (curr + 1) curr
    | false -> iter m (curr + 1) best
    (* Move onto the next iteration if current index is not in map *)
    else iter m (curr + 1) best
    (* We start best as the first column *)
    in iter m 0 (List.hd (fst (List.split (IntMap.bindings m._cols))))

    end
