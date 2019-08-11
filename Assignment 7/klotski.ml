(* klotski.ml: core functionality of the Klotski game. *)
(* Student name: Alec Andrews *)
(* CMS cluster login name: avandrew *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)
(* CSMan didn't accept initial submission *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Pervasives.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b = 
  let final_spot = LocSet.of_list [(3,1); (3,2); (4,1); (4, 2)] in
  CharMap.exists (fun p locs -> (LocSet.equal locs final_spot)) b.pieces
  

let compare b1 b2 = 
  let cmap1 = CharMap.bindings b1.pieces in
  let cmap2 = CharMap.bindings b2.pieces in 
  let locs1 = LocSetSet.of_list (snd (List.split cmap1)) in
  let locs2 = LocSetSet.of_list (snd (List.split cmap2)) in
  LocSetSet.compare locs1 locs2

  


let remove c ({ pieces = p; unoccupied = u } as b) =
   if CharMap.mem c p 
   then {pieces = CharMap.remove c p; 
   unoccupied = LocSet.union u (CharMap.find c p)}
   else b


let add (c, p) { pieces = ps; unoccupied = u } = 
  if (LocSet.subset p u) && (not (CharMap.mem c ps))
  then Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}
  else None

let direction_check c p d i b = 
  let rec iter c p d i b n = 
    let new_p = LocSet.map (fun (x, y) ->
      match d with
      | Up -> (x - n, y)
      | Down -> (x + n, y)
      | Left -> (x, y - n)
      | Right -> (x, y + n)) p in 
        if n = i then match add (c, new_p) b with 
          | Some t -> true
          | None -> false
        else 
          match add (c, new_p) b with
          | Some t -> iter c p d i b (n + 1)
          | None -> false
  in iter c p d i b 1



let make_move (c, d, i) b =
  if i < 1 || i > 4 || (not (CharMap.mem c b.pieces)) then None 
  else let current = CharMap.find c b.pieces in
  let cur_board = remove c b in
  if direction_check c current d i cur_board 
  then let new_p = LocSet.map (fun (x, y) ->
      match d with
      | Up -> (x - i, y)
      | Down -> (x + i, y)
      | Left -> (x, y - i)
      | Right -> (x, y + i)) current in 
  add (c, new_p) cur_board 
  else None

let compile_moves c d i b = 
  let rec iter c d i b l = 
  match make_move (c, d, i) b with
  | Some t -> iter c d (i + 1) b (t :: l);
  | None -> l
in iter c d i b []

let concatenator c i b = 
  (compile_moves c Up i b) @ (compile_moves c Down i b) 
  @ (compile_moves c Left i b) @ (compile_moves c Right i b)

let next b =
  let cmap = CharMap.bindings b.pieces in
  let chars = fst (List.split cmap) in 
  let rec char_iter c b cboards = 
    match c with
    | [] -> cboards
    | ch :: ct -> char_iter ct b ((concatenator ch 1 b) @ cboards)
    in char_iter chars b []

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

