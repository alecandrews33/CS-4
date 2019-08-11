(* Tests for CS 4 final exam, Winter 2018, part C. *)

(*
To run this test script from the ocaml toplevel, do this:

$ ocaml
# #use "topfind";;  (* may not be necessary *)
# #require "oUnit";;
# open OUnit;;
# #load "pentomino_types.cmo";;
# #load "pentominoes.cmo";;
# #use "tests_pentominoes.ml";;
*)

open OUnit2
open Pentomino_pieces
open Pentomino_types
open Pentominoes

(*** Utility functions. ***)

(* Expect an Invalid_argument exception. *)
let expect_invalid_arg msg thunk =
  assert_bool msg
    (try (thunk (); false)
     with Invalid_argument _ -> true)

(* Expect a Failure exception. *)
let expect_failure msg thunk =
  assert_bool msg
    (try (thunk (); false)
     with Failure _ -> true)

(* Test if two lists have the same elements in whatever order. *)
let equal_lists lst1 lst2 =
  let lst1' = List.sort compare lst1 in
  let lst2' = List.sort compare lst2 in
    lst1' = lst2'

(*** The tests. ***)

let all_tests = "all_tests" >:::
[ 
  "translate_piece" >:: (fun _ ->
    let p   = make_piece ('q', [(0, 1); (1, 0); (1, 1); (1, 2); (2, 2)]) in
    let p'  = translate_piece p 2 3 in
    let p'' = translate_piece p (-10) 200 in
      begin
        assert_bool "translate_piece 1" (p'.label = 'q' && p''.label = 'q');
        assert_bool "translate_piece 2" 
          (equal_lists (LocSet.elements p'.locs)
             [(2, 4); (3, 3); (3, 4); (3, 5); (4, 5)]);
        assert_bool "translate_piece 3" 
          (equal_lists (LocSet.elements p''.locs)
             [(-10, 201); (-9, 200); (-9, 201); (-9, 202); (-8, 202)])
      end
  );

  "normalize_piece" >:: (fun _ ->
    let p = make_piece ('a', [(10, 6); (10, 7); (11, 5); (11, 6); (12, 5)]) in
    let p' = normalize_piece p in
      begin
        assert_bool "normalize_piece 1" (p'.label = 'a');
        assert_bool "normalize_piece 2"
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 1); (0, 2); (1, 0); (1, 1); (2, 0)])
      end;
    
    let p = 
      make_piece ('b', [(-5, -4); (-5, -3); (-5, -2); (-4, -4); (-3, -4)])
    in
    let p' = normalize_piece p in
      begin
        assert_bool "normalize_piece 3" (p'.label = 'b');
        assert_bool "normalize_piece 4" 
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 0); (0, 1); (0, 2); (1, 0); (2, 0)])
      end;
  );

  "reflect_piece" >:: (fun _ ->
     let p = make_piece ('p', [(0, 1); (1, 0); (1, 1); (1, 2); (2, 2)]) in
     let p' = reflect_piece p in
     let q = make_piece ('q', [(10, 10); (10, 11); (11, 10); (11, 11); (12, 10)]) in
     let q' = reflect_piece q in
      begin
        assert_bool "reflect_piece 1" (p.label  = 'p');
        assert_bool "reflect_piece 2" (p'.label = 'p');
        assert_bool "reflect_piece 3" (q.label  = 'q');
        assert_bool "reflect_piece 4" (q'.label = 'q');
        assert_bool "reflect_piece 5" 
          (equal_lists (LocSet.elements p.locs) 
             [(0, 1); (1, 0); (1, 1); (1, 2); (2, 2)]);
        assert_bool "reflect_piece 6" 
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 1); (1, 0); (1, 1); (1, 2); (2, 0)]);
        assert_bool "reflect_piece 7" 
          (equal_lists (LocSet.elements q.locs) 
             [(10, 10); (10, 11); (11, 10); (11, 11); (12, 10)]);
        assert_bool "reflect_piece 8" 
          (equal_lists (LocSet.elements q'.locs) 
             [(0, 0); (0, 1); (1, 0); (1, 1); (2, 1)]);
      end;
  );

  "rotate_piece" >:: (fun _ ->
     let p = make_piece ('p', [(0, 1); (1, 0); (1, 1); (1, 2); (2, 2)]) in
	 let p' = rotate_piece p in
	 let q = make_piece ('q', [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4)]) in
	 let q' = rotate_piece q in
	 let r = make_piece ('r', [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1)]) in
	 let r' = rotate_piece r in
      begin
        assert_bool "rotate_piece 1" (p.label  = 'p');
        assert_bool "rotate_piece 2" (p'.label = 'p');
        assert_bool "rotate_piece 3" (q.label  = 'q');
        assert_bool "rotate_piece 4" (q'.label = 'q');
        assert_bool "rotate_piece 5" (r.label  = 'r');
        assert_bool "rotate_piece 6" (r'.label = 'r');
        assert_bool "rotate_piece 7" 
          (equal_lists (LocSet.elements p.locs) 
             [(0, 1); (1, 0); (1, 1); (1, 2); (2, 2)]);
        assert_bool "rotate_piece 8" 
          (equal_lists (LocSet.elements p'.locs) 
             [(0, 1); (1, 1); (1, 2); (2, 0); (2, 1)]);
        assert_bool "rotate_piece 9" 
          (equal_lists (LocSet.elements q.locs) 
             [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4)]);
        assert_bool "rotate_piece 10" 
          (equal_lists (LocSet.elements q'.locs) 
             [(0, 0); (1, 0); (2, 0); (3, 0); (4, 0)]);
        assert_bool "rotate_piece 11" 
          (equal_lists (LocSet.elements r.locs) 
             [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1)]);
        assert_bool "rotate_piece 12" 
          (equal_lists (LocSet.elements r'.locs) 
             [(0, 0); (0, 1); (1, 0); (1, 1); (2, 1)]);
      end
  );

  "translate_piece_on_board" >:: (fun _ ->
     let p = make_piece ('p', [(0, 0); (0, 1); (1, 0); (1, 1); (1, 2)]) in
     let brd = { nrows = 4; ncols = 4; member = fun _ -> true} in
     let ps = translate_piece_on_board brd p in
     let tp1 = make_piece ('p', [(0, 0); (0, 1); (1, 0); (1, 1); (1, 2)]) in
     let tp2 = make_piece ('p', [(0, 1); (0, 2); (1, 1); (1, 2); (1, 3)]) in
     let tp3 = make_piece ('p', [(1, 0); (1, 1); (2, 0); (2, 1); (2, 2)]) in
     let tp4 = make_piece ('p', [(1, 1); (1, 2); (2, 1); (2, 2); (2, 3)]) in
     let tp5 = make_piece ('p', [(2, 0); (2, 1); (3, 0); (3, 1); (3, 2)]) in
     let tp6 = make_piece ('p', [(2, 1); (2, 2); (3, 1); (3, 2); (3, 3)]) in
     let noneOf item lst = not (List.mem item lst) in
     let brd2 =
      {  nrows = 8;
         ncols = 8;
         member = fun loc -> noneOf loc [(3, 3); (3, 4); (4, 3); (4, 4)] } in
     let ps2 = translate_piece_on_board brd2 p in
       begin
         assert_bool "translate_piece_on_board 1" 
           (PieceSet.cardinal ps = 6);
         assert_bool "translate_piece_on_board 2" 
           (PieceSet.mem tp1 ps);
         assert_bool "translate_piece_on_board 3" 
           (PieceSet.mem tp2 ps);
         assert_bool "translate_piece_on_board 4" 
           (PieceSet.mem tp3 ps);
         assert_bool "translate_piece_on_board 5" 
           (PieceSet.mem tp4 ps);
         assert_bool "translate_piece_on_board 6" 
           (PieceSet.mem tp5 ps);
         assert_bool "translate_piece_on_board 7" 
           (PieceSet.mem tp6 ps);
         assert_bool "translate_piece_on_board 8" 
           (PieceSet.cardinal ps2 = 31)
       end

  );

  "make_piece_array" >:: (fun _ ->
     let piece_string_list = Pentomino_pieces.all_pieces in
     let noneOf item lst = not (List.mem item lst) in
     let brd =
          {  nrows = 8;
             ncols = 8;
             member = fun loc -> noneOf loc [(3, 3); (3, 4); (4, 3); (4, 4)] }
     in
     let arr = make_piece_array piece_string_list brd in
       assert_bool "make_piece_array 1" (Array.length arr = 1568)
  );

  "make_constraints_array" >:: (fun _ ->
     let piece_string_list = Pentomino_pieces.all_pieces in
     let brd = { nrows = 5; ncols = 5; member = fun _ -> true} in
     let ca = make_constraints_array piece_string_list brd in
     let ca_expected_as_list =
       [Loc (4, 4); Loc (4, 3); Loc (4, 2); Loc (4, 1); Loc (4, 0); 
        Loc (3, 4); Loc (3, 3); Loc (3, 2); Loc (3, 1); Loc (3, 0); 
        Loc (2, 4); Loc (2, 3); Loc (2, 2); Loc (2, 1); Loc (2, 0); 
        Loc (1, 4); Loc (1, 3); Loc (1, 2); Loc (1, 1); Loc (1, 0); 
        Loc (0, 4); Loc (0, 3); Loc (0, 2); Loc (0, 1); Loc (0, 0); 
        Label 'f'; Label 'i'; Label 'l'; Label 'n'; Label 'p';
        Label 't'; Label 'u'; Label 'v'; Label 'w'; Label 'x'; Label 'y';
        Label 'z'] 
     in
       assert_bool "make_constraints_array 1"
         (equal_lists (Array.to_list ca) ca_expected_as_list)
  );

  "make_constraints_map" >:: (fun _ ->
     let piece_string_list = Pentomino_pieces.all_pieces in
     let brd = { nrows = 5; ncols = 5; member = fun _ -> true} in
     let cmap = make_constraints_map piece_string_list brd in
     let cmap_as_list = PconstraintMap.bindings cmap in
     let cmap_expected_as_list =
       [(Loc (0, 0), 24); (Loc (0, 1), 23); (Loc (0, 2), 22); (Loc (0, 3), 21);
        (Loc (0, 4), 20); (Loc (1, 0), 19); (Loc (1, 1), 18); (Loc (1, 2), 17);
        (Loc (1, 3), 16); (Loc (1, 4), 15); (Loc (2, 0), 14); (Loc (2, 1), 13);
        (Loc (2, 2), 12); (Loc (2, 3), 11); (Loc (2, 4), 10); (Loc (3, 0), 9);
        (Loc (3, 1), 8); (Loc (3, 2), 7); (Loc (3, 3), 6); (Loc (3, 4), 5);
        (Loc (4, 0), 4); (Loc (4, 1), 3); (Loc (4, 2), 2); (Loc (4, 3), 1);
        (Loc (4, 4), 0); (Label 'f', 25); (Label 'i', 26); (Label 'l', 27);
        (Label 'n', 28); (Label 'p', 29); (Label 't', 30); (Label 'u', 31);
        (Label 'v', 32); (Label 'w', 33); (Label 'x', 34); (Label 'y', 35);
        (Label 'z', 36)]
     in
       assert_bool "make_constraints_map 1"
         (equal_lists (cmap_as_list) cmap_expected_as_list)
  );

  "make_binary_matrix_locs" >:: (fun _ ->
     let board =
         { nrows = 3;
           ncols = 5;
           member = fun _ -> true } in
     let pl_s = "\nX.\nX.\nX.\nXX\n" in
     let pt_s = "\nXXX\n.X.\n.X.\n" in
     let py_s = "\n..X.\nXXXX\n" in
     let pieces = [('l', pl_s); ('t', pt_s); ('y', py_s)] in
     let pa = make_piece_array pieces board in        (* not actually needed here *)
     let ca = make_constraints_array pieces board in  (* not actually needed here *)
     let locs = make_binary_matrix_locs pieces board in
     let loc_list = LocSet.elements locs in
     let loc_list_expected =
       [(0, 9); (0, 11); (0, 12); (0, 13); (0, 14); (0, 15); (1, 6); (1, 11);
        (1, 12); (1, 13); (1, 14); (1, 15); (2, 6); (2, 7); (2, 8); (2, 9);
        (2, 14); (2, 15); (3, 8); (3, 10); (3, 11); (3, 12); (3, 13); (3, 15);
        (4, 5); (4, 10); (4, 11); (4, 12); (4, 13); (4, 15); (5, 5); (5, 6);
        (5, 7); (5, 8); (5, 13); (5, 15); (6, 6); (6, 7); (6, 8); (6, 9); (6, 11);
        (6, 15); (7, 5); (7, 6); (7, 7); (7, 8); (7, 10); (7, 15); (8, 4); (8, 6);
        (8, 7); (8, 8); (8, 9); (8, 15); (9, 1); (9, 6); (9, 7); (9, 8); (9, 9);
        (9, 15); (10, 1); (10, 2); (10, 3); (10, 4); (10, 9); (10, 15); (11, 3);
        (11, 5); (11, 6); (11, 7); (11, 8); (11, 15); (12, 0); (12, 5); (12, 6);
        (12, 7); (12, 8); (12, 15); (13, 0); (13, 1); (13, 2); (13, 3); (13, 8);
        (13, 15); (14, 1); (14, 2); (14, 3); (14, 4); (14, 6); (14, 15); (15, 0);
        (15, 1); (15, 2); (15, 3); (15, 5); (15, 15); (16, 3); (16, 8); (16, 12);
        (16, 13); (16, 14); (16, 16); (17, 4); (17, 7); (17, 8); (17, 9); (17, 14);
        (17, 16); (18, 2); (18, 7); (18, 11); (18, 12); (18, 13); (18, 16);
        (19, 3); (19, 6); (19, 7); (19, 8); (19, 13); (19, 16); (20, 2); (20, 3);
        (20, 4); (20, 8); (20, 13); (20, 16); (21, 1); (21, 6); (21, 10); (21, 11);
        (21, 12); (21, 16); (22, 2); (22, 7); (22, 8); (22, 9); (22, 12); (22, 16);
        (23, 2); (23, 5); (23, 6); (23, 7); (23, 12); (23, 16); (24, 1); (24, 2);
        (24, 3); (24, 7); (24, 12); (24, 16); (25, 1); (25, 6); (25, 7); (25, 8);
        (25, 11); (25, 16); (26, 0); (26, 1); (26, 2); (26, 6); (26, 11); (26, 16);
        (27, 0); (27, 5); (27, 6); (27, 7); (27, 10); (27, 16); (28, 8); (28, 11);
        (28, 12); (28, 13); (28, 14); (28, 17); (29, 7); (29, 11); (29, 12);
        (29, 13); (29, 14); (29, 17); (30, 7); (30, 10); (30, 11); (30, 12);
        (30, 13); (30, 17); (31, 6); (31, 10); (31, 11); (31, 12); (31, 13);
        (31, 17); (32, 6); (32, 7); (32, 8); (32, 9); (32, 13); (32, 17); (33, 6);
        (33, 7); (33, 8); (33, 9); (33, 12); (33, 17); (34, 5); (34, 6); (34, 7);
        (34, 8); (34, 12); (34, 17); (35, 5); (35, 6); (35, 7); (35, 8); (35, 11);
        (35, 17); (36, 3); (36, 6); (36, 7); (36, 8); (36, 9); (36, 17); (37, 2);
        (37, 6); (37, 7); (37, 8); (37, 9); (37, 17); (38, 2); (38, 5); (38, 6);
        (38, 7); (38, 8); (38, 17); (39, 1); (39, 5); (39, 6); (39, 7); (39, 8);
        (39, 17); (40, 1); (40, 2); (40, 3); (40, 4); (40, 8); (40, 17); (41, 1);
        (41, 2); (41, 3); (41, 4); (41, 7); (41, 17); (42, 0); (42, 1); (42, 2);
        (42, 3); (42, 7); (42, 17); (43, 0); (43, 1); (43, 2); (43, 3); (43, 6);
        (43, 17)]
     in
       begin
         assert_bool "make_binary_matrix_locs 1"
           (Array.length pa = 44);
         assert_bool "make_binary_matrix_locs 2"
           (Array.length ca = 18);
         assert_bool "make_binary_matrix_locs 3"
           (LocSet.cardinal locs = 264);
         assert_bool "make_binary_matrix_locs 4"
           (equal_lists loc_list loc_list_expected)
       end
  );

]

let run_tests () = 
  begin
    Printf.printf "\nRUNNING PENTOMINOES TESTS...\n\n";
    run_test_tt_main all_tests;
  end

let _ = run_tests ()

