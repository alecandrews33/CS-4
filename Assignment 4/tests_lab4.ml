(* Tests for lab4.ml *)

open OUnit2
open Lab4

let square n = n * n

(* Compare two lists for "set equality" i.e. that they have the
 * same elements.  Here we require that no list have extra duplicates
 * of existing elements that the other list doesn't have. *)
let rec set_equal lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

(* Test cases for problem A.1 *)

let m0 = 
  make_mobile 
    (make_weight 1 1) 
    (make_weight 1 1)

let m1 = 
  make_mobile
    (make_weight 3 4)
    (make_structure 
      4
      (make_mobile
        (make_weight 1 2)
        (make_weight 2 1)))

let m2 =
  make_mobile
    (make_weight 1 400)
    (make_structure 
      10
      (make_mobile
        (make_weight 100 1)
        (make_weight 1 200)))
  
let m3 =
  make_mobile
    (make_weight 1 (total_weight1 m2))
    (make_structure 1 m2)

(* Test cases for problems A.2 and A.3 *)
let tree1 = Tree 
  [Num 10; 
   Sub (Tree [Num 20; 
              Sub (Tree [Num 42; Sub (Tree []); Num 12]); 
              Sub (Tree []);
              Sub (Tree [Num 13; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 2; Num 3])]
  
let tree2 = Tree
  [Num 100; 
   Sub (Tree [Num 400; 
              Sub (Tree [Num 1764; Sub (Tree []); Num 144]); 
              Sub (Tree []);
              Sub (Tree [Num 169; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 4; Num 9])]

(* For problem A.3: *)
let square_tree'' t = tree_map square t 

(* Helper functions for section C: *)
let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

let derivative expr var =
  let e = simplify expr in
  let d = deriv e var in
    simplify d

let all_tests = "all" >:::
[ 
  "Problem A.1" >:: (fun c ->
     assert_bool  "is_balanced" (is_balanced m0);
     assert_bool  "is_balanced" (is_balanced m1);
     assert_bool  "is_balanced" (not (is_balanced m2));
     assert_bool  "is_balanced" (not (is_balanced m3));
     assert_equal (total_weight1 m0) 2;
     assert_equal (total_weight2 m0) 2;
     assert_equal (total_weight1 m1) 7;
     assert_equal (total_weight2 m1) 7;
     assert_equal (total_weight1 m2) 601;
     assert_equal (total_weight2 m2) 601;
     assert_equal (total_weight1 m3) 1202;
     assert_equal (total_weight2 m3) 1202
  );

  "Problem A.2" >:: (fun c ->
     assert_equal (square_tree (Tree [])) (Tree []);
     assert_equal (square_tree' (Tree [])) (Tree []);
     assert_equal (square_tree tree1) tree2;
     assert_equal (square_tree' tree1) tree2
  );

  "Problem A.3" >:: (fun c ->
     assert_equal (square_tree'' (Tree [])) (Tree []);
     assert_equal (square_tree'' tree1) tree2;
     assert_equal (square_tree'' tree1) (square_tree tree1);
     assert_equal (square_tree'' tree2) (square_tree tree2)
  );

  "Problem A.4" >:: (fun c ->
     assert_equal (subsets []) [[]];
     assert_bool "A.4.2" (set_equal (subsets [1]) [[];[1]]);
     assert_bool "A.4.3" (set_equal
       (subsets [1;2;3]) 
       [[];[3];[2];[2;3];[1];[1;3];[1;2];[1;2;3]]);
  );

  "Problem A.5" >:: (fun c ->
     assert_equal (map square [1;2;3;4;5]) [1;4;9;16;25];
     assert_equal (map square []) [];
     assert_equal (append [] []) [];
     assert_equal (append [1] [2]) [1;2];
     assert_equal (append [1;2;3;4;5] []) [1;2;3;4;5];
     assert_equal (append [] [1;2;3;4;5]) [1;2;3;4;5];
     assert_equal (append [1;2;3;4;5] [6;7;8;9;10]) [1;2;3;4;5;6;7;8;9;10];
     assert_equal (length []) 0;
     assert_equal (length [1;2;3;4;5]) 5
  );

  "Problem A.6" >:: (fun c ->
     assert_equal (accumulate_n (+) 0 [[];[];[]]) [];
     assert_equal (accumulate_n (+) 0 [[1;2;3];[4;5;6];[7;8;9];[10;11;12]])
       [22;26;30];
     assert_equal (accumulate_n ( * ) 1 [[2;3];[4;5]]) [8;15]
  );

  "Problem A.7" >:: (fun c ->
     assert_equal (dot_product [] []) 0;
     assert_equal (dot_product [1;2;3] [4;5;6]) 32;
     assert_equal (matrix_times_vector [[1;0];[0;1]] [10;20]) [10;20];
     assert_equal (matrix_times_vector [[1;2];[3;4]] [-2;3]) [4;6];
     assert_equal (transpose [[1;2];[3;4]]) [[1;3];[2;4]];
     assert_equal (transpose [[1;2;3];[4;5;6]]) [[1;4];[2;5];[3;6]];
     assert_equal (matrix_times_matrix [[1;0];[0;1]] [[1;2];[3;4]]) 
                                       [[1;2];[3;4]];
     assert_equal (matrix_times_matrix [[1;2];[3;4]] [[1;2];[3;4]]) 
                                       [[7;10];[15;22]];
     assert_equal (matrix_times_matrix [[1;2;3];[4;5;6]] [[1;2];[3;4];[5;6]])
                                       [[22;28];[49;64]]
  );

  "Problem B.1" >:: (fun c ->
     assert_equal (quicksort [] (<)) [];
     assert_equal (quicksort [1] (<)) [1];
     assert_equal (quicksort [1;2;3;4;5] (<)) [1;2;3;4;5];
     assert_equal (quicksort [5;4;3;2;1;1;2;3;4;5] (<)) [1;1;2;2;3;3;4;4;5;5];
     assert_equal (quicksort [5;4;3;2;1;1;2;3;4;5] (>)) [5;5;4;4;3;3;2;2;1;1]
  );

  "Problem B.4" >:: (fun c ->
     assert_equal (insertion_sort [] (<)) [];
     assert_equal (insertion_sort [1] (<)) [1];
     assert_equal (insertion_sort [1;2;3;4;5] (<)) [1;2;3;4;5];
     assert_equal (insertion_sort [5;4;3;2;1;1;2;3;4;5] (<)) [1;1;2;2;3;3;4;4;5;5];
     assert_equal (insertion_sort [5;4;3;2;1;1;2;3;4;5] (>)) [5;5;4;4;3;3;2;2;1;1]
  );

  "Problem C.1" >:: (fun c ->
     assert_equal (simplify (Int 42)) (Int 42);
     assert_equal (simplify (Var "x")) (Var "x");
     assert_equal (simplify (Add (Int 32, Int 41))) (Int 73);
     assert_equal (simplify (Add (Add (Int 1, Int 2), Add (Int 3, Int 4))))
                  (Int 10);
     assert_equal (simplify (Add (Mul (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 14);
     assert_equal (simplify (Mul (Mul (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 24);
     assert_equal (simplify (Mul (Add (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 36);
     assert_equal (simplify (Pow (Int 0, 0))) (Int 1);
     assert_equal (simplify (Pow (Int 10, 2))) (Int 100);
     assert_equal (simplify (Pow (Add (Int 1, Int 2), 2))) (Int 9);
     assert_equal (simplify (Add (Var "x", Int 0))) (Var "x");
     assert_equal (simplify (Add (Int 0, Var "x"))) (Var "x");
     assert_equal (simplify (Mul (Int 0, Var "y"))) (Int 0);
     assert_equal (simplify (Mul (Var "y", Int 0))) (Int 0);
     assert_equal (simplify (Mul (Int 1, Var "z"))) (Var "z");
     assert_equal (simplify (Mul (Var "z", Int 1))) (Var "z");
     assert_equal (simplify (Pow (Var "x", 0))) (Int 1);
     assert_equal (simplify (Pow (Var "x", 1))) (Var "x");
     assert_equal (simplify (Pow (Add (Var "x", Int 0), 1))) (Var "x");
     assert_equal 
       (simplify (Add (Add (Var "x", Int 0), Mul (Var "y", Int 0)))) 
       (Var "x")
  );

  "Problem C.2" >:: (fun c ->
     assert_equal (derivative (Int 10) "x") (Int 0);
     assert_equal (derivative (Var "x") "x") (Int 1);
     assert_equal (derivative (Var "y") "x") (Int 0);
     assert_equal (derivative (Add (Var "x", Var "x")) "x") (Int 2);
     assert_equal (derivative (Add (Add (Var "x", Var "x"), Var "x")) "x") 
                              (Int 3);
     assert_equal (derivative (Mul (Var "x", Int 42)) "x") (Int 42);
     assert_equal (derivative (Mul (Var "x", Var "y")) "x") (Var "y");
     assert_equal (derivative (Mul (Var "x", Var "y")) "z") (Int 0);
     assert_equal 
       (derivative (Mul (Pow (Var "x", 2), Mul (Int 3, Var "x"))) "x")
       (Add (Mul (Pow (Var "x", 2), Int 3),
             Mul (Mul (Int 2, Var "x"), Mul (Int 3, Var "x"))));
     assert_equal (derivative (Pow (Var "y", 1)) "x") (Int 0);
     assert_equal (derivative (Pow (Var "x", 1)) "x") (Int 1);
     assert_equal (derivative (Pow (Var "x", 2)) "x") 
                              (Mul ((Int 2), (Var "x")));
     assert_equal (derivative (Pow (Mul (Int 3, Var "x"), 3)) "x")
                              (Mul 
                                (Mul 
                                  (Int 3, 
                                    Pow (Mul (Int 3, Var "x"), 2)),
                                  Int 3));
     assert_equal (derivative (Add (Mul (Int 4, Pow (Var "x", 3)), 
                                    Mul (Int 6, Pow (Var "x", 2)))) "x")
                              (Add (Mul (Int 4, Mul (Int 3, Pow (Var "x", 2))),
                                    Mul (Int 6, Mul (Int 2, Var "x"))))

  );
]

let _ = run_test_tt_main all_tests

