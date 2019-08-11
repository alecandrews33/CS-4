(* Tests for lab3.ml *)

open OUnit2
open Lab3

let check_within msg f1 f2 prec =
  assert_bool msg (cmp_float ~epsilon:prec f1 f2)

let check_within_points msg p1 p2 prec =
  let (x1, y1) = get_coords p1 in
  let (x2, y2) = get_coords p2 in
    check_within msg x1 x2 prec;
    check_within msg y1 y2 prec

let eps = 1.0e-8

let p1 = make_point 0.0 0.0
let p2 = make_point 10.0 0.0
let p3 = make_point 10.0 10.0
let s1 = make_segment p1 p2
let s2 = make_segment p2 p3
let s3 = make_segment p3 p1

let all_tests = "all" >:::
[ 
  "make_point/get_coords" >:: (fun c -> 
     let p = make_point 1.0 3.4 in
     let (x, y) = get_coords p in
       begin
         check_within "invalid x accessor for points" x 1.0 eps;
         check_within "invalid y accessor for points" y 3.4 eps
       end
  );

  "make_segment/get_points" >:: (fun c -> 
     let p1 = make_point 1.0 3.4 in
     let p2 = make_point (-1.0) 43.0 in
     let s = make_segment p1 p2 in
     let (p1', p2') = get_points s in
       begin
         check_within_points "invalid startp accessor for segments" p1' p1 eps;
         check_within_points "invalid endp accessor for segments" p2' p2 eps
       end
  );

  "segment_length" >:: (fun c -> 
     check_within "segment_length s1" (segment_length s1) 10.0 eps;
     check_within "segment_length s2" (segment_length s2) 10.0 eps;
     check_within "segment_length s3" (segment_length s3) 14.14213 0.00001
  );

  "midpoint_segment" >:: (fun c ->
     let (x, y) = get_coords (midpoint_segment s3) in
       check_within "x coord of midpoint of segment s3" x 5.0 eps;
       check_within "y coord of midpoint of segment s3" y 5.0 eps
  );

(*** TODO: Problem A.2 ***)

(*** Problem A.3: no tests ***)

(*** Problem A.4 ***)

  "integer pairs" >:: (fun c ->
     assert_equal  (pow 5 7) 78125;
     assert_equal  (pow 7 5) 16807;
     assert_equal  (int_log 5 78125) 7;
     assert_equal  (int_log 7 16807) 5;
     assert_equal  (make_pairi 5 7) 69984;
     assert_equal  (firsti (make_pairi 5 7)) 5;
     assert_equal  (secondi (make_pairi 5 7)) 7;
     assert_equal  (make_pairi 7 5) 31104;
     assert_equal  (firsti (make_pairi 7 5)) 7;
     assert_equal  (secondi (make_pairi 7 5)) 5
  );

(*** Problem A.5: no tests ***)

  "unary integers" >:: (fun c ->
     assert_equal (prev [()]) [];
     assert_equal (prev [(); (); (); (); ()]) [(); (); (); ()];
     assert_equal (integer_to_unary 0) [];
     assert_equal (integer_to_unary 1) [()];
     assert_equal (integer_to_unary 10) [(); (); (); (); (); (); (); (); (); ()];
     assert_equal (unary_to_integer [(); (); (); (); (); (); (); (); ()]) 9;
     assert_equal (unary_to_integer [(); (); ()]) 3;
     assert_equal (unary_to_integer [()]) 1;
     assert_equal (unary_to_integer []) 0;
     assert_equal (unary_add [(); (); ()] []) [(); (); ()];
     assert_equal (unary_add [] [(); (); ()]) [(); (); ()];
     assert_equal (unary_add [(); ()] [(); (); ()]) [(); (); (); (); ()];
     assert_equal (unary_to_integer 
       (unary_add (integer_to_unary 1001) (integer_to_unary 65535))) 66536;
  );

  "unary integers2" >:: (fun c ->
     assert_equal (prev' (Succ Zero)) Zero;
     assert_equal (prev' (Succ (Succ (Succ (Succ (Succ Zero)))))) 
       (Succ (Succ (Succ (Succ Zero))));
     assert_equal (integer_to_unary' 0) Zero;
     assert_equal (integer_to_unary' 1) (Succ Zero);
     assert_equal (integer_to_unary' 10) 
       (Succ (Succ (Succ (Succ (Succ 
         (Succ (Succ (Succ (Succ (Succ Zero))))))))));
     assert_equal (unary_to_integer' 
       (Succ (Succ (Succ (Succ (Succ 
         (Succ (Succ (Succ (Succ Zero)))))))))) 9;
     assert_equal (unary_to_integer' (Succ (Succ (Succ Zero)))) 3;
     assert_equal (unary_to_integer' (Succ Zero)) 1;
     assert_equal (unary_to_integer' Zero) 0;
     assert_equal (unary_add' (Succ (Succ (Succ Zero))) Zero) 
       (Succ (Succ (Succ Zero)));
     assert_equal (unary_add' Zero (Succ (Succ (Succ Zero)))) 
       (Succ (Succ (Succ Zero)));
     assert_equal (unary_add' (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))) 
       (Succ (Succ (Succ (Succ (Succ Zero)))));
     assert_equal (unary_to_integer'
       (unary_add' (integer_to_unary' 1001) (integer_to_unary' 65535))) 66536;
  );

(*** Problem A.6: no tests ***)

(*** Problem A.7: no tests ***)

(*** Problem B.1 ***)
  "last_sublist" >:: (fun c ->
    assert_raises (Invalid_argument "last_sublist: empty list") (fun () -> last_sublist []);
    assert_equal  (last_sublist [1]) [1];
    assert_equal  (last_sublist [1; 2; 3; 4; 5]) [5]
  );

(*** Problem B.2 ***)

  "reverse" >:: (fun c ->
    assert_equal (reverse []) [];
    assert_equal (reverse [1; 2; 3; 4; 5]) [5; 4; 3; 2; 1];
    assert_equal (reverse [[1; 4]; [9]; [16; 25]]) [[16; 25]; [9]; [1; 4]]
  );

(*** Problem B.3: no tests ***)

(*** Problem B.4: no tests ***)

(*** Problem B.5 ***)

  "count_negative_numbers" >:: (fun c ->
    assert_equal (count_negative_numbers []) 0;
    assert_equal (count_negative_numbers [-2; -1; 0; 1; 2]) 2;
    assert_equal (count_negative_numbers [1; 2; 3; 4; 5]) 0;
    assert_equal (count_negative_numbers [-2; 0; -2; 0; -1; 1]) 3
  );

  "power_of_two_list" >:: (fun c ->
    assert_equal (power_of_two_list 0) [];
    assert_equal (power_of_two_list 1) [1];
    assert_equal (power_of_two_list 5) [1; 2; 4; 8; 16]
  );

  "prefix_sum" >:: (fun c ->
    assert_equal (prefix_sum []) [];
    assert_equal (prefix_sum [1]) [1];
    assert_equal (prefix_sum [1; 2; 3; 4]) [1; 3; 6; 10]
  );

(*** Problem B.6 ***)

  "deep_reverse" >:: (fun c ->
    assert_equal (deep_reverse []) [];
    assert_equal (deep_reverse [[1; 2]; [3; 4]]) [[4; 3]; [2; 1]];
    assert_equal (deep_reverse [[[1; 2]; [3; 4]]; [[5; 6]; [7; 8]]])
     [[[7; 8]; [5; 6]]; [[3; 4]; [1; 2]]]
   );

(*** Problem B.7 ***)

  "deep_reverse_nested" >:: (fun c ->
    assert_equal (deep_reverse_nested (Value 10)) (Value 10);
    assert_equal (deep_reverse_nested 
      (List [Value 10; Value 20; Value 30; Value 40])) 
      (List [Value 40; Value 30; Value 20; Value 10]);
    assert_equal (deep_reverse_nested 
      (List [List [Value 10; Value 20]; List [Value 30; Value 40]])) 
      (List [List [Value 40; Value 30]; List [Value 20; Value 10]]);
    assert_equal (deep_reverse_nested 
      (List [Value 10; List [Value 20; Value 30]])) 
      (List [List [Value 30; Value 20]; Value 10]);
    assert_equal (deep_reverse_nested 
      (List [List [Value 10; Value 20]; Value 30]))
      (List [Value 30; List [Value 20; Value 10]]);
    assert_equal (deep_reverse_nested 
      (List [Value 10; List [Value 20; List [Value 30; Value 40]; Value 50]; Value 60])) 
      (List [Value 60; List [Value 50; List [Value 40; Value 30]; Value 20]; Value 10])
  );

]

let _ = run_test_tt_main all_tests

