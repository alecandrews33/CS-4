(* Interface file for lab3.ml *)

(* A.1 *)

type point
type segment
val make_point : float -> float -> point
val get_coords : point -> float * float
val make_segment : point -> point -> segment
val get_points : segment -> point * point
val midpoint_segment : segment -> point 
val segment_length : segment -> float
val print_point : point -> unit

(* A.2 *)

type rectangle
val rectangle_lower_segment : rectangle -> segment
val rectangle_upper_segment : rectangle -> segment
val rectangle_left_segment : rectangle -> segment
val rectangle_right_segment : rectangle -> segment

type rectangle2
val rectangle_lower_segment2 : rectangle2 -> segment
val rectangle_upper_segment2 : rectangle2 -> segment
val rectangle_left_segment2 : rectangle2 -> segment
val rectangle_right_segment2 : rectangle2 -> segment
val rectangle_perimeter : rectangle -> float
val rectangle_perimeter2 : rectangle2 -> float
val rectangle_area : rectangle -> float
val rectangle_area2 : rectangle2 -> float
val make_rectangle : point -> point -> rectangle
val make_rectangle2 : float -> float -> float -> float -> rectangle2

(* A.3 *)

val make_pair : 'a -> 'b -> ('a -> 'b -> 'c) -> 'c
val first : (('a -> 'b -> 'a) -> 'c) -> 'c
val second : (('a -> 'b -> 'b) -> 'c) -> 'c

(* A.4 *)

val pow : int -> int -> int
val int_log : int -> int -> int
val make_pairi : int -> int -> int
val firsti : int -> int
val secondi : int -> int

(* A.5 *)

val zero : unit list
val is_zero : unit list -> bool
val succ : unit list -> unit list
val prev : unit list -> unit list
val integer_to_unary : int -> unit list
val unary_to_integer : unit list -> int
val unary_add : unit list -> unit list -> unit list 

type nat = Zero | Succ of nat
val zero' : nat
val is_zero' : nat -> bool
val succ' : nat -> nat
val prev' : nat -> nat
val integer_to_unary' : int -> nat
val unary_to_integer' : nat -> int
val unary_add' : nat -> nat -> nat

(* A.6 *)

val zerof : 'a -> 'b -> 'b
val add1 : (('a -> 'b) -> 'c -> 'a) -> ('a -> 'b) -> 'c -> 'b
val one : ('a -> 'b) -> 'a -> 'b
val two : ('a -> 'a) -> 'a -> 'a
val three : ('a -> 'a) -> 'a -> 'a
val four : ('a -> 'a) -> 'a -> 'a
val five : ('a -> 'a) -> 'a -> 'a
val six : ('a -> 'a) -> 'a -> 'a
val seven : ('a -> 'a) -> 'a -> 'a
val eight : ('a -> 'a) -> 'a -> 'a
val nine : ('a -> 'a) -> 'a -> 'a
val ten : ('a -> 'a) -> 'a -> 'a
val add : ('a -> 'b -> 'c) -> ('a -> 'd -> 'b) -> 'a -> 'd -> 'c
val church_to_integer : ((int -> int) -> int -> 'a) -> 'a



(* B.1 *)

val last_sublist : 'a list -> 'a list

(* B.2 *)

val reverse : 'a list -> 'a list

(* B.3 *)

val square_list : int list -> int list
val square_list2 : int list -> int list

(* B.4 *)

(* No code to write. *)

(* B.5 *)

val count_negative_numbers : int list -> int
val power_of_two_list : int -> int list
val prefix_sum : int list -> int list

(* B.6 *)

(* B.7 *)

val deep_reverse : 'a list list -> 'a list list

(* B.8 *)

type 'a nested_list = Value of 'a | List of 'a nested_list list
val deep_reverse_nested : 'a nested_list -> 'a nested_list

