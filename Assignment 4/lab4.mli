(* lab4.mli *)

(* A.1 *)
type mobile = Mobile of branch * branch
and branch = Weight of int * int | Structure of int * mobile
val make_mobile : branch -> branch -> mobile
val make_weight : int -> int -> branch
val make_structure : int -> mobile -> branch
val left_branch : mobile -> branch
val right_branch : mobile -> branch
val branch_length : branch -> int
val branch_structure : branch -> [> `Structure of mobile | `Weight of int ]
val branch_weight1 : branch -> int
val total_weight1 : mobile -> int
val branch_weight2 : branch -> int
val total_weight2 : mobile -> int
val is_balanced : mobile -> bool
type mobile' = { left : branch'; right : branch'; }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'
val make_mobile' : branch' -> branch' -> mobile'
val make_weight' : int -> int -> branch'
val make_structure' : int -> mobile' -> branch'
val left_branch' : mobile' -> branch'
val right_branch' : mobile' -> branch'
val branch_length' : branch' -> int
val branch_structure' : branch' -> [> `Structure of mobile' | `Weight of int ]
val branch_weight' : branch' -> int
val total_weight' : mobile' -> int
val is_balanced' : mobile' -> bool

(* A.2 *)

type tree = Tree of elem list
and elem = Num of int | Sub of tree
val square_tree : tree -> tree
val square_tree' : tree -> tree

(* A.3 *)

val tree_map : (int -> int) -> tree -> tree

(* A.4 *)

val subsets : 'a list -> 'a list list

(* A.5 *)

val accumulate : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val map : ('a -> 'b) -> 'a list -> 'b list
val append : 'a list -> 'a list -> 'a list
val length : 'a list -> int

(* A.6 *)

val accumulate_n : ('a -> 'b -> 'b) -> 'b -> 'a list list -> 'b list

(* A.7 *)

val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val dot_product : int list -> int list -> int
val matrix_times_vector : int list list -> int list -> int list
val transpose : 'a list list -> 'a list list
val matrix_times_matrix : int list list -> int list list -> int list list

(* B.1 *)

val quicksort : 'a list -> ('a -> 'a -> bool) -> 'a list

(* B.4 *)

val insertion_sort : 'a list -> ('a -> 'a -> bool) -> 'a list

(* C.1 *)

type expr =
    Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int

val simplify1 : expr -> expr

(* C.2 *)

val deriv : expr -> string -> expr

