(* A.1.a *)

type mobile = Mobile of branch * branch
and branch = Weight of int * int | Structure of int * mobile

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch (Mobile (l, r)) = l

let right_branch (Mobile (l, r)) = r

let branch_length b = 
	match b with
	| Weight (l, w) -> l
	| Structure (l, m) -> l

let branch_structure b = 
	match b with
	| Weight (l, w) -> `Weight w
	| Structure (l, m) -> `Structure m

(* A.1.b *)

let rec branch_weight1 b = 
	match b with
	| Weight (l, w) -> w
	| Structure (l, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) = 
	(branch_weight1 l + branch_weight1 r)

let rec branch_weight2 b = 
	match (branch_structure b) with
	| `Weight w -> w
	| `Structure m -> total_weight2 m
and total_weight2 m = 
	(branch_weight2 (left_branch m) + 
	branch_weight2 (right_branch m))

(* A.1.c *)

let rec is_balanced m = 
	let rec iter b = 
	match (branch_structure b) with
	| `Weight w -> true
	| `Structure m -> is_balanced m
in if iter (left_branch m) && iter (right_branch m) &&
	(branch_length (left_branch m) * branch_weight1 (left_branch m)) 
	= (branch_length (right_branch m) * branch_weight1 (right_branch m)) 
	then true 
	else false

(* A.1.d *)

type mobile' = { left : branch'; right : branch'; }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = 
	{left = l ; right = r}

let make_weight' l w = 
	Branch' (l, Weight' w)

let make_structure' l m = 
	Branch' (l, Structure' m)

let left_branch' {left : branch'; right : branch'} = 
	left

let right_branch' {left : branch'; right : branch'} = 
	right

let branch_length' (Branch' (l, _)) = l

let branch_structure' (Branch'(l, d)) = 
	match d with
	| Weight' w -> `Weight w
	| Structure' m -> `Structure m

let rec branch_weight' b = 
	match (branch_structure' b) with
	| `Weight w -> w
	| `Structure m -> total_weight' m
and total_weight' m = 
	(branch_weight' (left_branch' m) + 
	branch_weight' (right_branch' m))

let rec is_balanced' m = 
	let rec iter b = 
	match (branch_structure' b) with
	| `Weight w -> true
	| `Structure m -> is_balanced' m
	in if iter (left_branch' m) && iter (right_branch' m) &&
	(branch_length' (left_branch' m) * branch_weight' (left_branch' m)) 
	= (branch_length' (right_branch' m) * branch_weight' (right_branch' m)) 
	then true 
	else false







(* A.2 *)

type tree = Tree of elem list
and elem = Num of int | Sub of tree

let rec square_tree (Tree items) = 
	let square x = x * x in 
	let rec iter things = 
		match things with
		| [] -> []
		| Num n :: t -> Num (square n) :: iter t
		| Sub s :: t -> Sub (square_tree s) :: iter t
	in Tree (iter items) 

let rec square_tree' (Tree items) = 
	let square x = x * x in 
	let transform el = 
	match el with
	| Sub s -> Sub (square_tree' s)
	| Num n -> Num (square n)
in Tree (List.map transform items)

(* A.3 *)

let rec tree_map change (Tree items) = 
	let transform el = 
	match el with
	| Sub s -> Sub (tree_map change s)
	| Num n -> Num (change n)
in Tree (List.map transform items)

(* A.4 *)

let rec subsets = function
	| [] -> [[]]
	| h :: t -> let rest = subsets t in
		rest @ (List.map (fun x -> h :: x) rest)

(* This function is recursive, and will keep breaking up the input list
into a head and a tail until the tail is an empty list, when it will 
return an empty list inside of a list. At each step with head and tail,
the subsets of the tail are computed, and then the function returns 
all of those subsets added to the same subsets with the head appended 
to the front of them. Thus, the function will have recursive calls 
all the way down until rest = [[]]. At this point, it will start 
returning to previous recursive calls. The first
step back up the recursive tree would be [[]] @ [[h]] = [[], h]. This
process would continue back up the tree, adding all possible subsets
of the tail with those same subsets along with the head. This works
because all subsets of the tail are also distinct subsets when the
head is added since they are distinct without the head. *)

(* A.5 *)

let rec accumulate op initial sequence =
	match sequence with
	| [] -> initial
	| h :: t -> op h (accumulate op initial t)

let map p sequence = 
	accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 = 
	accumulate (fun x r -> x :: r) seq2 seq1

let length sequence = 
	accumulate (fun x r -> r + 1) 0 sequence

(* A.6 *)

let rec accumulate_n op init seqs = 
	match seqs with 
	| [] -> failwith "empty list"
	| [] :: _ -> [] 
	| h :: t -> accumulate op init (List.map List.hd seqs) :: 
	accumulate_n op init (List.map List.tl seqs)

(* A.7 *)

let rec map2 f x y = 
	match (x, y) with
	| ([], []) -> []
	| ([], _) -> failwith "unequal lists"
	| (_, []) -> failwith "unequal lists"
	| (h::t, s::b) -> f h s :: map2 f t b

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = List.map (fun x -> dot_product v x) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

let matrix_times_matrix m n = 
	let cols = transpose n in 
		List.map (fun x -> matrix_times_vector cols x) m

(* B.1 *)

let rec quicksort items cmp = 
	match items with
	| [] -> []
	| h :: t -> let lower = List.filter (fun x -> cmp x h) t in
	let higher = List.filter (fun x -> not (cmp x h)) t in 
	(quicksort lower cmp) @ [h] @ (quicksort higher cmp)

(* B.2 *)

(* This quicksort function is an instance of generative recursion
since we are recursing on subparts of the input that we created.
We created new lists to store values above and below the pivot and 
recursed on them. These weren't natural parts of the input data. *)

(* B.3 *)

(* Taking out the one element base case forces the function to sort
the one element lists, but they are already sorted. This creates many
unnecessary recursive calls and results in a stack overflow. *)

(* B.4 *)

let rec insert_in_order new_result a_list cmp = 
	match a_list with 
		| [] -> [new_result]
		| h :: t when cmp new_result h -> new_result :: a_list
		| h :: t -> h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp = 
	match a_list with 
	| [] -> []
	| h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* This is structural recursion since we recurse on the natural
subparts of the data (first element and rest). *)

(* C.1 *)

type expr =
    Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Pow of expr * int

let rec simplify1 expr =
	let pow a b = 
	let rec iter a b current =
	if b = 0 then current else
	iter a (b - 1) (current * a) in
	iter a b 1 in
	match expr with 
	| Int x -> Int x
	| Var z -> Var z
	| Add (Int 0, x) -> x
	| Add (x, Int 0) -> x
	| Add (Int x, Int y) ->Int (x + y)
	| Add (x, y) -> Add (simplify1 x, simplify1 y)
	| Mul (Int 0, _) -> Int 0
	| Mul (_, Int 0) -> Int 0 
	| Mul (Int 1, x) -> x
	| Mul (x, Int 1) -> x
	| Mul (Int x, Int y) -> Int (x * y)
	| Mul (x, y) -> Mul (simplify1 x, simplify1 y)
	| Pow (_, 0) -> Int 1
	| Pow (x, 1) -> x
	| Pow (Int x, y) -> Int (pow x y)
	| Pow (x, y) -> Pow (simplify1 x, y)

let rec simplify expr =
let e = simplify1 expr in
  if expr = e
    then expr
    else simplify e

(* C.2 *)



let rec deriv expr str = 
	match expr with 
	| Int x -> Int 0
	| Var x when x = str -> Int 1
	| Var _ -> Int 0
	| Add (x, y) -> Add (deriv x str, deriv y str)
	| Mul (x, y) -> let dy = deriv y str and dx = deriv x str in
	Add (Mul(dy, x), Mul (y, dx))
	| Pow (x, y) -> Mul (Mul (Int y, Pow (x, y - 1)), deriv x str)
	
let derivative expr var =
    let e = simplify expr in
    let d = deriv e var in
      simplify d
