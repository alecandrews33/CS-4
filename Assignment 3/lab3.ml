(* A.1 *)

type point = { x : float; y : float }
let make_point x_value y_value = {x = x_value; y = y_value}
let get_coords {x = x_value; y = y_value} = (x_value, y_value)

type segment = { startp: point; endp : point }
let make_segment p1 p2 = 
	{startp = p1; endp = p2}
let get_points {startp = p1 ; endp = p2} = (p1, p2)

let midpoint_segment seg = 
	let points = get_points seg in
	let x1 = fst (get_coords (fst points)) in
	let x2 = fst (get_coords (snd points)) in
	let y1 = snd (get_coords (fst points)) in
	let y2 = snd (get_coords (snd points)) in
	make_point ((x1 +. x2) /. 2.0) 
				((y1 +. y2) /. 2.0)


let segment_length seg = 
	let points = get_points seg in
	let x1 = fst (get_coords (fst points)) in
	let x2 = fst (get_coords (snd points)) in
	let y1 = snd (get_coords (fst points)) in
	let y2 = snd (get_coords (snd points)) in
	let square x = x *. x in
		sqrt (square (x1 -. x2) 
			+. square (y1 -. y2))
	

let print_point {x = x_value; y = y_value} = 
	Printf.printf "(%g, %g)" x_value y_value

(* A.2 *)

type rectangle = {lower_left : point; upper_right : point}
let make_rectangle p1 p2 =
	{lower_left = p1; upper_right = p2}
let rectangle_lower_segment {lower_left = p1; upper_right = p2} =
	make_segment p1 (make_point (fst (get_coords p2)) (snd (get_coords p1)))
let rectangle_upper_segment {lower_left = p1; upper_right = p2} = 
	make_segment (make_point (fst (get_coords p1)) (snd (get_coords p2))) p2
let rectangle_left_segment {lower_left = p1; upper_right = p2} = 
	make_segment (make_point (fst (get_coords p1)) (snd (get_coords p2))) p1
let rectangle_right_segment {lower_left = p1; upper_right = p2} = 
	make_segment p2 (make_point (fst (get_coords p2)) (snd (get_coords p1))) 

let rectangle_perimeter rect = 
	segment_length (rectangle_lower_segment rect) +.
	segment_length (rectangle_upper_segment rect) +.
	segment_length (rectangle_left_segment rect) +.
	segment_length (rectangle_right_segment rect)

let rectangle_area rect = 
	segment_length (rectangle_lower_segment rect) *.
	segment_length (rectangle_right_segment rect)

type rectangle2 = {lower_x : float; upper_x : float; lower_y : float; upper_y : float}
let make_rectangle2 x1 x2 y1 y2 =
	{lower_x = x1; upper_x = x2; lower_y = y1; upper_y = y2}
let rectangle_lower_segment2 {lower_x = x1; upper_x = x2; lower_y = y1; upper_y = y2} =
	make_segment (make_point x1 y1) (make_point x2 y1)
let rectangle_upper_segment2 {lower_x = x1; upper_x = x2; lower_y = y1; upper_y = y2} =
	make_segment (make_point x1 y2) (make_point x2 y2)
let rectangle_left_segment2 {lower_x = x1; upper_x = x2; lower_y = y1; upper_y = y2} =
	make_segment (make_point x1 y1) (make_point x1 y2)
let rectangle_right_segment2 {lower_x = x1; upper_x = x2; lower_y = y1; upper_y = y2} =
	make_segment (make_point x2 y2) (make_point x2 y1)

let rectangle_perimeter2 rect = 
	segment_length (rectangle_lower_segment2 rect) +.
	segment_length (rectangle_upper_segment2 rect) +.
	segment_length (rectangle_left_segment2 rect) +.
	segment_length (rectangle_right_segment2 rect)

let rectangle_area2 rect = 
	segment_length (rectangle_lower_segment2 rect) *.
	segment_length (rectangle_right_segment2 rect)

(* A.3 *)

let make_pair x y = fun m -> m x y

let first z = z (fun x y -> x)

let second z = z (fun x y -> y)

(* 
first (make_pair x y)
	evaluate make_pair x y (argument to first)
	first (fun m -> m x y)
	evaluate first z to fun z -> z (fun x y -> x)
	fun (fun x y -> x) -> (fun x y -> x) x y -> x


second (make_pair 1 2)
	evaluate make_pair 1 2
		evaluate 1 -> 1
		evaluate 2 -> 2
		evaluate make_pair to fun x y -> (fun m -> m x y)
		apply fun x y ... to 1, 2
		fun m -> m 1 2
	evaluate second to fun z -> z (fun x y -> y)
	apply fun z ... to fun m -> m 1 2
		(fun m -> m 1 2) (fun x y -> y)
		apply fun m -> m 1 2 to fun x y -> y
			(fun x y -> y) 1 2
			evaluate 1 -> 1
			evaluate 2 -> 2
			apply fun x y ... to 1, 2
				result is 2

*)

(* A.4 *)

let pow a b = 
	let rec iter a b current =
	if b = 0 then current else
	iter a (b - 1) (current * a) in
	iter a b 1

let int_log a b = 
	let rec iter a b current =
	match b with 
	| 1 -> current 
	| y when y mod a = 0 -> iter a (b / a) (current + 1)
	| _ -> current
	in iter a b 0

let make_pairi a b = 
	pow 2 a * pow 3 b

let firsti a = 
	int_log 2 a

let secondi a = 
	int_log 3 a

(* A.5 *)

let zero = []

let succ u = () :: u

let is_zero = 
	function
	| [] -> true
	| () :: _ -> false

let prev u = 
	match u with
	| [] -> invalid_arg "You cannot get the previous of 0."
	| h :: t -> t

let integer_to_unary n = 
	let rec iter count ulist =
		if count = 0 then ulist 
		else iter (count - 1) (succ ulist)
	in iter n []

let unary_to_integer ulist = 
	let rec iter count lst = 
		if is_zero lst then count
		else iter (count + 1) (prev lst)
	in iter 0 ulist

let unary_add ulist1 ulist2 = 
	let rec iter lst1 lst2 =
		if is_zero lst1 then lst2
		else iter (prev lst1) (succ lst2)
	in iter ulist1 ulist2

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = 
	function
	| Zero -> true
	| Succ _ -> false

let succ' u = Succ u

let prev' u = 
	match u with
	| Zero -> invalid_arg "You cannot get the previous of 0."
	| Succ x -> x 

let rec integer_to_unary' = function
	| 0 -> zero'
	| n -> succ' (integer_to_unary' (n - 1))

let rec unary_to_integer' = function
	| Zero -> 0
	| Succ x -> 1 + unary_to_integer' x

let rec unary_add' a b = 
	match a with
	| Zero -> b
	| Succ x -> unary_add' x (succ' b)

(* The definitions really don't change much from the previous representation. 
	They are still recursive or iterative and look through the lists one at a time
	or decrement a counter until the base case of 0 is reached.*)

(* A.6 *)

let zerof s z = z

let add1 n s z = s (n s z) 

let one s z = s z

let two s z = s (s z)

let three s z = s (s (s z))

let four s z = s (s (s (s z)))

let five s z = s (s (s (s (s z))))

let six s z = s (s (s (s (s (s z)))))

let seven s z = s (s (s (s (s (s (s z))))))

let eight s z = s (s (s (s (s (s (s (s z)))))))

let nine s z = s (s (s (s (s (s (s (s (s z))))))))

let ten s z = s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)

let church_to_integer m = 
	m (fun x -> x + 1) 0

(* A.7 *)

(* For zerof, 'a must be (int -> int), and this is reasonable
because it takes in the s function (which acts as a successor 
function that gets the successor of an int). 'b next must be int, and
this makes the returned value an int. *)

(* For one, ('a -> 'b) is (int -> int), so 'a must be an int, and 'b, 
what we are returning, has already been defined as an int, so this
is the type that is returned. Although church_to_integer returns
'a, by setting the types earlier, it ensures that in these cases
an int is returned. *)

(* B.1 *)

let rec last_sublist = function
	| [] -> invalid_arg "last_sublist: empty list"
	| [x] -> [x]
	| h::t -> last_sublist t

(* B.2 *)

let reverse lst = 
	let rec iter lst1 new_list = 
		match lst1 with
		| [] -> new_list
		| h::t -> iter t (h :: new_list)
	in iter lst []

(* B.3 *)

let rec square_list = function 
	| [] -> []
	| h::t -> (h * h) :: square_list t


let square_list2 items = 
	let square x = x * x in
	List.map square items


(* B.4 *)

(* His answer list is in reverse order because each value that
 is appended is appended to the head of the answer list. Thus, since we
 iterate forward throught the input list, our answer list will be in reverse
order of our desired answer list. *)

(* This doesn't work because the :: constructor appends the first argument to 
the second argument, but the second argument must be a list. In this case, he is
trying to append to an integer, which does not work. *)

let square_list items = 
	let rec iter things answer =
		match things with 
		| [] -> answer
		| h :: t -> iter t (answer @ [h * h])
	in iter items []

(* This resulting function isn't very efficient. For each value being 
added, one must copy every value over from the answer list, which takes 
O(n) time, resulting in a total time complexity of O(n^2) *)

(* B.5.1 *)

let count_negative_numbers lst =
	let rec iter lst1 count = 
	match lst1 with
	| [] -> count
	| h :: t -> if h < 0 then iter t (count + 1)
	else iter t count
	in iter lst 0

(* B.5.2 *)

let power_of_two_list n = 
	let rec iter num lst = 
	if num < 0 then lst else iter (num - 1) ((pow 2 num) :: lst)
in iter (n - 1) []

(* B.5.3 *)

let prefix_sum lst =
	let rec iter sum lst1 lst2 =
	match lst1 with 
	| [] -> List.rev lst2
	| h :: t -> iter (sum + h) t ((sum + h) :: lst2)
in iter 0 (lst) []

(* B.6 *)

let deep_reverse lst = 
	let rec iter lst1 lst2 =
	match lst1 with 
	| [] -> lst2
	| h :: t -> iter t ((reverse h) :: lst2)
in iter lst []

(* B.7 *)
type 'a nested_list = Value of 'a | List of 'a nested_list list

let deep_reverse_nested lst = 
	let rec iter lst1 lst2 = 
	match lst1 with 
	| [] -> lst2
	| Value v :: t -> iter t (Value v :: lst2)
	| List l :: t ->  iter t (List (iter l []) :: lst2)
in 
	match lst with
	| Value v -> Value v
	| List l -> List (iter l [])








