open Num

(* A.1 *)

(* Assuming that OCaml is using applicative-order evaluation, then
the space complexity of the tree-recursive fibonacci function is O(n).
This is because the maximum depth of the recursion tree is n. Once the
recursion tree gets to this maximum depth, it begins returning values 
back up the tree, which makes it so that there are less operations 
pending because their recursive calls have been evaluated and thus,
they themselves will have been evaluated. The space complexity is 
different from the time complexity because the time complexity looks at 
all of the function calls made (which is two per recursive call) and 
the time that each function call takes. However, the space complexity
only looks at how deep our recursive tree gets (how many pending 
operations there are in the worst case and how many pieces of data that 
are being stored, waiting for a returned value), which is only n in
the worst case because the base cases will allow operations to be
evaluated after this point, and thus no longer pending. *)

(* A.2.a *)

(* The function p is applied 5 times when sine 12.15 is evaluated. *)
(* The order of growth in space is the ceiling of log base 3 of 10 * a. The 
reasoning behind this is that log returns how many times the base must 
divide the number passed to the log to get 1. Since our function goes
until angle < 0.1, or 1 / 10, we must multiply a by 10 to get the number 
of times 3 must divide into a to get the angle below 0.1. Furthermore,
we need the ceiling of this function because we can only divide by a
whole 3, not a fraction of 3. The order of growth in space is exactly 
this value because this is how many pending operations there are.
The time complexity is O(log a) since the angle is divided by a constant
value every recursive call. *)

(* A.3.1 *)

let rec fast_expt b n = 
	let is_even m = m mod 2 = 0 in
	let square m = m * m in 
		match n with 
		| 0 -> 1
		| w when (is_even w ) -> square (fast_expt b (w / 2))
		| _ -> b * fast_expt b (n - 1)
		
(* A.3.2 *)
		
let ifast_expt b n =
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
	let rec iter b n a = 
	match n with 
	| 0 -> a
	| w when is_even w -> (iter (square b) (n / 2) a)
	| _ -> iter b (n - 1) (b * a) in
	iter b n 1

(* A.4 *)

let rec fast_mult a b = 
	let is_even m = m mod 2 = 0 in
	let double m = 2 * m in 
	let halve m = m / 2 in 
		match b with
		| 0 -> 0
		| w when is_even w -> double (fast_mult a (halve b))
		| _ -> a + fast_mult a (b - 1)
		
(* A.5 *)

let ifast_mult a b = 
	let is_even m = m mod 2 = 0 in
	let double m = 2 * m in 
	let halve m = m / 2 in 
	let rec iter a b total = 
	match b with
	| 0 -> total
	| w when is_even w -> iter (double a) (halve b) total
	| _ -> iter a (b - 1) (total + a) in
	iter a b 0
	
(* A.6 *)

(* Worst case time complexity is O(2^log n). There are two recursive
calls each call, but they are called on half of n. The space complexity
is O(log n) since n is divided by 2 each time and the stored memory
will be returned so the maximum depth of the stack is log n, and this
is the worst case space complexity. *)

(* A.7.1 *)

(* This is a linear recursive function because it makes a recursive 
call that is not a tail call each iteration and decrements the input by 
1 each call. It is recursive because higher level calls have to wait
for the lower level calls to return before they can return. It is linear
because it increases the number of calls linearly with the input size.
*)

(* A.7.2 *)

(* The time complexity is O(n) since the number of calls scales linearly
with the input size. The space complexity is also O(n) since every 
recursive call must wait for the next call (as a pending operation) to
return before it can return. *)

(* B.1.a *)

(* (fun x y -> (x * (2 + y))) 20 (2 * 4) *)

(* B.1.b *)

(* (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0 *)

(* B.1.c *)

(* (fun x -> let y = 2 in let z = 3 in x * y * z) 1
	(fun x -> (fun y -> let z = 3 in x * y * z) 2) 1
	(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1 *)
	
(* B.1.d *)

(* (fun x -> let x = 2 in let x = 3 in x * x * x) 1
	(fun x -> (fun x -> let x = 3 in x * x * x) 2) 1
	(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1 *)
	
(* B.2 *)

(* 
let x = 2 * 10
and y = 3 + 4
in 
	let y = 14 in 
		let z = 22 in 
			x * y * z
			
(fun x y -> let y = 14 in
				let z = 22 in
					x * y * z) (2 * 10) (3 + 4)
					
(fun x y -> (fun y -> let z = 22 in
						x * y * z) 14) (2 * 10) (3 + 4)
						
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)

Evaluate 2 * 10 -> 20
Evaluate 3 + 4 -> 7

(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) 20 7

(fun x y -> (fun y -> x * y * 22) 14) 20 7

Evaluate 14 * 22 = 308

(fun x y -> x * 308) 20 7

Evaluate 20 * 308 = 6160

*)

(* B.3 *)

(* Desugaring this, we get:

(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

x and y have yet to be bound to a value, so they can not be passed as arguments
to the function. Due to the usual applicative-order evaluation rule,
the arguments will be evaluated first, and ocaml doesn't have any set
value for x or y yet. To fix this, we simply will use the keyword "in"
more effectively to create nested lets.

let x = 10 in
y = x * 2 in 
z = y + 3 in 
	x + y + z
*)

let ni = num_of_int 

(* C.1 *)	
	
let isum term a next b = 
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (result +/ term a)
	in
		iter a (ni(0))
		
(* C.2 *)

let rec product_rec term a next b =
	if a >/ b
		then (ni 1)
		else term a */ (product_rec term (next a) next b)
		
let factorial_rec n =
	product_rec (fun x -> x) (ni(1)) (fun x -> x +/ (ni(1))) n
	
let product_iter term a next b =
	let rec iter a result = 
		if a >/ b
			then result
			else iter (next a) (result */ term a)
	in 
		iter a (ni(1))

let factorial_iter n =
	product_iter (fun x -> x) (ni(1)) (fun x -> x +/ (ni(1))) n
	
let pi_product n = 
	ni(4) */ product_rec (fun x -> 
						if (int_of_num x) mod 2 = 0 
						then (x +/ (ni(2))) // (x +/ (ni(1)))
						else (x +/ (ni(1))) // (x +/ (ni(2)))) (ni(1))
						 (fun x -> x +/ (ni(1))) n

let pi_approx = 
	float_of_num (pi_product (ni(1000)))
	
(* C.3 *)

let rec accumulate_rec combiner null_value term a next b = 
	if a >/ b
	then null_value
	else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)
	
let accumulate_iter combiner null_value term a next b = 
	let rec iter combiner a result = 
		if a >/ b
			then result 
			else iter combiner (next a) (combiner result (term a))
	in
	iter combiner a null_value
	
let next = fun x -> x +/ (ni 1)
let term = fun x -> x
	
let sum term a next b = 
	accumulate_rec (+/) (ni 0) term a next b
	
	
let product term a next b = 
	accumulate_rec ( */ ) (ni 1) term a next b
	
(* C.4 *)

let compose f1 f2 =  
	fun x -> f1 (f2 x)
	
(* C.5 *)

let rec repeated f n = 
	if n = 0 then fun x -> x
	else compose f (repeated f (n - 1)) 
	
(* C.6 *)

let smooth f dx = 
	fun x -> (f (x -. dx) +. f(x) +. f (x +. dx)) /. (3.0)
	
let nsmoothed f n dx = 
	let smooth_dx f = smooth f dx in
	(repeated smooth_dx n) f 
	
(* D.1 *)

let upper_bound x = 
	int_of_float (sqrt(float_of_int x))




let is_prime x = 
	let upper = upper_bound x in
	let rec iter a = 
	match a with
	| y when y > upper -> true
	| y when x mod y = 0 -> false
	| _ -> iter (a + 1) 
	in 
	if x < 2 then false else iter 2

	

	
(* D.2 *)

let smallest_prime_factor x = 
	let rec iter a = 
	if is_prime a = true && x mod a = 0 then a
	else iter (a + 1) in 
	if is_prime x = true || x < 2 
	then invalid_arg "This number is prime" else iter 2
	

	
