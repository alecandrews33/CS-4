(* A.1 *)

(* 1. int = 10 *)

(* 2. float = 10 *)

(* 3. int = 12 *)

(* 4. This expression has type float but an expression was expected
of type int - The operator used is for ints while the numbers used
were floats *)

(* 5. This expression has type int but an expression was expected 
of type float - We used an operator for floats with ints *)

(* 6. This expression has type float but an expression was expected of
 type int - We used an operator for ints with a float as an argument *)
 
(* 7. This expression has type int but an expression was expected of 
type float - We used an operator for floats with an int as an 
argument *)

(* 8. float = 7.2 *)

(* 9. int = 5 *)

(* 10. int = 7 *)

(* 11. val a : int = 3 *)

(* 12. val b : int = 4 *)

(* 13. bool = false *)

(* 14. bool = true *)

(* 15. bool = false - '=' means "structurally equal" while '==' means
"identical" (* exact same object in memory *) *)

(* 16. (int * int * int) list = [(1, 2, 3)] *)

(* 17. (int * int * int) list = [(1, 2, 3)]  Lists are supposed
to use semicolons to separate their data. This example used commas,
and it appears that ocaml interpreted it as a list of a tuple since
commas are used to separate data in a tuple. *)

(* 18. int = 4 *)

(* 19. Syntax error - and can not be used, one must use && *)

(* 20. int = 6 *)

(* 21. This expression has type int but an expression was expected of 
type unit - The then and else clause expressions must have the same type
but leaving off the else clause puts an implicit else () at the end
of the if expression *)

(* A.2 *)

(* This function takes three integer numbers as arguments and returns 
the squares of the two larger numbers *)

let sum_of_squares_of_two_largest x y z =
	if x < y && x < z then (y * y + z * z) else
		(x * x + if y > z then (y * y) else (z * z))
		
(* A.3 *)

(* This function checks to see if the argument b is greater than 0. If 
it is, then it returns the sum of a and b. If it is not, then it returns 
a - b. This effectively returns a plus the absolute value of b. *)

(* B.1 *)

(* With an interpreter that uses normal-order evaluation, this 
expression would first evaluate the predicate expression (if x = 0), and 
to do this, it must evaluate x. Thus, it will evaluate x to 0, the if
statement will be True and the then statement will be entered, returning
a 0. However, with an interpreter that uses applicative-order evaluation
this expression would first evaluate the two passed arguments to test.
x would evaluate to 0, while y would enter an infinite recursive call.
With y in an infinite recursive call, this expression won't be able to
return anything. *)

(* B.2 *)

(* A stack overflow occurs during evaluation due to looping recursion.
In the built in if function, when the predicate expression evaluates
to either true or false, it is replaced with either the then or else
expression respectively. Thus, even in a recursive function like this 
one where the else expression is a recursive call, there is only one
function call being evaluated at a time. In the new_if function, the 
pattern matching does not replace the predicate expression with the 
corresponding expression, but rather evaluates the corresponding 
expression. In this case, when false, it evaluates another recursive 
call of sqrt_iter, which adds more and more function calls onto the 
stack waiting for some returned values, which ends up in a stack 
overflow. *)

(* B.3 *)

(*  add_a 2 5
	evaluate 2 -> 2
	evaluate 5 -> 5
	evaluate add_a -> 
		fun a b -> if a = 0 then b else inc (add_a (dec a) b)
	apply fun a b -> (...) to 2 5
		(* substitute 2 for a, 5 for b in fun expression to get: *)
		if 2 = 0 then 5 else inc (add_a (dec 2) 5)
		(* evaluate predicate expression *)
			evaluate 2 -> 2
			evaluate 0 -> 0
			evaluate = -> =
			apply = to 2, 0 -> false
		(* since expression is false, replace with false clause: *)
		inc (add_a (dec 2) 5)
			evaluate add_a (dec 2) 5
				evaluate dec 2
					evaluate 2 -> 2
					evaluate dec -> dec
					apply dec to 2 -> 1
				evaluate 5 -> 5
				evaluate add_a ->
					fun a b -> if a = 0 
								  then b 
								  else inc (add_a (dec a) b)
				apply fun a b (...) to 1, 5
					(* substitute 1 for a, 5 for b in fun expression to
					get: *)
					if 1 = 0 then 5 else inc (add_a (dec 1) 5)
					(* evaluate predicate expression *)
						evaluate 1 -> 1
						evaluate 0 -> 0
						evaluate = -> =
						apply = to 1, 0 -> false
					(* since expression is false, replace with 
					false clause: *)
					inc (add_a (dec 1) 5)
						evaluate add_a (dec 1) 5
							evaluate dec 1
							evaluate 1 -> 1
							evaluate dec -> dec
							apply dec to 1 -> 0
						evaluate 5 -> 5
						evaluate add_a ->
							fun a b -> if a = 0
										  then b
										  else inc (add_a (dec a) b)
						apply fun a b (...) to 0, 5
							(* substitute 0 for a, 5 for b in fun 
							expression to get: *)
							if 0 = 0 then 5 else inc (add_a (dec 0) 5)
							(* evaluate predicate expression *)
								evaluate 0 -> 0
								evaluate 0 -> 0
								evaluate = -> =
								apply = to 0, 0 -> true
							(* since expression is true, replace with 
							true clause: *)
							result: 5
							(* add_a (dec 1) 5 -> 5 *)
					(* Now we have inc (5) *)
					evaluate inc -> inc
					apply inc to 5 -> 6
					(* add_a (dec 2) 5 -> 6 *)
		(* Now we have inc (6) *)
		evaluate inc -> inc
		apply inc to 6 -> 7
	(* add_a 2 5 -> 7 *)
	final result: 7 *)
	
(*  add_b 2 5
	evaluate 2 -> 2
	evaluate 5 -> 5
	evaluate add_b -> 
		fun a b -> if a = 0 then b else add_b (dec a) (inc b)
	apply fun a b (...) to 2, 5
		(* substitute 2 for a, 5 for b in fun expression to get: *)
		if 2 = 0 then 5 else add_b (dec 2) (inc 5)
		(* evaluate predicate expression *)
			evaluate 2 -> 2
			evaluate 0 -> 0
			evaluate = -> =
			apply = to 2, 0 -> false
		(* since expression is false, replace with false clause *)
	add_b (dec 2) (inc 5)
		evaluate dec 2
			evaluate 2 -> 2
			evaluate dec -> dec
			apply dec to 2 -> 1
		evaluate inc 5
			evaluate 5 -> 5
			evaluate inc -> inc
			apply inc to 5 -> 6
		evaluate add_b -> 
			fun a b -> if a = 0 then b else add_b (dec a) (inc b)
	apply fun a b (...) to 1, 6
		(* substitute 1 for a, 6 for b in fun expression to get: *)
		if 1 = 0 then 6 else add_b (dec 1) (inc 6)
		(* evaluate predicate expression *)
			evaluate 1 -> 1
			evaluate 0 -> 0
			evaluate = -> 0
			apply = to 1, 0 -> false
		(* since expression is false, replace with false clause *)
	add_b (dec 1) (inc 6)
		evaluate dec 1
			evaluate 1 -> 1
			evaluate dec -> dec
			apply dec to 1 -> 0
		evaluate inc 6
			evaluate 6 -> 6
			evaluate inc -> inc
			apply inc to 6 -> 7
		evaluate add_b -> 
			fun a b -> if a = 0 then b else add_b (dec a) (inc b)
	apply fun a b (...) to 0, 7
		(* substitute 0 for a, 7 for b in fun expression
		to get: *)
		if 0 = 0 then 7 else add_b (dec 0) (inc 7)
		(* evaluate predicate expression *)
			evaluate 0 -> 0
			evaluate 0 -> 0
			evaluate = -> =
			apply = to 0, 0 -> true
		(* since expression is true, replace with true clause *)
		result: 7
		*)
		
(* The first function is recursive as it calls itself repeatedly
until a base case is reached and then it begins returning to 
earlier calls. The second function is iterative as it updates 
its arguments with each call and doesn't have to return and backtrack
up a recursive tree. *)

(* C.1 *)

(* This function computes the factorial of the input number, which for
a number n is equal to n * (n - 1) * ... * 1. *)

let rec factorial n =
	if n = 0 then 1 else n * factorial (n - 1)
	
(* C.1.a *)

(* This function takes a non-negative integer argument and computes
that term of the infinite series expansion of e. *)
	
let e_term i = 
	(float_of_int (1)) /. (float_of_int (factorial (i - 1)))
	
(* C.1.b *)

(* This is a recursive funciton that takes one positive integer 
argument and computes an approximation to e by summing up that many 
terms of the infinite series expansion of e. *)
	
let rec e_approximation num =
	if num = 1 then float_of_int(num) 
	else (e_term num +. e_approximation (num - 1))
			
(* C.1.c *)
				
(* The approximation of 20 terms gave the answer:
2.71828182845904553 and exp 1.0 gave:2.71828182845904509  *)

(* C.1.d *)

(* When I attempt to get a better approximation with 100 terms, the
result returned is infinity. This is because the factorial function
is trying to compute 100 factorial, and there is integer overflow. 
Factorial 100 is returned as 0 so e_term at 100 is 1/0 and equates
to infinity. *)

(* C.2 *)

(* These two functions take in non-negative integers and return whether
or not they are even or odd respectively. *)

let rec is_even x = 
	if x = 0 || is_odd (x - 1) = true then true else false
and is_odd y = if y = 0 || is_even (y - 1) = false then false else true

(* C.3 *)

(* Recursive and iterative solutions to problem C.3 *)

let rec f_rec n = 
	if n < 3 then n 
		else (f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec (n - 3))
	
		
let f_iter n = 
	let rec helper current max f1 f2 f3 = 
	if max < 3 then max
	else if current > max then f1 
	else helper (current + 1) max (f1 + 2 * f2 + 3 * f3) f1 f2
	in helper 3 n 2 1 0
	
(* C.4 *)

(* Function to find pascal coefficient in given row and row index. *)

let rec pascal_coefficient x y = 
	match x, y with 
	| _, n when n > x || n < 1 -> failwith "invalid arguments"
	| j, _ when j < 1 -> failwith "invalid arguments"
	| _, 1 -> 1
	| _, w when w = x -> 1
	| _, _ -> (pascal_coefficient (x - 1) (y - 1) 
	+ pascal_coefficient (x - 1) (y))
	
		
		


