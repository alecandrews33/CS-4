(*
  	-- Desugar expression to:

  	let factorial = 
  	fun n -> 
  		let rec iter m r = 
  			if m = 0
  				then r
  			else iter (m - 1) (r * m)
  		in iter n 1
  	in 
  		factorial 3

	-- Start with initial environment:

    FRAME 0 (initial environment)
      parent: none
      bindings:
        - : [primitive function -]
        * : [primitive function *]

  	FUNCTION 0 (fun n -> let rec iter m r = (...))
  		parent: FRAME 0
  		param: n
  		body: let rec iter m r = ...

	FRAME 1 (let factorial = FUNCTION 0 in ...)
		parent: FRAME 0
		bindings: 
			factorial : FUNCTION 0 
	
	FRAME 2 (FUNCTION 0 applied to 3)
		parent: FRAME 0 
		bindings: 
			n : 3

	FUNCTION 1 (fun m r -> if m = 0 then ...)
		parent: FRAME 2
		param: m r
		body: if m = 0 then ...

	FRAME 3 (let rec iter = FUNCTION 1 in ...)
		parent: FRAME 2
		bindings:
			iter : FUNCTION 1

	FRAME 4 (FUNCTION 1 applied to 3 1)
		parent: FRAME 3
		bindings: 
			m : 3 
			r : 1

  FRAME 5 (FUNCTION 1 applied to 2 3)
    parent: FRAME 3
    bindings:
      m: 2
      r: 3

  FRAME 6 (FUNCTION 1 applied to 1 6)
    parent: FRAME 3
    bindings:
      m: 1
      r: 6

  FRAME 7 (FUNCTION 1 applied to 0 6)
    parent: FRAME 3
    bindings:
      m: 0 
      r: 6
  		
*)

(* A.2 *)

(*let factorial = 
  let f = ref (fun n -> 0) in*)


(* B.1 *)
exception Stat_error of string

let make_stat_1 () = 
  let sum = ref 0.0 and
  sumsq = ref 0.0 and
  n = ref 0 in 
  object
    method append datum = 
      sum := !sum +. datum;
      sumsq := !sumsq +. (datum *. datum);
      n := !n + 1
    method mean = match !n with
      | 0 -> raise (Stat_error "need at least one value for mean")
      | _ -> !sum /. float_of_int(!n)
    method variance = match !n with
      | 0 -> raise (Stat_error "need at least one value for variance")
      | _ -> 
        ((!sumsq -. (!sum *. !sum) /. float_of_int(!n))) /. float_of_int(!n)
    method stdev = match !n with
      | 0 -> raise (Stat_error "need at least one value for stdev")
      | _ -> 
        sqrt(((!sumsq -. (!sum *. !sum) /. float_of_int(!n)))
          /. float_of_int(!n))
    method clear = 
      sum := 0.0;
      sumsq := 0.0;
      n := 0
    end

let make_stat_2 () = 
  let sum = ref 0.0 and
  sumsq = ref 0.0 and
  n = ref 0 in 
  object (self)
    method append datum = 
      sum := !sum +. datum;
      sumsq := !sumsq +. (datum *. datum);
      n := !n + 1
    method private _variance = 
      ((!sumsq -. (!sum *. !sum) /. float_of_int(!n))) /. float_of_int(!n)
    method mean = match !n with
      | 0 -> raise (Stat_error "need at least one value for mean")
      | _ -> !sum /. float_of_int(!n)
    method variance = match !n with
      | 0 -> raise (Stat_error "need at least one value for variance")
      | _ -> self#_variance
    method stdev = match !n with
      | 0 -> raise (Stat_error "need at least one value for stdev")
      | _ -> sqrt(self#_variance)
    method clear = 
      sum := 0.0;
      sumsq := 0.0;
      n := 0
    end

module type PRIORITY_QUEUE =
  sig
    exception Empty
    type elem
    type t
    val empty : t
    val is_empty : t -> bool
    val insert : t -> elem -> t
    val find_min : t -> elem
    val delete_min : t -> t
    val from_list : elem list -> t
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) = 
  struct

  exception Empty

  type elem = int
  type t = Leaf | Node of elem * int * t * t

  let empty = Leaf
  let is_empty heap = if (heap = empty) then true else false

  let rank heap = match heap with 
    | Leaf -> 0
    | Node (_, b, _, _) -> b

  let rec merge first second = 
    let new_heap heap1 heap2 min = 
      if (rank heap1) < (rank heap2) 
      then Node (min, (rank heap1) + 1, heap2, heap1)
      else Node (min, (rank heap2) + 1, heap1, heap2) 
    in
    match (first, second) with
    | (Leaf, t) -> t
    | (t, Leaf) -> t
    | (Node (a1, b1, l1, r1), Node (a2, b2, l2, r2)) -> 
      begin
        if a1 < a2 then new_heap l1 (merge r1 (Node (a2, b2, l2, r2))) a1
        else new_heap (merge r2 (Node (a1, b1, l1, r1))) l2 a2
      end

  let insert heap el = merge heap (Node (el, 1, Leaf, Leaf))

  let find_min heap = match heap with
    | Leaf -> raise Empty
    | Node (a, b, l, r) -> a

  let delete_min heap = match heap with
    | Leaf -> raise Empty
    | Node (a, b, l, r) -> merge l r

  let rec from_list lst = match lst with
    | [] -> Leaf
    | h :: t -> insert (from_list t) h

  end

  let heap_sort lst = 
    let rec iter input output =
    if PriorityQueue.is_empty input then List.rev output
    else  iter (PriorityQueue.delete_min input) 
      ((PriorityQueue.find_min input) :: output)
  in iter (PriorityQueue.from_list lst) []

(* C.2 *)

type comparison = LT | EQ | GT

module type ORDERED = 
  sig
    type t
    val cmp: t -> t -> comparison
  end

module OrderedString = 
  struct
    type t = string
    let cmp x y = 
      if x = y then EQ else if x < y then LT else GT
  end
      
module MakePriorityQueue (Elt : ORDERED)
  : (PRIORITY_QUEUE with type elem = Elt.t) = 
  struct
  exception Empty

  type elem = Elt.t
  type t = Leaf | Node of elem * int * t * t

  let empty = Leaf
  let is_empty heap = (heap = empty)

  let rank heap = match heap with 
    | Leaf -> 0
    | Node (_, b, _, _) -> b

  let rec merge first second = 
    let new_heap heap1 heap2 min = 
      if (rank heap1) < (rank heap2) 
      then Node (min, (rank heap1) + 1, heap2, heap1)
      else Node (min, (rank heap2) + 1, heap1, heap2) 
    in
    match (first, second) with
    | (Leaf, t) -> t
    | (t, Leaf) -> t
    | (Node (a1, b1, l1, r1), Node (a2, b2, l2, r2)) -> 
      begin
        if a1 < a2 then new_heap l1 (merge r1 (Node (a2, b2, l2, r2))) a1
        else new_heap (merge r2 (Node (a1, b1, l1, r1))) l2 a2
      end

  let insert heap el = merge heap (Node (el, 1, Leaf, Leaf))

  let find_min heap = match heap with
    | Leaf -> raise Empty
    | Node (a, b, l, r) -> a

  let delete_min heap = match heap with
    | Leaf -> raise Empty
    | Node (a, b, l, r) -> merge l r

  let rec from_list lst = match lst with
    | [] -> Leaf
    | h :: t -> insert (from_list t) h
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst = 
  let rec iter input output =
    if StringPQ.is_empty input then List.rev output
    else  iter (StringPQ.delete_min input) 
      ((StringPQ.find_min input) :: output)
  in iter (StringPQ.from_list lst) []

(* D.1 *)

type 'a evaluators = Unevaluated of (unit -> 'a) | Evaluated of 'a

type 'a lazy_t = 'a evaluators ref 

let make_lazy e = ref (Unevaluated e)

let force lz = match !lz with
  | Unevaluated f -> let value = f () in 
    begin
      lz := Evaluated (value);
      value
    end
  | Evaluated eval -> eval

(* D.2.1 *)

 let y = 
    fun f -> 
      (fun z -> z (`Roll z)) 
      (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

let rec sum1 lst = match lst with
  | [] -> 0
  | h :: t -> h + sum1 t

let almost_sum = fun f -> 
  fun lst -> match lst with
    | [] -> 0
    | h :: t -> h + f t

let sum = y almost_sum


(* D.2.2 *)

  let factorial2 n =
    let iter = fun f -> fun x -> match x with
    | (0, r) -> r
    | (n, r) -> f (n - 1, n * r)
    in y iter (n, 1)