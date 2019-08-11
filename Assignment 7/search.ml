(* search.ml: search strategies *)
(* Student name:  Alec Andrews              *)
(* CMS cluster login name:  avandrew    *)

(* CSMan didn't accept my initial submission *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let rec iter queue visited =
    if S.is_empty queue then raise Not_found
    else let hist = S.pop queue in 
    if DS.mem (List.hd hist) visited then iter queue visited
      else if D.is_solved (List.hd hist) then hist 
      else 
        let visited = DS.add (List.hd hist) visited in 
        let children = D.next (List.hd hist) in
        begin
          List.iter (fun x -> S.push (x :: hist) queue) children;
          iter queue visited;
        end

    let search init = 
    let queue = S.create () in
    let first_hist = [init] in
    let visited = DS.empty in 
    begin
    S.push first_hist queue;
    iter queue visited;
    end

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

