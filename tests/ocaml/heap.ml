(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* $Id$ *)

(*s Heaps *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

module type S =sig

  (* Type of functional heaps *)
  type t

  (* Type of elements *)
  type elt

  (* The empty heap *)
  val empty : t

  (* [add x h] returns a new heap containing the elements of [h], plus [x];
     complexity $O(log(n))$ *)
  val add : elt -> t -> t

  (* [maximum h] returns the maximum element of [h]; raises [EmptyHeap]
     when [h] is empty; complexity $O(1)$ *)
  val maximum : t -> elt

  (* [remove h] returns a new heap containing the elements of [h], except
     the maximum of [h]; raises [EmptyHeap] when [h] is empty;
     complexity $O(log(n))$ *)
  val remove : t -> t

  (* usual iterators and combinators; elements are presented in
     arbitrary order *)
  val iter : (elt -> unit) -> t -> unit

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

end

exception EmptyHeap

(*s Functional implementation *)

module Functional(X : Ordered) = struct

  (* Heaps are encoded as complete binary trees, i.e., binary trees
     which are full expect, may be, on the bottom level where it is filled
     from the left.
     These trees also enjoy the heap property, namely the value of any node
     is greater or equal than those of its left and right subtrees.

     There are 4 kinds of complete binary trees, denoted by 4 constructors:
     [FFF] for a full binary tree (and thus 2 full subtrees);
     [PPF] for a partial tree with a partial left subtree and a full
     right subtree;
     [PFF] for a partial tree with a full left subtree and a full right subtree
     (but of different heights);
     and [PFP] for a partial tree with a full left subtree and a partial
     right subtree. *)

  type t =
    | Empty
    | FFF of t * X.t * t (* full    (full,    full) *)
    | PPF of t * X.t * t (* partial (partial, full) *)
    | PFF of t * X.t * t (* partial (full,    full) *)
    | PFP of t * X.t * t (* partial (full,    partial) *)

  type elt = X.t

  let empty = Empty

  (* smart constructors for insertion *)
  let p_f l x r = match l with
    | Empty | FFF _ -> PFF (l, x, r)
    | _ -> PPF (l, x, r)

  let pf_ l x = function
    | Empty | FFF _ as r -> FFF (l, x, r)
    | r -> PFP (l, x, r)

  let rec add x = function
    | Empty ->
        FFF (Empty, x, Empty)
    (* insertion to the left *)
    | FFF (l, y, r) | PPF (l, y, r) ->
        if X.compare x y > 0 then p_f (add y l) x r else p_f (add x l) y r
    (* insertion to the right *)
    | PFF (l, y, r) | PFP (l, y, r) ->
        if X.compare x y > 0 then pf_ l x (add y r) else pf_ l y (add x r)

  let maximum = function
    | Empty -> raise EmptyHeap
    | FFF (_, x, _) | PPF (_, x, _) | PFF (_, x, _) | PFP (_, x, _) -> x

  (* smart constructors for removal; note that they are different
     from the ones for insertion! *)
  let p_f l x r = match l with
    | Empty | FFF _ -> FFF (l, x, r)
    | _ -> PPF (l, x, r)

  let pf_ l x = function
    | Empty | FFF _ as r -> PFF (l, x, r)
    | r -> PFP (l, x, r)

  let rec remove = function
    | Empty ->
        raise EmptyHeap
    | FFF (Empty, _, Empty) ->
        Empty
    | PFF (l, _, Empty) ->
        l
    (* remove on the left *)
    | PPF (l, x, r) | PFF (l, x, r) ->
        let xl = maximum l in
        let xr = maximum r in
        let l' = remove l in
        if X.compare xl xr >= 0 then
          p_f l' xl r
        else
          p_f l' xr (add xl (remove r))
    (* remove on the right *)
    | FFF (l, x, r) | PFP (l, x, r) ->
        let xl = maximum l in
        let xr = maximum r in
        let r' = remove r in
        if X.compare xl xr > 0 then
          pf_ (add xr (remove l)) xl r'
        else
          pf_ l xr r'

  let rec iter f = function
    | Empty ->
        ()
    | FFF (l, x, r) | PPF (l, x, r) | PFF (l, x, r) | PFP (l, x, r) ->
        iter f l; f x; iter f r

  let rec fold f h x0 = match h with
    | Empty ->
        x0
    | FFF (l, x, r) | PPF (l, x, r) | PFF (l, x, r) | PFP (l, x, r) ->
        fold f l (fold f r (f x x0))

end
