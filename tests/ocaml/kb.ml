(* ========================================================================= *)
(* Knuth-Bendix completion done by HOL inference. John Harrison 2005         *)
(*                                                                           *)
(* This was written by fairly mechanical modification of the code at         *)
(*                                                                           *)
(*   http://www.cl.cam.ac.uk/users/jrh/atp/order.ml                          *)
(*   http://www.cl.cam.ac.uk/users/jrh/atp/completion.ml                     *)
(*                                                                           *)
(* for HOL's slightly different term structure, with ad hoc term             *)
(* manipulations replaced by inference on equational theorems. We also have  *)
(* the optimization of throwing left-reducible rules back into the set of    *)
(* critical pairs. However, we don't prioritize smaller critical pairs or    *)
(* anything like that; this is still a very naive implementation.            *)
(*                                                                           *)
(* For something very similar done 15 years ago, see Konrad Slind's Master's *)
(* thesis: "An Implementation of Higher Order Logic", U Calgary 1991.        *)
(* ========================================================================= *)

let is_realvar w x = is_var x & not(mem x w);;

let rec real_strip w tm =
  if mem tm w then tm,[] else
  let l,r = dest_comb tm in
  let f,args = real_strip w l in f,args@[r];;

(* ------------------------------------------------------------------------- *)
(* Construct a weighting function.                                           *)
(* ------------------------------------------------------------------------- *)

let weight lis (f,n) (g,m) =
  let i = index f lis and j = index g lis in
  i > j or i = j & n > m;;

(* ------------------------------------------------------------------------- *)
(* Generic lexicographic ordering function.                                  *)
(* ------------------------------------------------------------------------- *)

let rec lexord ord l1 l2 =
  match (l1,l2) with
    (h1::t1,h2::t2) -> if ord h1 h2 then length t1 = length t2
                       else h1 = h2 & lexord ord t1 t2
  | _ -> false;;

(* ------------------------------------------------------------------------- *)
(* Lexicographic path ordering. Note that we also use the weights            *)
(* to define the set of constants, so they don't literally have to be        *)
(* constants in the HOL sense.                                               *)
(* ------------------------------------------------------------------------- *)

let rec lpo_gt w s t =
  if is_realvar w t then not(s = t) & mem t (frees s)
  else if is_realvar w s or is_abs s or is_abs t then false else
  let f,fargs = real_strip w s and g,gargs = real_strip w t in
  exists (fun si -> lpo_ge w si t) fargs or
        forall (lpo_gt w s) gargs &
        (f = g & lexord (lpo_gt w) fargs gargs or
         weight w (f,length fargs) (g,length gargs))
and lpo_ge w s t = (s = t) or lpo_gt w s t;;

(* ------------------------------------------------------------------------- *)
(* Unification. Again we have the weights "w" fixing the set of constants.   *)
(* ------------------------------------------------------------------------- *)

let rec istriv w env x t =
  if is_realvar w t then t = x or defined env t & istriv w env x (apply env t)
  else if is_const t then false else
  let f,args = strip_comb t in
  exists (istriv w env x) args & failwith "cyclic";;

let rec unify w env tp =
  match tp with
   ((Var(_,_) as x),t) | (t,(Var(_,_) as x)) when not(mem x w) ->
        if defined env x then unify w env (apply env x,t)
        else if istriv w env x t then env else (x|->t) env
  | (Comb(f,x),Comb(g,y)) -> unify w (unify w env (x,y)) (f,g)
  | (s,t) -> if s = t then env else failwith "unify: not unifiable";;

(* ------------------------------------------------------------------------- *)
(* Full unification, unravelling graph into HOL-style instantiation list.    *)
(* ------------------------------------------------------------------------- *)

let fullunify w (s,t) =
  let env = unify w undefined (s,t) in
  let th = map (fun (x,t) -> (t,x)) (graph env) in
  let rec subs t =
    let t' = vsubst th t in
    if t' = t then t else subs t' in
  map (fun (t,x) -> (subs t,x)) th;;

(* ------------------------------------------------------------------------- *)
(* Construct "overlaps": ways of rewriting subterms using unification.       *)
(* ------------------------------------------------------------------------- *)

let LIST_MK_COMB f ths = rev_itlist (fun s t -> MK_COMB(t,s)) ths (REFL f);;

let rec listcases fn rfn lis acc =
  match lis with
    [] -> acc
  | h::t -> fn h (fun i h' -> rfn i (h'::map REFL t)) @
            listcases fn (fun i t' -> rfn i (REFL h::t')) t acc;;

let rec overlaps w th tm rfn =
  let l,r = dest_eq(concl th) in
  if not (is_comb tm) then [] else
  let f,args = strip_comb tm in
  listcases (overlaps w th) (fun i a -> rfn i (LIST_MK_COMB f a)) args
            (try [rfn (fullunify w (l,tm)) th] with Failure _ -> []);;

(* ------------------------------------------------------------------------- *)
(* Rename variables canonically to avoid clashes or remove redundancy.       *)
(* ------------------------------------------------------------------------- *)

let fixvariables s th =
  let fvs = subtract (frees(concl th)) (freesl(hyp th)) in
  let gvs = map2 (fun v n -> mk_var(s^string_of_int n,type_of v))
                 fvs (1--(length fvs)) in
  INST (zip gvs fvs) th;;

let renamepair (th1,th2) = fixvariables "x" th1,fixvariables "y" th2;;

(* ------------------------------------------------------------------------- *)
(* Find all critical pairs.                                                  *)
(* ------------------------------------------------------------------------- *)

let crit1 w eq1 eq2 =
  let l1,r1 = dest_eq(concl eq1)
  and l2,r2 = dest_eq(concl eq2) in
  overlaps w eq1 l2 (fun i th -> TRANS (SYM(INST i th)) (INST i eq2));;

let thm_union l1 l2 =
  itlist (fun th ths -> let th' = fixvariables "x" th in
                        let tm = concl th' in
                        if exists (fun th'' -> concl th'' = tm) ths then ths
                        else th'::ths)
         l1 l2;;

let critical_pairs w tha thb =
  let th1,th2 = renamepair (tha,thb) in
  if concl th1 = concl th2 then crit1 w th1 th2 else
  filter (fun th -> let l,r = dest_eq(concl th) in l <> r)
         (thm_union (crit1 w th1 th2) (thm_union (crit1 w th2 th1) []));;

(* ------------------------------------------------------------------------- *)
(* Normalize an equation and try to orient it.                               *)
(* ------------------------------------------------------------------------- *)

let normalize_and_orient w eqs th =
  let th' = GEN_REWRITE_RULE TOP_DEPTH_CONV eqs th in
  let s',t' = dest_eq(concl th') in
  if lpo_ge w s' t' then th' else if lpo_ge w t' s' then SYM th'
  else failwith "Can't orient equation";;

(* ------------------------------------------------------------------------- *)
(* Print out status report to reduce user boredom.                           *)
(* ------------------------------------------------------------------------- *)

let status(eqs,crs) eqs0 =
  if eqs = eqs0 & (length crs) mod 1000 <> 0 then () else
  (print_string(string_of_int(length eqs)^" equations and "^
                string_of_int(length crs)^" pending critical pairs");
   print_newline());;

(* ------------------------------------------------------------------------- *)
(* Basic completion, throwing back left-reducible rules.                     *)
(* ------------------------------------------------------------------------- *)

let left_reducible eqs eq =
  can (CHANGED_CONV(GEN_REWRITE_CONV (LAND_CONV o ONCE_DEPTH_CONV) eqs))
      (concl eq);;

let rec complete w (eqs,crits) =
  match crits with
    (eq::ocrits) ->
        let trip =
          try let eq' = normalize_and_orient w eqs eq in
              let s',t' = dest_eq(concl eq') in
              if s' = t' then (eqs,ocrits) else
              let crits',eqs' = partition(left_reducible [eq']) eqs in
              let eqs'' = eq'::eqs' in
              eqs'',
              ocrits @ crits' @ itlist ((@) o critical_pairs w eq') eqs'' []
          with Failure _ ->
              if exists (can (normalize_and_orient w eqs)) ocrits
              then (eqs,ocrits@[eq])
              else failwith "complete: no orientable equations" in
        status trip eqs; complete w trip
  | [] -> eqs;;

(* ------------------------------------------------------------------------- *)
(* Overall completion.                                                       *)
(* ------------------------------------------------------------------------- *)

let complete_equations wts eqs =
  let eqs' = map (normalize_and_orient wts []) eqs in
  complete wts ([],eqs');;

(* ------------------------------------------------------------------------- *)
(* Knuth-Bendix example 4: the inverse property.                             *)
(* ------------------------------------------------------------------------- *)

complete_equations [`1`; `(*):num->num->num`; `i:num->num`]
  [SPEC_ALL(ASSUME `!a b. i(a) * a * b = b`)];;

(* ------------------------------------------------------------------------- *)
(* Knuth-Bendix example 6: central groupoids.                                *)
(* ------------------------------------------------------------------------- *)

complete_equations [`(*):num->num->num`]
 [SPEC_ALL(ASSUME `!a b c. (a * b) * (b * c) = b`)];;

(* ------------------------------------------------------------------------- *)
(* Knuth-Bendix example 9: cancellation law.                                 *)
(* ------------------------------------------------------------------------- *)

complete_equations
 [`1`; `( * ):num->num->num`; `(+):num->num->num`; `(-):num->num->num`]
 (map SPEC_ALL (CONJUNCTS (ASSUME
  `(!a b:num. a - a * b = b) /\
   (!a b:num. a * b - b = a) /\
   (!a. a * 1 = a) /\
   (!a. 1 * a = a)`)));;

(* ------------------------------------------------------------------------- *)
(* Another example: pure congruence closure (no variables).                  *)
(* ------------------------------------------------------------------------- *)

complete_equations [`c:A`; `f:A->A`]
 (map SPEC_ALL (CONJUNCTS (ASSUME
   `((f(f(f(f(f c))))) = c:A) /\ (f(f(f c)) = c)`)));;

(* ------------------------------------------------------------------------- *)
(* Knuth-Bendix example 1: group theory.                                     *)
(* ------------------------------------------------------------------------- *)

let eqs = map SPEC_ALL (CONJUNCTS (ASSUME
  `(!x. 1 * x = x) /\ (!x. i(x) * x = 1) /\
   (!x y z. (x * y) * z = x * y * z)`));;

complete_equations [`1`; `(*):num->num->num`; `i:num->num`] eqs;;

(* ------------------------------------------------------------------------- *)
(* Near-rings (from Aichinger's Diplomarbeit).                               *)
(* ------------------------------------------------------------------------- *)

let eqs = map SPEC_ALL (CONJUNCTS (ASSUME
  `(!x. 0 + x = x) /\
   (!x. neg x + x = 0) /\
   (!x y z. (x + y) + z = x + y + z) /\
   (!x y z. (x * y) * z = x * y * z) /\
   (!x y z. (x + y) * z = (x * z) + (y * z))`));;

let nreqs =
complete_equations
  [`0`; `(+):num->num->num`; `neg:num->num`; `( * ):num->num->num`] eqs;;

(*** This weighting also works OK, though the system is a bit bigger

let nreqs =
complete_equations
  [`0`; `(+):num->num->num`; `( * ):num->num->num`; `INV`] eqs;;

****)

(* ------------------------------------------------------------------------- *)
(* A "completion" tactic.                                                    *)
(* ------------------------------------------------------------------------- *)

let COMPLETE_TAC w th =
  let eqs = map SPEC_ALL (CONJUNCTS(SPEC_ALL th)) in
  let eqs' = complete_equations w eqs in
  MAP_EVERY (ASSUME_TAC o GEN_ALL) eqs';;

(* ------------------------------------------------------------------------- *)
(* Solve example problems in gr *)

g `(!x. 1 * x = x) /\
   (!x. i(x) * x = 1) /\
   (!x y z. (x * y) * z = x * y * z)
   ==> !x y. i(y) * i(i(i(x * i(y)))) * x = 1`;;

e (DISCH_THEN(COMPLETE_TAC [`1`; `(*):num->num->num`; `i:num->num`]));;
e (ASM_REWRITE_TAC[]);;

g `(!x. 0 + x = x) /\
   (!x. neg x + x = 0) /\
   (!x y z. (x + y) + z = x + y + z) /\
   (!x y z. (x * y) * z = x * y * z) /\
   (!x y z. (x + y) * z = (x * z) + (y * z))
   ==> (neg 0  * (x * y + z + neg(neg(w + z))) + neg(neg b + neg a) =
        a + b)`;;

e (DISCH_THEN(COMPLETE_TAC
     [`0`; `(+):num->num->num`; `neg:num->num`; `( * ):num->num->num`]));;
e (ASM_REWRITE_TAC[]);;
