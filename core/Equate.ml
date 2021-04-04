open Basis
open Syntax
open Effect

exception UnequalTypes
exception UnequalTerms
exception Impossible
exception Todo

open Monad.Notation (L)

let guard m =
  let* thy = L.theory in
  match Logic.consistency thy with
  | `Inconsistent -> L.ret ()
  | `Consistent -> m

let gfam_to_gtele gbase lfam env =
  GTlCons (gbase, LTlCons (lfam, LTlNil), env)

let rec equate_gtp : gtp -> gtp -> unit L.m =
  fun gtp0 gtp1 ->
  guard @@
  let* gtp0 = L.global @@ Eval.whnf_tp gtp0 in
  let* gtp1 = L.global @@ Eval.whnf_tp gtp0 in
  match gtp0, gtp1 with
  | GBool, GBool -> L.ret ()
  | GPi (gbase0, lfam0, env0), GPi (gbase1, lfam1, env1) ->
    let gtl0 = gfam_to_gtele gbase0 lfam0 env0 in
    let gtl1 = gfam_to_gtele gbase1 lfam1 env1 in
    equate_gtele gtl0 gtl1
  | GRcdTp (lbls0, gtl0), GRcdTp (lbls1, gtl1) when lbls0 = lbls1 ->
    equate_gtele gtl0 gtl1
  | _ ->
    L.throw UnequalTypes

and equate_gtele : gtele -> gtele -> unit L.m =
  fun gtl0 gtl1 ->
    guard @@
    match gtl0, gtl1 with
    | GTlNil, GTlNil -> L.ret ()
    | GTlCons (gtp0, ltl0, env0), GTlCons (gtp1, ltl1, env1) ->
      let gfib env gtp ltl = L.global @@ G.local env @@ L.bind_tm gtp @@ fun _ -> Eval.eval_tele ltl in
      let* gfib0 = gfib env0 gtp0 ltl0 in
      let* gfib1 = gfib env1 gtp1 ltl1 in
      equate_gtele gfib0 gfib1
    | _ ->
      L.throw UnequalTypes

and equate_gtm : gtp -> gtm -> gtm -> unit L.m =
  fun gtp gtm0 gtm1 ->
  L.global @@ Eval.whnf_tp gtp |>>
  function
  | GPi (gbase, lfam, env) ->
    equate_fun gbase lfam env gtm0 gtm1
  | GRcdTp (lbls, gtl) ->
    equate_rcd lbls gtl gtm0 gtm1
  | GBool ->
    equate_base gtm0 gtm1
  | GAbortTp ->
    L.ret ()

and equate_base gtm0 gtm1 =
  let* gtm0 = L.global @@ Eval.whnf gtm0 in
  let* gtm1 = L.global @@ Eval.whnf gtm1 in
  match gtm0, gtm1 with
  | GTt, GTt | GFf, GFf ->
    L.ret ()
  | Glued _, Glued _ ->
    raise Todo
  | _ ->
    raise UnequalTerms

and equate_fun gbase lfam env gf0 gf1 =
  L.bind_tm gbase @@ fun var ->
  let* gv0 = L.global @@ Eval.gapp gf0 var in
  let* gv1 = L.global @@ Eval.gapp gf1 var in
  let* gfib = L.global @@ G.local env @@ L.append_tm var @@ Eval.eval_tp lfam in
  equate_gtm gfib gv0 gv1

and equate_rcd lbls gtl gr0 gr1 =
  let rec loop lbls gtl =
    match lbls, gtl with
    | [], GTlNil ->
      L.ret ()
    | lbl::lbls, GTlCons (gbase, ltl, env) ->
      let* gv0 = L.global @@ Eval.gproj lbl gr0 in
      let* gv1 = L.global @@ Eval.gproj lbl gr1 in
      let* () = equate_gtm gbase gv0 gv1 in
      let* gfib = L.global @@ G.local env @@ L.append_tm gv0 @@ Eval.eval_tele ltl in
      loop lbls gfib
    | _ ->
      L.throw Impossible
  in
  loop lbls gtl
