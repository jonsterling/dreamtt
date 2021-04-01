open Basis
open Syntax

exception UnequalTypes

module M = RefineM

let rec equate_gtp : gtp -> gtp -> unit M.m =
  let open Monad.Notation (M) in
  fun gtp0 gtp1 ->
    match gtp0, gtp1 with
    | GBool, GBool -> M.ret ()
    | GPi (gbase0, lfam0, env0), GPi (gbase1, lfam1, env1) ->
      let gtl0 = GTlCons (gbase0, LTlCons (lfam0, LTlNil), env0) in
      let gtl1 = GTlCons (gbase1, LTlCons (lfam1, LTlNil), env1) in
      equate_gtele gtl0 gtl1
    | GRcdTp (lbls0, gtl0), GRcdTp (lbls1, gtl1) when lbls0 = lbls1 ->
      equate_gtele gtl0 gtl1
    | _ ->
      M.throw UnequalTypes

and equate_gtele : gtele -> gtele -> unit M.m =
  let open Monad.Notation (M) in
  fun gtl0 gtl1 ->
    match gtl0, gtl1 with
    | GTlNil, GTlNil -> M.ret ()
    | GTlCons (gtp0, ltl0, env0), GTlCons (gtp1, ltl1, env1) ->
      let* () = equate_gtp gtp0 gtp1 in
      M.scope gtp0 @@ fun x ->
      let envx0 = Env.append env0 x in
      let envx1 = Env.append env1 x in
      let* gfib0 = M.lift_eval @@ Eval.eval_tele envx0 ltl0 in
      let* gfib1 = M.lift_eval @@ Eval.eval_tele envx1 ltl1 in
      equate_gtele gfib0 gfib1
    | _ ->
      M.throw UnequalTypes
