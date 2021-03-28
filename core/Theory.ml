open Basis
open Syntax

exception UnequalTypes
exception Impossible


let rec tp_of_gtm =
  function
  | GTt | GFf -> GBool
  | GLam (gfam, _) ->
    GPi gfam
  | GPair (gfam, _, _) ->
    GSg gfam
  | GRcd (lbls, gtele, _) ->
    GRcdTp (lbls, gtele)
  | GEta gneu ->
    tp_of_gneu gneu

and tp_of_gneu =
  function
  | GVar (_, gtp) -> gtp
  | GSnoc (gneu, gfrm) ->
    match tp_of_gneu gneu, gfrm with
    | GPi (_, lfam, env), GApp gtm ->
      Eval.run_exn @@ Eval.eval_tp (Env.append env gtm) lfam
    | GSg (gbase, _, _), GFst ->
      gbase
    | GSg (_, lfam, env), GSnd ->
      let gfst = GEta (GSnoc (gneu, GFst)) in
      Eval.run_exn @@ Eval.eval_tp (Env.append env gfst) lfam
    | _ ->
      raise Impossible


module M = RefineM

let rec equate_gtp : gtp -> gtp -> unit M.m =
  let open Monad.Notation (M) in
  fun gtp0 gtp1 ->
    match gtp0, gtp1 with
    | GBool, GBool -> M.ret ()
    | GPi (gbase0, lfam0, env0), GPi (gbase1, lfam1, env1)
    | GSg (gbase0, lfam0, env0), GSg (gbase1, lfam1, env1) ->
      let* () = equate_gtp gbase0 gbase1 in
      M.scope gbase0 @@ fun x ->
      let envx0 = Env.append env0 x in
      let envx1 = Env.append env1 x in
      let* gfib0 = M.lift_eval @@ Eval.eval_tp envx0 lfam0 in
      let* gfib1 = M.lift_eval @@ Eval.eval_tp envx1 lfam1 in
      equate_gtp gfib0 gfib1
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
