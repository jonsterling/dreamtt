open Basis
open Syntax
open Rule
open Effect
open Monad.Notation (L)

let pi (base : tp_rule) (fam : gtm -> tp_rule) : tp_rule =
  TpRule.rule @@
  let* lbase = TpRule.run base in
  let* gbase = Eval.eval_tp lbase in
  L.bind_tm gbase @@ fun var ->
  let+ lfam = TpRule.run @@ fam var in
  LPi (lbase, lfam)

let lam (bdy : gtm -> chk_rule) : chk_rule =
  ChkRule.rule @@
  function
  | GPi ((gbase, lfam, env) as gfam) ->
    L.bind_tm gbase @@ fun var ->
    let+ lbdy =
      ChkRule.run (bdy var) @<<
      L.global @@ G.local env @@ L.append_tm var @@
      Eval.eval_tp lfam
    in
    LLam (gfam, lbdy)
  | _ ->
    L.throw TypeError

let app (fn : syn_rule) (arg : chk_rule) : syn_rule =
  SynRule.rule @@
  let* gtm0 = SynRule.run fn in
  match tp_of_gtm gtm0 with
  | GPi (gbase, _, _) ->
    let* larg = ChkRule.run arg gbase in
    let* gtm1 = Eval.eval larg in
    L.global @@ Eval.gapp gtm0 gtm1
  | _ ->
    L.throw TypeError

