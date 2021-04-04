open Basis
open Syntax
open Effect

exception Impossible

module LStringMapUtil = Monad.MapUtil (L) (StringMap)

let guard ~abort m =
  let open Monad.Notation (G) in
  let* thy = G.theory in
  match Logic.consistency thy with
  | `Consistent -> m
  | `Inconsistent -> G.ret abort


let rec eval : ltm -> gtm lm =
  fun ltm ->
  let open Monad.Notation (L) in
  match ltm with
  | LLam (gfam, ltm) ->
    let+ env = L.env in
    GLam (gfam, (ltm, env))
  | LVar ix ->
    let+ env = L.env in
    Env.proj env ix
  | LTt -> L.ret GTt
  | LFf -> L.ret GFf
  | LApp (ltm0, ltm1) ->
    let* gtm0 = eval ltm0 in
    let* gtm1 = eval ltm1 in
    L.global @@ gapp gtm0 gtm1
  | LProj (lbl, ltm) ->
    let* gtm = eval ltm in
    L.global @@ gproj lbl gtm
  | LRcd (lbls, gtele, lmap) ->
    let+ gmap = LStringMapUtil.flat_map eval lmap in
    GRcd (lbls, gtele, gmap)
  | LAbort ->
    L.ret GAbort

and gapp gtm0 gtm1 =
  guard ~abort:GAbort @@
  let open Monad.Notation (G) in
  match gtm0 with
  | GLam (_, (ltm, tm_env)) ->
    G.local (Env.append tm_env gtm1) @@ eval ltm
  | Glued glued ->
    let+ glued' = gapp_glued glued gtm1 in
    Glued glued'
  | _ ->
    G.throw Impossible

and gproj lbl gtm =
  let open Monad.Notation (G) in
  guard ~abort:GAbort @@
  match gtm with
  | GRcd (_, _, gmap) ->
    begin
      match StringMap.find_opt lbl gmap with
      | Some gtm -> G.ret gtm
      | None -> G.throw Impossible
    end
  | Glued glued ->
    let+ glued' = gproj_glued lbl glued in
    Glued glued'
  | _ ->
    G.throw Impossible

and gapp_glued glued arg =
  let open Monad.Notation (G) in
  match glued.tp with
  | GPi (gtp, lfam, env) ->
    let supp = glued.supp in
    let base = GSnoc (glued.base, GApp arg) in
    let part, env =
      let env = glued.env in
      let lvl = Env.fresh env in
      let env = Env.append env arg in
      LApp (glued.part, LVar (Env.lvl_to_ix env lvl)), env
    in
    let+ tp = G.local env @@ L.append_tm arg @@ eval_tp lfam in
    {tp; base; supp; part; env}
  | _ ->
    G.throw Impossible

and gproj_glued lbl glued =
  let open Monad.Notation (G) in
  match glued.tp with
  | GRcdTp (lbls, gtl) ->
    let+ tp = tp_of_rcd_field lbls gtl lbl (Glued glued) in
    let supp = glued.supp in
    let base = GSnoc (glued.base, GProj lbl) in
    let part, env = LProj (lbl, glued.part), glued.env in
    {tp; base; supp; part; env}
  | _ ->
    G.throw Impossible


and eval_tp : ltp -> gtp lm =
  let open Monad.Notation (L) in
  function
  | LPi (lbase, lfam) ->
    let* gbase = eval_tp lbase in
    let+ env = L.env in
    GPi (gbase, lfam, env)
  | LBool ->
    L.ret GBool
  | LRcdTp (lbls, ltl) ->
    let+ gtl = eval_tele ltl in
    GRcdTp (lbls, gtl)
  | LAbortTp ->
    L.ret GAbortTp

and eval_tele : ltele -> gtele lm =
  let open Monad.Notation (L) in
  function
  | LTlNil -> L.ret GTlNil
  | LTlCons (ltp, ltele) ->
    let* gtp = eval_tp ltp in
    let+ env = L.env in
    GTlCons (gtp, ltele, env)



and tp_of_gtm gtm =
  guard ~abort:GAbortTp @@
  match gtm with
  | GTt | GFf -> G.ret GBool
  | GLam (gfam, _) ->
    G.ret @@ GPi gfam
  | GRcd (lbls, gtele, _) ->
    G.ret @@ GRcdTp (lbls, gtele)
  | Glued glued ->
    G.ret glued.tp
  | GAbort ->
    G.ret GAbortTp

and tp_of_rcd_field lbls gtl lbl gtm =
  guard ~abort:GAbortTp @@
  let open Monad.Notation (G) in
  match lbls, gtl with
  | [], GTlNil ->
    G.throw Impossible
  | lbl' :: _, GTlCons (gtp, _, _) when lbl = lbl' ->
    G.ret gtp
  | lbl' :: lbls, GTlCons (_, ltl, env) ->
    let* gtm = gproj lbl' gtm in
    let* gtl' = G.local (Env.append env gtm) @@ eval_tele ltl in
    tp_of_rcd_field lbls gtl' lbl gtm
  | _ ->
    G.throw Impossible
