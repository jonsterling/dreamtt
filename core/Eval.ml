open Basis
open Syntax
open Effect

exception Impossible

module LStringMapUtil = Monad.MapUtil (L) (StringMap)

let guard_gtm m =
  let open Monad.Notation (G) in
  let* thy = G.theory in
  match Logic.consistency thy with
  | `Consistent -> m
  | `Inconsistent ->
    G.ret GAbort


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
  guard_gtm @@
  match gtm0 with
  | GLam (_, (ltm, tm_env)) ->
    G.local (Env.append tm_env gtm1) @@ eval ltm
  | GEta gneu ->
    G.ret @@ GEta (GSnoc (gneu, GApp gtm1))
  | _ ->
    G.throw Impossible

and gproj lbl gtm =
  guard_gtm @@
  match gtm with
  | GRcd (_, _, gmap) ->
    begin
      match StringMap.find_opt lbl gmap with
      | Some gtm -> G.ret gtm
      | None -> G.throw Impossible
    end
  | GEta gneu ->
    G.ret @@ GEta (GSnoc (gneu, GProj lbl))
  | _ ->
    G.throw Impossible

let rec eval_tp : ltp -> gtp lm =
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



let rec tp_of_gtm =
  function
  | GTt | GFf -> G.ret GBool
  | GLam (gfam, _) ->
    G.ret @@ GPi gfam
  | GRcd (lbls, gtele, _) ->
    G.ret @@ GRcdTp (lbls, gtele)
  | GEta gneu ->
    tp_of_gneu gneu
  | GAbort ->
    G.ret GAbortTp

and tp_of_gneu =
  let open Monad.Notation (G) in
  function
  | GVar (_, gtp) ->
    G.ret gtp
  | GSnoc (gneu, gfrm) ->
    let* tp = tp_of_gneu gneu in
    match tp, gfrm with
    | GPi (_, lfam, env), GApp gtm ->
      G.local (Env.append env gtm) @@ eval_tp lfam
    | GRcdTp (lbls, gtl), GProj lbl ->
      tp_of_rcd_field lbls gtl lbl gneu
    | _ ->
      G.throw Impossible


and tp_of_rcd_field lbls gtl lbl gneu =
  let open Monad.Notation (G) in
  match lbls, gtl with
  | [], GTlNil ->
    G.throw Impossible
  | lbl' :: _, GTlCons (gtp, _, _) when lbl = lbl' ->
    G.ret gtp
  | lbl' :: lbls, GTlCons (_, ltl, env) ->
    let gtm = GEta (GSnoc (gneu, GProj lbl')) in
    let* gtl' = G.local (Env.append env gtm) @@ eval_tele ltl in
    tp_of_rcd_field lbls gtl' lbl gneu
  | _ ->
    G.throw Impossible
