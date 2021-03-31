open Basis
open Syntax

exception Impossible

module M = Error.M
module StringMapUtil = Monad.MapUtil (M) (StringMap)
open Monad.Notation (M)
include M


let run_exn = M.run_exn


let rec eval env : ltm -> gtm m =
  function
  | LLam (gfam, ltm) ->
    ret @@ GLam (gfam, (ltm, env))
  | LVar ix ->
    ret @@ Env.proj env ix
  | LTt -> ret GTt
  | LFf -> ret GFf
  | LApp (ltm0, ltm1) ->
    let* gtm0 = eval env ltm0 in
    let* gtm1 = eval env ltm1 in
    gapp gtm0 gtm1
  | LProj (lbl, ltm) ->
    let* gtm = eval env ltm in
    gproj lbl gtm
  | LRcd (lbls, gtele, lmap) ->
    let+ gmap = StringMapUtil.flat_map (eval env) lmap in
    GRcd (lbls, gtele, gmap)


and gapp gtm0 gtm1 : gtm m =
  match gtm0 with
  | GLam (_, (ltm, tm_env)) ->
    let tm_env = Env.append tm_env gtm1 in
    eval tm_env ltm
  | GEta gneu ->
    ret @@ GEta (GSnoc (gneu, GApp gtm1))
  | _ ->
    throw Impossible

and gproj lbl gtm =
  match gtm with
  | GRcd (_, _, gmap) ->
    begin
      match StringMap.find_opt lbl gmap with
      | Some gtm -> M.ret gtm
      | None -> throw Impossible
    end
  | GEta gneu ->
    ret @@ GEta (GSnoc (gneu, GProj lbl))
  | _ ->
    throw Impossible


let rec eval_tp env : ltp -> gtp m =
  function
  | LPi (lbase, lfam) ->
    let+ gbase = eval_tp env lbase in
    GPi (gbase, lfam, env)
  | LBool ->
    ret GBool
  | LRcdTp (lbls, ltl) ->
    let+ gtl = eval_tele env ltl in
    GRcdTp (lbls, gtl)

and eval_tele env : ltele -> gtele m =
  function
  | LTlNil -> ret GTlNil
  | LTlCons (ltp, ltele) ->
    let+ gtp = eval_tp env ltp in
    GTlCons (gtp, ltele, env)
