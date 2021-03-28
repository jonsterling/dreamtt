open Basis
open Syntax

exception Impossible

module M = Error.M
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
  | LPair (gfam, ltm0, ltm1) ->
    let* gtm0 = eval env ltm0 in
    let* gtm1 = eval env ltm1 in
    ret @@ GPair (gfam, gtm0, gtm1)
  | LFst ltm ->
    let* gtm = eval env ltm in
    gfst gtm
  | LSnd ltm ->
    let* gtm = eval env ltm in
    gsnd gtm


and gapp gtm0 gtm1 : gtm m =
  match gtm0 with
  | GLam (_, (ltm, tm_env)) ->
    let tm_env = Env.append tm_env gtm1 in
    eval tm_env ltm
  | GEta gneu ->
    ret @@ GEta (GSnoc (gneu, GApp gtm1))
  | _ ->
    throw Impossible

and gfst gtm =
  match gtm with
  | GPair(_, gtm0, _) ->
    ret gtm0
  | GEta gneu ->
    ret @@ GEta (GSnoc (gneu, GFst))
  | _ ->
    throw Impossible

and gsnd gtm =
  match gtm with
  | GPair(_, _, gtm1) ->
    ret gtm1
  | GEta gneu ->
    ret @@ GEta (GSnoc (gneu, GSnd))
  | _ ->
    throw Impossible


let rec eval_tp env : ltp -> gtp m =
  function
  | LPi (lbase, lfam) ->
    let+ gbase = eval_tp env lbase in
    GPi (gbase, lfam, env)
  | LSg (lbase, lfam) ->
    let+ gbase = eval_tp env lbase in
    GSg (gbase, lfam, env)
  | LBool ->
    ret GBool


let eval_tele env : ltele -> gtele m =
  function
  | LTlNil -> ret GTlNil
  | LTlCons (lbl, ltp, ltele) ->
    let+ gtp = eval_tp env ltp in
    GTlCons (lbl, gtp, ltele, env)
