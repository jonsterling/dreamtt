open Basis
open Syntax
open Effect

exception Impossible

module LStringMapUtil = Monad.MapUtil (L) (StringMap)

module rec Eval : sig
  val eval : ltm -> gtm lm
  val eval_tp : ltp -> gtp lm
  val eval_tele : ltele -> gtele lm
end =
struct
  open Compute
  open Monad.Notation (L)

  let rec eval : ltm -> gtm lm =
    fun ltm ->
      match ltm with
      | LLam (gfam, ltm) ->
        let+ env = L.env in
        GLam (gfam, (ltm, env))
      | LVar ix ->
        let* env = L.env in
        begin
          match Env.proj env ix with
          | `Tm x -> L.ret x
          | _ -> L.throw Impossible
        end
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

  and eval_tp : ltp -> gtp lm =
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
    | LTpVar ix ->
      let* env = L.env in
      begin
        match Env.proj env ix with
        | `Tp x -> L.ret x
        | _ -> L.throw Impossible
      end

  and eval_tele : ltele -> gtele lm =
    function
    | LTlNil -> L.ret GTlNil
    | LTlCons (ltp, ltele) ->
      let* gtp = eval_tp ltp in
      let+ env = L.env in
      GTlCons (gtp, ltele, env)
end
and Compute : sig
  val whnf : gtm -> gtm gm
  val whnf_tp : gtp -> gtp gm

  val gapp : gtm -> gtm -> gtm gm
  val gproj : string -> gtm -> gtm gm
end =
struct
  open Eval
  open Monad.Notation (G)

  let guard ~abort m =
    let* thy = G.theory in
    match Logic.consistency thy with
    | `Consistent -> m
    | `Inconsistent -> G.ret abort


  let rec whnf : gtm -> gtm gm =
    fun gtm ->
      proj_part gtm @@ gtm_bdry gtm |>>
      function
      | `Done -> G.ret gtm
      | `Step gtm -> whnf gtm

  and whnf_tp : gtp -> gtp gm =
    fun gtp ->
      proj_tp_part gtp (gtp_bdry gtp) |>>
      function
      | `Done -> G.ret gtp
      | `Step gtp -> whnf_tp gtp

  and tp_of_rcd_field lbls gtl lbl gtm =
    guard ~abort:GAbortTp @@
    match lbls, gtl with
    | [], GTlNil ->
      G.throw Impossible
    | lbl' :: _, GTlCons (gtp, _, _) when lbl = lbl' ->
      G.ret gtp
    | lbl' :: lbls, GTlCons (_, ltl, env) ->
      let* gtm = gproj lbl' gtm in
      let* gtl' = G.local env @@ L.append_tm gtm @@ eval_tele ltl in
      tp_of_rcd_field lbls gtl' lbl gtm
    | _ ->
      G.throw Impossible

  and proj_part : gtm -> ltm part -> [`Done | `Step of gtm] gm =
    fun gtm (Prt part) ->
      let* thy = G.theory in
      if Logic.test thy [] part.supp then
        let+ gtm = G.local part.env @@ eval part.part in
        `Step gtm
      else
        G.ret `Done

  and proj_tp_part : gtp -> ltp part -> [`Done | `Step of gtp] gm =
    fun gtp (Prt part) ->
      let* thy = G.theory in
      if Logic.test thy [] part.supp then
        let+ gtp = G.local part.env @@ eval_tp part.part in
        `Step gtp
      else
        G.ret `Done


  and gapp gtm0 gtm1 =
    guard ~abort:GAbort @@
    match gtm0 with
    | GLam (_, (ltm, tm_env)) ->
      G.local (Env.append tm_env @@ `Tm gtm1) @@ eval ltm
    | Glued glued ->
      let+ glued' = gapp_glued glued gtm1 in
      Glued glued'
    | _ ->
      G.throw Impossible

  and gproj lbl gtm =
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

  and gapp_glued (Gl glued) arg =
    whnf_tp glued.gtp |>> function
    | GPi (gtp, lfam, env) ->
      let supp = glued.supp in
      let base = GSnoc (glued.base, GApp arg) in
      let part, env =
        let env = glued.env in
        let lvl = Env.fresh env in
        let env = Env.append env @@ `Tm arg in
        LApp (glued.part, LVar (Env.lvl_to_ix env lvl)), env
      in
      let+ gfib = G.local env @@ L.append_tm arg @@ eval_tp lfam in
      Gl {gtp = gfib; base; supp; part; env}
    | _ ->
      G.throw Impossible

  and gproj_glued lbl (Gl glued) =
    whnf_tp glued.gtp |>>
    function
    | GRcdTp (lbls, gtl) ->
      let+ gtp = tp_of_rcd_field lbls gtl lbl (Glued (Gl glued)) in
      let supp = glued.supp in
      let base = GSnoc (glued.base, GProj lbl) in
      let part, env = LProj (lbl, glued.part), glued.env in
      Gl {gtp; base; supp; part; env}
    | _ ->
      G.throw Impossible
end

include Eval
include Compute
