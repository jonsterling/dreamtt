open Basis
open Syntax
open Effect
open Rule

exception TypeError

open Monad.Notation (L)

let with_tp kont =
  ChkRule.rule @@ fun tp ->
  ChkRule.run (kont tp) tp

let inst_tp_fam : ltp -> env -> gtm -> gtp G.m =
  fun lfam env gtm ->
  let envx = Env.append env @@ `Tm gtm in
  G.local envx @@ Eval.eval_tp lfam

let inst_tm_fam : ltm -> env -> gtm -> gtm G.m =
  fun lfam env gtm ->
  let envx = Env.append env @@ `Tm gtm in
  G.local envx @@ Eval.eval lfam


let core x =
  SynRule.rule @@ L.ret x

let bool : tp_rule =
  TpRule.rule @@
  L.ret LBool

let tt : chk_rule =
  ChkRule.rule @@
  function
  | GBool -> L.ret LTt
  | _ -> L.throw TypeError

let ff : chk_rule =
  ChkRule.rule @@
  function
  | GBool -> L.ret LFf
  | _ -> L.throw TypeError


let tl_nil : tele_rule =
  TeleRule.rule @@ L.ret ([], LTlNil)

let rec freshen lbl lbls =
  if List.mem lbl lbls then
    freshen (lbl ^ "'") lbls
  else
    lbl

let tl_cons lbl tp_rule tele_rule =
  TeleRule.rule @@
  let* lbase = TpRule.run tp_rule in
  let* gbase = Eval.eval_tp lbase in
  L.bind_tm gbase @@ fun var ->
  let+ lbls, lfam = TeleRule.run @@ tele_rule var in
  let lbl' = freshen lbl lbls in
  lbl' :: lbls, LTlCons (lbase, lfam)

let pi (base : tp_rule) (fam : gtm -> tp_rule) : tp_rule =
  TpRule.rule @@
  let* lbase = TpRule.run base in
  let* gbase = Eval.eval_tp lbase in
  L.bind_tm gbase @@ fun var ->
  let+ lfam = TpRule.run @@ fam var in
  LPi (lbase, lfam)


let rcd_tp (tele : tele_rule) : tp_rule =
  TpRule.rule @@
  let+ lbls, ltl = TeleRule.run tele in
  LRcdTp (lbls, ltl)


let lam (bdy : gtm -> chk_rule) : chk_rule =
  ChkRule.rule @@
  function
  | GPi ((gbase, lfam, env) as gfam) ->
    L.bind_tm gbase @@ fun var ->
    let+ lbdy = ChkRule.run (bdy var) @<< L.global @@ inst_tp_fam lfam env var in
    LLam (gfam, lbdy)
  | _ ->
    L.throw TypeError

let rcd (chk_map : chk_rule StringMap.t) : chk_rule =
  ChkRule.rule @@
  function
  | GRcdTp (lbls, gtl) ->
    let rec loop tmap lbls gtl =
      match lbls, gtl with
      | [], GTlNil -> L.ret tmap
      | lbl :: lbls, GTlCons (gtp, ltl, tlenv) ->
        begin
          match StringMap.find_opt lbl chk_map with
          | Some chk_rule ->
            let* ltm = ChkRule.run chk_rule gtp in
            let* gtm = Eval.eval ltm in
            let* gtl' = L.global @@ G.local tlenv @@ L.append_tm gtm @@ Eval.eval_tele ltl in
            let tmap' = StringMap.add lbl ltm tmap in
            loop tmap' lbls gtl'
          | None ->
            L.throw TypeError
        end
      | _ ->
        L.throw TypeError
    in
    let* tmap = loop StringMap.empty lbls gtl in
    L.ret @@ LRcd (lbls, gtl, tmap)
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

let proj lbl (syn_rule : syn_rule) : syn_rule =
  SynRule.rule @@
  let* gtm = SynRule.run syn_rule in
  match tp_of_gtm gtm with
  | GRcdTp (lbls, _) when List.mem lbl lbls ->
    L.global @@ Eval.gproj lbl gtm
  | _ ->
    L.throw TypeError

let ext_in (chk_rule : chk_rule) : chk_rule =
  ChkRule.rule @@
  function
  | GExtTp (gtp, Prt part) ->
    let* ltm = ChkRule.run chk_rule gtp in
    let* () =
      L.scope_thy (`Ext part.supp) @@
      let* gtm = Eval.eval ltm in
      let* gtm' = Eval.eval part.part in
      Equate.equate_gtm gtp gtm gtm'
    in
    L.ret @@ LExtIn (gtp, Prt part, ltm)
  | _ ->
    L.throw TypeError

let ext_out (syn_rule : syn_rule) : syn_rule =
  SynRule.rule @@
  let* gtm = SynRule.run syn_rule in
  match tp_of_gtm gtm with
  | GExtTp _ ->
    L.global @@ Eval.gext_out gtm
  | _ ->
    L.throw TypeError

let fst (syn_rule : syn_rule) : syn_rule =
  proj "fst" syn_rule

let snd (syn_rule : syn_rule) : syn_rule =
  proj "snd" syn_rule

let sg (base : tp_rule) (fam : gtm -> tp_rule) : tp_rule =
  rcd_tp @@
  tl_cons "fst" base @@ fun var ->
  tl_cons "snd" (fam var) @@ fun _ ->
  tl_nil

let pair (chk_rule0 : chk_rule) (chk_rule1 : chk_rule) : chk_rule =
  StringMap.empty
  |> StringMap.add "fst" chk_rule0
  |> StringMap.add "snd" chk_rule1
  |> rcd

let chk_abort : chk_rule =
  ChkRule.rule @@
  fun _ ->
  let* thy = L.theory in
  match Logic.consistency thy with
  | `Inconsistent -> L.ret LAbort
  | `Consistent -> L.throw TypeError


let rec conv_ : gtm -> chk_rule =
  function
  | GTt -> tt
  | GFf -> ff
  | GLam (_, ltm, env) ->
    lam @@ fun var ->
    ChkRule.rule @@ fun gfib ->
    let* gtm = L.global @@ inst_tm_fam ltm env var in
    ChkRule.run (conv_ gtm) gfib
  | GExtIn (_, _, gtm) ->
    ext_in (conv_ gtm)
  | GRcd (_, _, gmap) ->
    rcd @@ StringMap.map conv_ gmap
  | Glued glued ->
    conv_glued_ glued
  | GAbort ->
    chk_abort


and conv_glued_ : (gneu, ltm) glued -> chk_rule =
  fun (Gl glued) ->
  ChkRule.rule @@ fun gtp ->
  let* gtm = L.global @@ G.local glued.env @@ Eval.eval glued.part in
  let* () = Equate.equate_gtp gtp glued.gtp in
  let* thy = L.theory in
  if Logic.test thy [] glued.supp then
    ChkRule.run (conv_ gtm) gtp
  else
    conv_neu_ glued.base

and conv_neu_ : gneu -> ltm L.m =
  function
  | GVar lvl ->
    let+ env = L.env in
    let ix = Env.lvl_to_ix env lvl in
    LVar ix

  | GSnoc (gneu, gfrm) ->
    let* ltm = conv_neu_ gneu in
    match gfrm with
    | GProj lbl ->
      L.ret @@ LProj (lbl, ltm)
    | GApp arg ->
      let* ltm = conv_neu_ gneu in
      let tp_arg = tp_of_gtm arg in
      let+ larg = ChkRule.run (conv_ arg) tp_arg in
      LApp (ltm, larg)
    | GExtOut ->
      let+ ltm = conv_neu_ gneu in
      LExtOut ltm

let conv : syn_rule -> chk_rule =
  fun syn ->
  ChkRule.rule @@ fun gtp ->
  let* gtm = SynRule.run syn in
  ChkRule.run (conv_ gtm) gtp


let fail_tp exn = TpRule.rule @@ L.throw exn
let fail_chk exn = ChkRule.rule @@ fun _ -> L.throw exn
let fail_syn exn = SynRule.rule @@ L.throw exn


let elim_implicit_connectives : syn_rule -> syn_rule =
  fun syn ->
  SynRule.rule @@
  let* tm = SynRule.run syn in
  match tp_head @@ tp_of_gtm tm with
  | `Ext ->
    SynRule.run @@ ext_out @@ SynRule.rule @@ L.ret tm
  | _ ->
    L.ret tm

let intro_implicit_connectives : chk_rule -> chk_rule =
  fun chk ->
  with_tp @@ fun tp ->
  match tp_head tp with
  | `Ext -> ext_in chk
  |_ -> chk
