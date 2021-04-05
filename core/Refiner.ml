open Basis
open Syntax
open Effect
open Rule

open Monad.Notation (L)

let core x =
  SynRule.rule @@ L.ret x

include BoolRules
include PiRules
include RcdRules
include TlRules
include SgRules
include ExtRules

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
    let* gtm = L.global @@ G.local env @@ L.append_tm var @@ Eval.eval ltm in
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

let with_tp kont =
  ChkRule.rule @@ fun tp ->
  ChkRule.run (kont tp) tp

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
