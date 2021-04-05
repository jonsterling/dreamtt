open Basis
open Syntax
open Rule
open Effect
open Monad.Notation (L)

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

