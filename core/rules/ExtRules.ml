open Basis
open Syntax
open Rule
open Effect
open Monad.Notation (L)

let ext_in (chk_rule : chk_rule) : chk_rule =
  ChkRule.rule @@
  function
  | GExtTp (gtp, part) ->
    let* ltm = ChkRule.brun chk_rule gtp part in
    L.ret @@ LExtIn (gtp, part, ltm)
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


