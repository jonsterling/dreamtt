open RuleKit

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


