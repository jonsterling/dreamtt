open RuleKit
open TlRules
open RcdRules

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

