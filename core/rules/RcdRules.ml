open Basis
open Syntax
open Rule
open Effect
open Monad.Notation (L)

let rcd_tp (tele : tele_rule) : tp_rule =
  TpRule.rule @@
  let+ lbls, ltl = TeleRule.run tele in
  LRcdTp (lbls, ltl)

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

let proj lbl (syn_rule : syn_rule) : syn_rule =
  SynRule.rule @@
  let* gtm = SynRule.run syn_rule in
  match tp_of_gtm gtm with
  | GRcdTp (lbls, _) when List.mem lbl lbls ->
    L.global @@ Eval.gproj lbl gtm
  | _ ->
    L.throw TypeError

