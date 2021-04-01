open Basis
open Code

module R = Core.Refiner

type resolver = Core.tm StringMap.t

module M = Reader.Make (struct type local = resolver end)
module StringMapUtil = Monad.MapUtil (M) (StringMap)
include M
open Monad.Notation (M)

exception ElabError

let commute rule f : _ m =
  M.reader @@ fun res ->
  rule @@ fun x ->
  M.run res @@ f x

let add_var x var =
  locally @@ StringMap.add x var

let rec elab_chk_code : code -> R.chk_rule m =
  function
  | R rcode ->
    elab_chk_rcode rcode
  | L lcode ->
    elab_chk_lcode lcode

and elab_syn_code : code -> R.syn_rule m =
  function
  | L lcode ->
    elab_syn_lcode lcode
  | R _ ->
    ret @@ R.fail_syn ElabError

and elab_chk_rcode : rcode -> R.chk_rule m =
  function
  | Tt -> ret R.tt
  | Ff -> ret R.ff
  | Lam (x, codex) ->
    commute R.lam @@ fun var ->
    add_var x var @@
    elab_chk_code codex
  | Pair (code0, code1) ->
    let+ chk0 = elab_chk_code code0
    and+ chk1 = elab_chk_code code1 in
    R.pair chk0 chk1
  | Rcd code_map ->
    let* chk_map = StringMapUtil.flat_map elab_chk_code code_map in
    ret @@ R.rcd chk_map
  | _ ->
    ret @@ R.fail_chk ElabError

and elab_chk_lcode (lcode : lcode) : R.chk_rule m =
  commute R.with_tp @@ fun gtp ->
  match Core.tp_head gtp with
  | `Pi ->
    commute R.lam @@ fun var ->
    elab_chk_lcode @@ App (L lcode, L (Core var))
  | `Bool ->
    let+ syn = elab_syn_lcode lcode in
    R.conv syn
  | `Rcd lbls ->
    let rec loop chk_map lbls =
      match lbls with
      | [] -> ret chk_map
      | lbl :: lbls ->
        let* chk = elab_chk_lcode @@ Proj (lbl, L (lcode)) in
        loop (StringMap.add lbl chk chk_map) lbls
    in
    let+ chk_map = loop StringMap.empty lbls in
    R.rcd chk_map
  | `Abort ->
    ret @@ R.chk_abort

and elab_syn_lcode : lcode -> R.syn_rule m =
  function
  | Var x ->
    let+ res = read in
    R.core @@ StringMap.find x res
  | App (fn, arg) ->
    let+ syn = elab_syn_code fn
    and+ chk = elab_chk_code arg in
    R.app syn chk
  | Fst code ->
    let+ syn = elab_syn_code code in
    R.fst syn
  | Snd code ->
    let+ syn = elab_syn_code code in
    R.snd syn
  | Proj (lbl, code) ->
    let+ syn = elab_syn_code code in
    R.proj lbl syn
  | Core tm ->
    ret @@ R.core tm

and elab_tp_code : code -> R.tp_rule m =
  function
  | R rcode ->
    elab_tp_rcode rcode
  | _ ->
    ret @@ R.fail_tp ElabError

and elab_tp_rcode : rcode -> R.tp_rule m =
  function
  | Bool ->
    ret R.bool
  | Pi (x, code0, code1) ->
    let* tp_base = elab_tp_code code0 in
    commute (R.pi tp_base) @@ fun var ->
    add_var x var @@
    elab_tp_code code1
  | Sg (x, code0, code1) ->
    let* tp_base = elab_tp_code code0 in
    commute (R.sg tp_base) @@ fun var ->
    add_var x var @@
    elab_tp_code code1
  | RcdTp tele_code ->
    let+ tele = elab_tele_code tele_code in
    R.rcd_tp tele
  | _ ->
    ret @@ R.fail_tp ElabError

and elab_tele_code : tele_code -> R.tele_rule m =
  function
  | TlNil ->
    ret R.tl_nil
  | TlCons (lbl, code0, code1) ->
    let* tp_base = elab_tp_code code0 in
    commute (R.tl_cons lbl tp_base) @@ fun var ->
    add_var lbl var @@
    elab_tele_code code1
