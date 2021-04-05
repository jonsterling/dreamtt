open Basis
open Syntax
open Effect

type tp_rule = ltp L.m
type syn_rule = gtm L.m

type chk_rule =
  | Chk of (gtp -> ltm L.m)
  | BChk of (gtp -> ltm part -> ltm L.m)


type tele_rule = (string list * ltele) L.m

module TpRule =
struct
  type t = tp_rule
  let rule x = x
  let run x = x
end

module ChkRule =
struct
  type t = chk_rule
  let rule x = Chk x
  let brule x = BChk x

  let run =
    function
    | Chk x -> x
    | BChk bchk ->
      fun tp ->
        bchk tp @@
        Prt {supp = PBot; part = LAbort; env = Env.empty}

  let brun =
    let open Monad.Notation (L) in
    function
    | Chk chk ->
      fun tp (Prt part) ->
        let* ltm = chk tp in
        let* () =
          L.scope_thy (`Ext part.supp) @@
          let* gtm = Eval.eval ltm in
          let* gtm' = Eval.eval part.part in
          Equate.equate_gtm tp gtm gtm'
        in
        L.ret ltm
    | BChk bchk ->
      bchk
end

module SynRule =
struct
  type t = syn_rule
  let rule x = x
  let run x = x
end

module TeleRule =
struct
  type t = tele_rule
  let rule x = x
  let run x = x
end
