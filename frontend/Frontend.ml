open Basis
open Core

(* {1 The source language} *)

type code = R of rcode | L of lcode
and rcode = Bool | Pi of string * code * code | Sg of string * code * code | Tt | Ff | Lam of string * code | Pair of code * code
and lcode = Var of string | App of code * code | Fst of code | Snd of code | Core of tm

(* {1 Elaborator} *)

exception ElabError

module R = Refiner

module Elaborator =
struct
  module StrMap = Map.Make (String)
  type resolver = tm StrMap.t

  module M = Reader.Make (struct type local = resolver end)
  include M

  open Monad.Notation (M)

  let commute rule f : _ m =
    M.reader @@ fun res ->
    rule @@ fun x ->
    M.run res @@ f x

  let add_var x var =
    locally @@ StrMap.add x var

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
      raise ElabError

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
    | _ ->
      raise ElabError

  and elab_chk_lcode (lcode : lcode) : R.chk_rule m =
    commute R.with_tp @@ fun gtp ->
    match tp_head gtp with
    | `Pi ->
      commute R.lam @@ fun var ->
      elab_chk_lcode @@ App (L lcode, L (Core var))
    | `Sg ->
      let+ chk0 = elab_chk_lcode @@ Fst (L lcode)
      and+ chk1 = elab_chk_lcode @@ Snd (L lcode) in
      R.pair chk0 chk1
    | `Bool ->
      let+ syn = elab_syn_lcode lcode in
      R.conv syn

  and elab_syn_lcode : lcode -> R.syn_rule m =
    function
    | Var x ->
      let+ res = read in
      R.core @@ StrMap.find x res
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
    | Core tm ->
      ret @@ R.core tm

  and elab_tp_code : code -> R.tp_rule m =
    function
    | R rcode ->
      elab_tp_rcode rcode
    | _ ->
      raise ElabError

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
    | _ ->
      raise ElabError
end

module S = Syntax

module NameSupply : Local.Elt with type sort = unit and type elt = string =
struct
  type sort = unit
  type elt = string

  let var () lvl =
    "x" ^ string_of_int (Env.int_of_lvl lvl)
end

module Distiller =
struct
  module M = Local.Make (NameSupply)
  open Monad.Notation (M)

  include M

  let rec distill_ltm : S.ltm -> code m =
    function
    | LVar ix ->
      let+ env = read in
      let x = Env.proj env ix in
      L (Var x)

    | LTt ->
      ret @@ R Tt

    | LFf ->
      ret @@ R Ff

    | LFst tm ->
      let+ code = distill_ltm tm in
      L (Fst code)

    | LSnd tm ->
      let+ code = distill_ltm tm in
      L (Snd code)

    | LLam (_, tm) ->
      M.scope () @@ fun x ->
      let+ code = distill_ltm tm in
      R (Lam (x, code))

    | LApp (tm0, tm1) ->
      let+ code0 = distill_ltm tm0
      and+ code1 = distill_ltm tm1 in
      L (App (code0, code1))

    | LPair (_, tm0, tm1) ->
      let+ code0 = distill_ltm tm0
      and+ code1 = distill_ltm tm1 in
      R (Pair (code0, code1))
end
