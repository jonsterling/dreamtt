open Basis
open Core

(* {1 The source language} *)

type code = R of rcode | L of lcode
and rcode = Tt | Ff | Lam of string * code | Pair of code * code
and lcode = Var of string | App of code * code | Fst of code | Snd of code | Core of tm

(* {1 Elaborator} *)

module StrMap = Map.Make (String)
type resolver = tm StrMap.t

exception ElabError

module R = Refiner

let rec chk_code res : code -> R.chk =
  function
  | R rcode ->
    chk_rcode res rcode
  | L lcode ->
    chk_lcode res lcode

and syn_code res : code -> R.syn = 
  function
  | L lcode ->
    syn_lcode res lcode
  | R _ -> 
    raise ElabError

and chk_rcode res : rcode -> R.chk  = 
  function
  | Tt -> R.tt
  | Ff -> R.ff
  | Lam (x, codex) -> 
    R.lam @@ fun var ->
    let resx = StrMap.add x var res in
    chk_code resx codex
  | Pair (code0, code1) ->
    let chk0 = chk_code res code0 in
    let chk1 = chk_code res code1 in
    R.pair chk0 chk1

and chk_lcode res (lcode : lcode) : R.chk = 
  R.with_tp @@ fun gtp ->
  match tp_head gtp with
  | `Pi ->
    R.lam @@ fun var ->
    chk_lcode res @@ App (L lcode, L (Core var))
  | `Sg ->
    let chk0 = chk_lcode res @@ Fst (L lcode) in
    let chk1 = chk_lcode res @@ Snd (L lcode) in
    R.pair chk0 chk1
  | `Bool ->
    R.conv @@ syn_lcode res lcode

and syn_lcode res : lcode -> R.syn = 
  function
  | Var x ->
    R.core @@ StrMap.find x res 
  | App (fn, arg) -> 
    R.app (syn_code res fn) (chk_code res arg)
  | Fst code ->
    R.fst @@ syn_code res code
  | Snd code ->
    R.snd @@ syn_code res code
  | Core tm ->
    R.core tm 



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
      let+ env = get_env in
      let x = Env.proj env ix in
      L (Var x)

    | LTt -> ret @@ R Tt

    | LFf -> ret @@ R Ff

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
