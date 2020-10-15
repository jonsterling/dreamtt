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
