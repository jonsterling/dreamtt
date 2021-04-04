open Basis
open Core
open Code

module L = struct type local = string Env.t end
module M = Reader.Make (L)

open Monad.Notation (M)
module StringMapUtil = Monad.MapUtil (M) (StringMap)

include M

let scope k =
  reader @@ fun env ->
  let x = "x" ^ string_of_int @@ Env.int_of_lvl @@ Env.fresh env in
  run (Env.append env x) @@ k x


let rec distill_ltm : Syntax.ltm -> code m =
  function
  | LVar ix ->
    let+ env = read in
    let x = Env.proj env ix in
    L (Var x)

  | LTt ->
    ret @@ R Tt

  | LFf ->
    ret @@ R Ff

  | LLam (_, tm) ->
    scope @@ fun x ->
    let+ code = distill_ltm tm in
    R (Lam (x, code))

  | LApp (tm0, tm1) ->
    let+ code0 = distill_ltm tm0
    and+ code1 = distill_ltm tm1 in
    L (App (code0, code1))

  | LRcd (_, _, lmap) ->
    let+ code_map = StringMapUtil.flat_map distill_ltm lmap in
    R (Rcd code_map)

  | LProj (lbl, tm) ->
    let+ code = distill_ltm tm in
    L (Proj (lbl, code))

  | LAbort ->
    ret @@ R Abort

  | LExtOut ltm ->
    distill_ltm ltm
