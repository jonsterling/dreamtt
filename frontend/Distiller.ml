open Basis
open Core
open Code

module NameSupply : Local.Elt with type sort = unit and type elt = string =
struct
  type sort = unit
  type elt = string

  let var () lvl =
    "x" ^ string_of_int (Env.int_of_lvl lvl)
end

module M = Local.Make (NameSupply)
open Monad.Notation (M)
module StringMapUtil = Monad.MapUtil (M) (StringMap)

include M

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
    M.scope () @@ fun x ->
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
