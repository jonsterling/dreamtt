module Var =
struct
  type t = Env.lvl
  let compare = compare
end

module VarSet = Set.Make (Var)

type prop = Syntax.prop
open Syntax

type thy =
  | Consistent of {true_vars : VarSet.t}
  | Inconsistent

let emp =
  Consistent {true_vars = VarSet.empty}

let ext thy phi =
  match thy with
  | Inconsistent -> Inconsistent
  | Consistent {true_vars} ->
    match phi with
    | PropVar x ->
      Consistent {true_vars = VarSet.add x true_vars}
    | Top ->
      thy
    | Bot ->
      Inconsistent

let consistency =
  function
  | Consistent _ -> `Consistent
  | Inconsistent -> `Inconsistent

let test_closed thy phi =
  match thy with
  | Inconsistent -> true
  | Consistent {true_vars} ->
    match phi with
    | PropVar x -> VarSet.mem x true_vars
    | Top -> true
    | Bot -> false

let test thy cx phi =
  let thy' = List.fold_left ext thy cx in
  test_closed thy' phi

type update =
  [`Ext of prop]

let update (`Ext phi) thy =
  ext thy phi
