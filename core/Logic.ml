module Var =
struct
  type t = [`L of int]
  let compare = compare
end

module VarSet = Set.Make (Var)

type prop = Var.t Prop.t

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
    | Prop.Var x ->
      Consistent {true_vars = VarSet.add x true_vars}
    | Prop.Prop Top ->
      thy
    | Prop.Prop Bot ->
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
    | Prop.Var x -> VarSet.mem x true_vars
    | Prop.Prop Top -> true
    | Prop.Prop Bot -> false

let test thy cx phi =
  let thy' = List.fold_left ext thy cx in
  test_closed thy' phi
