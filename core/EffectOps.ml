open Basis
open Syntax

module type G =
sig
  type 'a m
  include Error.Ops with type 'a m := 'a m
  val theory : Logic.thy m
  val scope_thy : Logic.update -> 'a m -> 'a m
end

module type L =
sig
  include G

  val env : gtm Env.t m
  val bind_tm : gtp -> (gtm -> 'a m) -> 'a m
  val append_tm : gtm -> 'a m -> 'a m
end
