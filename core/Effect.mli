open Basis
open Syntax

type 'a gm
type 'a lm

module G :
sig
  include Monad.S with type 'a m = 'a gm
  include Error.Ops with type 'a m := 'a m
  val theory : Logic.thy m
  val scope_thy : Logic.update -> 'a m -> 'a m
  val local : gtm Env.t -> 'a lm -> 'a m
end

module L :
sig
  include Monad.S with type 'a m = 'a lm
  include Error.Ops with type 'a m := 'a m
  val theory : Logic.thy m
  val scope_thy : Logic.update -> 'a m -> 'a m

  val global : 'a gm -> 'a m
  val env : gtm Env.t m
  val bind_tm : gtp -> (gtm -> 'a m) -> 'a m
end
