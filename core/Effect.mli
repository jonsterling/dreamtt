open Basis
open Syntax

type 'a gm
type 'a lm

module G :
sig
  include Monad.S with type 'a m = 'a gm
  include EffectOps.G with type 'a m := 'a m
  val local : gtm Env.t -> 'a lm -> 'a m
end

module L :
sig
  include Monad.S with type 'a m = 'a lm
  include EffectOps.L with type 'a m := 'a m
  val global : 'a gm -> 'a m
end
