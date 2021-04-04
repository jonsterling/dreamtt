open Basis
open Syntax

(** This module contains two effect monads that are used across [dreamtt]; one
    for "global" computations that do not depend on the local variable
    environment, and one for "local" computations that do depend on the local
    variable environment.
*)

(** Monad for computations that don't depend on the local environment. *)
type 'a gm

(** Monad for computations that do depend on the local environment. *)
type 'a lm

module G :
sig
  include Monad.S with type 'a m = 'a gm
  include EffectOps.G with type 'a m := 'a m

  (** Execute with a local variable environment. *)
  val local : [`Tm of gtm | `Tp of gtp] Env.t -> 'a lm -> 'a m
end

module L :
sig
  include Monad.S with type 'a m = 'a lm
  include EffectOps.L with type 'a m := 'a m

  (** Forget the local variable environment. *)
  val global : 'a gm -> 'a m
end
