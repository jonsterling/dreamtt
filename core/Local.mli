open Basis

module type Tm =
sig
  type tp
  type tm
  val var : tp -> Env.lvl -> tm
end

module type S = 
sig
  type tp 
  type tm

  include Monad.S
  val run : tm Env.t -> 'a m -> 'a
  val scope : tp -> (tm -> 'a m) -> 'a m
  val get_env : tm Env.t m
end

module Make (Tm : Tm) : S with type tp = Tm.tp and type tm = Tm.tm

module M : S with type tp = Syntax.gtp and type tm = Syntax.gtm
