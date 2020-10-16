(** A convenient monad for working with a local environment. *)

open Basis

module type S = 
sig
  type sort
  type elt

  include Monad.S

  (** {1 Operations} *)

  val get_env : elt Env.t m
  val throw : exn -> 'a m

  (** {1 Control operators} *)

  (** Extend the ambient context with a variable of a given sort; the variable is
      passed to the continuation. *)
  val scope : sort -> (elt -> 'a m) -> 'a m


  (** {1 Runners} *)

  val run : elt Env.t -> 'a m -> 'a Error.M.m
  val run_exn : elt Env.t -> 'a m -> 'a
end

module type Elt = 
sig
  type sort
  type elt

  val var : sort -> Env.lvl -> elt
end

module Make (E : Elt) : S with type sort = E.sort and type elt = E.elt
module M : S with type sort = Syntax.gtp and type elt = Syntax.gtm
