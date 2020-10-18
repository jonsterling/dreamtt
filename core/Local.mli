(** A convenient monad for working with a local environment. *)

open Basis

module type Ops =
sig
  type 'a m
  type sort
  type elt

  include Reader.Ops with type 'a m := 'a m and type local := elt Env.t


  (** {1 Control operators} *)

  (** Extend the ambient context with a variable of a given sort; the variable is
      passed to the continuation. *)
  val scope : sort -> (elt -> 'a m) -> 'a m
end

module type T =
sig
  include Monad.Trans
  include Ops with type 'a m := 'a m

  type env = elt Env.t

  (** {1 Runners} *)

  val run : elt Env.t -> 'a m -> 'a n
end

module type Elt =
sig
  type sort
  type elt

  val var : sort -> Env.lvl -> elt
end


module type S =
sig
  include T with type 'a n = 'a Error.M.m
  val throw : exn -> 'a m
end

module Make (E : Elt) : S with type sort = E.sort and type elt = E.elt

module M : S with type sort = Syntax.gtp and type elt = Syntax.gtm
