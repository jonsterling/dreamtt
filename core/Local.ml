open Basis
open Syntax


module type Ops =
sig
  type 'a m
  type env

  include Reader.Ops with type 'a m := 'a m and type local := env
end

module type T =
sig
  type sort
  type elt
  type env = elt Env.t

  include Monad.Trans
  include Ops with type 'a m := 'a m and type env := env

  (** {1 Control operators} *)

  (** Extend the ambient context with a variable of a given sort; the variable is
      passed to the continuation. *)
  val scope : sort -> (elt -> 'a m) -> 'a m


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


module Make (E : Elt) =
struct
  include E

  type env = elt Env.t

  module M = Reader.MakeT (struct type local = env end) (Error.M)
  include M

  let throw e = lift @@ Error.M.throw e

  let scope (sort : sort) (k : elt -> 'a m) : 'a m =
    reader @@ fun env ->
    let x = var sort @@ Env.fresh env in
    run (Env.append env x) @@ k x
end


module TmElt =
struct
  type sort = gtp
  type elt = gtm
  let var gtp lvl =
    GEta (GVar (lvl, gtp))
end


module M = Make (TmElt)

