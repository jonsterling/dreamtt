open Basis
open Syntax

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

  let get_env = read

  let run_exn env m = 
    Error.M.run (run env m) @@ function
    | Result.Ok a -> a
    | Result.Error e -> raise e
end


module TmElt =
struct
  type sort = gtp
  type elt = gtm
  let var gtp lvl = 
    GEta (GVar (lvl, gtp))
end


module M = Make (TmElt)

