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

  val run : elt Env.t -> 'a m -> ('a, exn) Result.t
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

  type 'a m = env -> ('a, exn) Result.t

  let throw e _ = 
    Error e

  let run env m = 
    m env

  let run_exn  env m : 'a = 
    match m env with
    | Ok a -> a
    | Error e -> raise e

  let ret a = 
    fun _ -> Ok a

  let bind (m : 'a m) (f : 'a -> 'b m) : 'b m = 
    fun env ->
    Result.bind (m env) @@ fun x ->
    f x env

  let scope (sort : sort) (k : elt -> 'a m) : 'a m = 
    fun env ->
    let x = var sort @@ Env.fresh env in 
    k x @@ Env.append env x

  let get_env env = 
    Ok env
end


module TmElt =
struct
  type sort = gtp
  type elt = gtm
  let var gtp lvl = 
    GEta (GVar (lvl, gtp))
end


module M = Make (TmElt)

