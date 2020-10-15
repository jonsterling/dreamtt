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


module Make (Tm : Tm) =
struct
  include Tm 

  type env = Tm.tm Env.t
  type 'a m = env -> 'a

  let run env m = 
    m env

  let ret a = 
    fun _ -> a

  let bind m f = 
    fun env ->
    f (m env) env

  let scope (tp : Tm.tp) (k : Tm.tm -> 'a m) : 'a m = 
    fun env ->
    let x = Tm.var tp @@ Env.fresh env in 
    k x @@ Env.append env x

  let get_env env = 
    env
end

module Tm = 
struct
  open Syntax
  type tp = gtp 
  type tm = gtm 
  let var gtp lvl =
    GEta (GVar (lvl, gtp))
end

module M = Make (Tm)
