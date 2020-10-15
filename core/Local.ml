open Syntax

type tp = gtp 
type tm = gtm 
let var gtp lvl =
  GEta (GVar (lvl, gtp))

type env = tm Env.t
type 'a m = env -> 'a

let run env m = 
  m env

let ret a = 
  fun _ -> a

let bind m f = 
  fun env ->
  f (m env) env

let scope (tp : tp) (k : tm -> 'a m) : 'a m = 
  fun env ->
  let x = var tp @@ Env.fresh env in 
  k x @@ Env.append env x

let get_env env = 
  env
