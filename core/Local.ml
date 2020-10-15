open Syntax

type tp = gtp 
type tm = gtm 
let var gtp lvl =
  GEta (GVar (lvl, gtp))

type env = tm Env.t

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

let scope (tp : tp) (k : tm -> 'a m) : 'a m = 
  fun env ->
  let x = var tp @@ Env.fresh env in 
  k x @@ Env.append env x

let get_env env = 
  Ok env
