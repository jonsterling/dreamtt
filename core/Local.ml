module type Tm =
sig
  type tp
  type tm
  val var : tp -> Env.lvl -> tm
end

module Make (Tm : Tm) : sig
  type 'a m

  val scope : Tm.tp -> (Tm.tm -> 'a m) -> 'a m
  val ret : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val map : ('a -> 'b) -> 'a m -> 'b m

  val run : Tm.tm Env.t -> 'a m -> 'a

  val get_env : Tm.tm Env.t m
end = 
struct
  type env = Tm.tm Env.t
  type 'a m = env -> 'a

  let run env m = 
    m env

  let ret a = 
    fun _ -> a

  let bind m f = 
    fun env ->
    f (m env) env

  let map f m =
    bind m @@ fun x -> ret (f x)

  let scope (tp : Tm.tp) (k : Tm.tm -> 'a m) : 'a m = 
    fun env ->
    let x = Tm.var tp @@ Env.fresh env in 
    k x @@ Env.append env x

  let get_env env = 
    env
end


