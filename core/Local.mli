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
end
