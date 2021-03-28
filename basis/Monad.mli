module type S =
sig
  type 'a m
  val ret : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module type Trans =
sig
  type 'a n
  include S
  val lift : 'a n -> 'a m
end

module Identity : S with type 'a m = 'a

module type Notation =
sig
  type 'a m
  val (let*) : 'a m -> ('a -> 'b m) -> 'b m
  val (and*) : 'a m -> 'b m -> ('a * 'b) m
  val (let+) : 'a m -> ('a -> 'b) -> 'b m
  val (and+) : 'a m -> 'b m -> ('a * 'b) m
  val (<@>) : ('a -> 'b) -> 'a m -> 'b m
  val (|>>) : 'a m -> ('a -> 'b m) -> 'b m
  val (@<<) : ('a -> 'b m) -> 'a m -> 'b m
  val (<&>) : 'a m -> 'b m -> ('a * 'b) m
end

module Notation (M : S) : Notation with type 'a m := 'a M.m

module MapUtil (M : S) (N : Map.S) :
sig
  val flat_map : ('a -> 'b M.m) -> 'a N.t -> 'b N.t M.m
end
