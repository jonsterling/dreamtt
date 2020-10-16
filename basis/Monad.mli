module type S =
sig
  type 'a m
  val ret : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module type Reader =
sig
  type local
  include S

  val read : local m
  val locally : (local -> local) -> 'a m -> 'a m

  val reader : (local -> 'a) -> 'a m
  val run : local -> 'a m -> 'a
end

module Reader (L : sig type local end) : Reader with type local = L.local

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
