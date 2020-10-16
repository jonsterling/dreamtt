module type T =
sig
  type local
  include Monad.S

  type 'a n

  val read : local m
  val locally : (local -> local) -> 'a m -> 'a m

  val reader : (local -> 'a n) -> 'a m
  val run : local -> 'a m -> 'a n
end

module type S = T with type 'a n = 'a

module MakeT (L : sig type local end) (M : Monad.S) : T with type 'a n = 'a M.m and type local = L.local 
module Make (L : sig type local end) : S with type local = L.local
