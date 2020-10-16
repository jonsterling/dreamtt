(** This module contains the interface and functors for access and scoped updates of local state. *)

(** The operations of a reader monad *)
module type Ops =
sig
  type 'a m
  type local

  val read : local m
  val locally : (local -> local) -> 'a m -> 'a m
end

(** The reader monad transformer interface. *)
module type T =
sig
  include Monad.Trans
  include Ops with type 'a m := 'a m

  val read : local m
  val locally : (local -> local) -> 'a m -> 'a m

  val reader : (local -> 'a n) -> 'a m
  val run : local -> 'a m -> 'a n
end

(** The reader monad, i.e. the instance of the transformer at the identity monad. *)
module type S = T with type 'a n = 'a

module MakeT (L : sig type local end) (M : Monad.S) : T with type 'a n = 'a M.m and type local = L.local
module Make (L : sig type local end) : S with type local = L.local
