(** This module contains the interface and functors for access and scoped updates of local state. *)

(** The reader monad transformer interface. *)
module type T =
sig
  type local
  include Monad.Trans

  val read : local m
  val locally : (local -> local) -> 'a m -> 'a m

  val reader : (local -> 'a n) -> 'a m
  val run : local -> 'a m -> 'a n
end

(** The reader monad, i.e. the instance of the transformer at the identity monad. *)
module type S = T with type 'a n = 'a

module MakeT (L : sig type local end) (M : Monad.S) : T with type 'a n = 'a M.m and type local = L.local 
module Make (L : sig type local end) : S with type local = L.local
