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

module type S = T with type 'a n = 'a

module MakeT (L : sig type local end) (M : Monad.S) : T with type 'a n = 'a M.m and type local = L.local =
struct
  include L
  type 'a n = 'a M.m
  type 'a m = local -> 'a n

  let lift m _ = m

  let ret a _ = M.ret a

  let bind (m : 'a m) (k : 'a -> 'b m) = 
    fun l ->
    M.bind (m l) @@ fun x ->
    k x l

  let locally f m l = m (f l)
  let reader f = f
  let run l m = m l
  let read l = M.ret l
end

module Make (L : sig type local end) = MakeT (L) (Monad.Identity)


