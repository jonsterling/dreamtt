module type S =
sig
  type 'a m
  val ret : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module type ReaderT =
sig
  type local
  include S 

  type 'a n

  val read : local m
  val locally : (local -> local) -> 'a m -> 'a m

  val reader : (local -> 'a n) -> 'a m
  val run : local -> 'a m -> 'a n
end


module type Reader = ReaderT with type 'a n = 'a

module ReaderT (L : sig type local end) (M : S) : ReaderT with type 'a n = 'a M.m and type local = L.local =
struct
  include L
  type 'a n = 'a M.m
  type 'a m = local -> 'a n

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

module Identity : S with type 'a m = 'a = 
struct
  type 'a m = 'a
  let ret a = a
  let bind x f = f x
end

module Reader (L : sig type local end) = ReaderT (L) (Identity)

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

module Notation (M : S) : Notation with type 'a m := 'a M.m =
struct
  let (let*) = M.bind

  let (and*) m n =
    let* x = m in
    let* y = n in
    M.ret (x, y)

  let (let+) m f = M.bind m (fun x -> M.ret (f x))

  let (and+) m n = (and*) m n

  let (<@>) f m = (let+) m f
  let (|>>) = (let*)
  let (@<<) f m = m |>> f
  let (<&>) = (and+)
end
  


