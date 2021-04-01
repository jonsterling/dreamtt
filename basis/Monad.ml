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


module Identity : S with type 'a m = 'a =
struct
  type 'a m = 'a
  let ret a = a
  let bind x f = f x
end


module MapUtil (M : S) (N : Map.S) =
struct
  open Notation (M)
  let flat_map (f : 'a -> 'b M.m) (map : 'a N.t) : 'b N.t M.m =
    let rec loop out =
      function
      | [] -> M.ret out
      | (lbl, x) :: xs ->
        let* y = f x in
        loop (N.add lbl y out) xs
    in
    loop N.empty @@ N.bindings map
end
