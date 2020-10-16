module type Ops =
sig
  type 'a m
  val throw : exn -> 'a m
  val catch : 'a m -> (('a, exn) Result.t -> 'b m) -> 'b m
end

module type T =
sig
  include Monad.Trans
  include Ops with type 'a m := 'a m

  val run : 'a m -> (('a, exn) Result.t -> 'b n) -> 'b n
end

module type S = T with type 'a n = 'a

module MakeT (M : Monad.S) : T with type 'a n = 'a M.m
module M : T with type 'a n = 'a
