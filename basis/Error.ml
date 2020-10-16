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
  val run_exn : 'a m -> 'a n
end

module type S = T with type 'a n = 'a

module MakeT (M : Monad.S) : T with type 'a n = 'a M.m =
struct
  type 'a n = 'a M.m
  type 'a m = ('a, exn) Result.t n

  let ret : 'a -> 'a m =
    fun a ->
    M.ret @@ Ok a

  let bind (m : 'a m) (k : 'a -> 'b m) : 'b m =
    M.bind m @@ function
    | Ok a ->
      k a
    | Error e ->
      M.ret @@ Error e

  let catch (m : 'a m) (k : ('a, exn) Result.t -> 'b m) : 'b m =
    M.bind m k

  let run (m : 'a m) (k : ('a, exn) Result.t -> 'b n) : 'b n =
    M.bind m k

  let run_exn m =
    M.bind m @@ function
    | Ok a -> M.ret a
    | Error e -> raise e

  let throw e =
    M.ret @@ Error e

  let lift n =
    M.bind n @@ fun x ->
    M.ret @@ Ok x
end

module M = MakeT (Monad.Identity)
