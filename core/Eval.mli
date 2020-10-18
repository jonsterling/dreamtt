open Basis
open Syntax

module type MonadEval =
sig
  include Monad.S
  include Error.Ops with type 'a m := 'a m
end

module Make (M : MonadEval) :
sig
  open M
  val eval : env -> ltm -> gtm m
  val eval_tp : env -> ltp -> gtp m

  val gapp : gtm -> gtm -> gtm m
  val gfst : gtm -> gtm m
  val gsnd : gtm -> gtm m
end
