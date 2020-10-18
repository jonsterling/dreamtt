module Env = Env
module Theory = Theory
module Syntax = Syntax
module Local = Local

open Syntax

module Proof =
struct
  type 'a t = 'a
  let out x = x
end

type tp = gtp Proof.t
type tm = gtm Proof.t

module type MonadCore =
sig
  include Theory.MonadTheory
  val run_exn : Syntax.gtm Env.t -> 'a m -> 'a
end

module Make (M : MonadCore) =
struct
  include Syntax
  include Theory.Make (M)
  let tp_of_tm = tp_of_gtm
  module Refiner = Refiner.Make (M)
end
