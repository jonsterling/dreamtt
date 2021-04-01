include Equate

module Env = Env
module Equate = Equate
module Syntax = Syntax
module Local = Local
include Syntax

module Proof =
struct
  type 'a t = 'a
  let out x = x
end

type tp = gtp Proof.t
type tm = gtm Proof.t

let tp_of_tm = Eval.tp_of_gtm

module Refiner = Refiner
