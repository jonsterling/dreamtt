include Equate

module Env = Env
module Equate = Equate
module Syntax = Syntax
module Logic = Logic
module Effect = Effect
include Syntax

module Proof =
struct
  type 'a t = 'a
  let out x = x
end

type tp = gtp Proof.t
type tm = gtm Proof.t

let tp_of_tm = tp_of_gtm

module Rule = Rule
module Refiner = Refiner
