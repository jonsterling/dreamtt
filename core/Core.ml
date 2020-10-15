include Syntax

include Theory

type tp = gtp
type tm = gtm

let tp_of_tm = Theory.tp_of_gtm

module Refiner = Refiner
