open Syntax
open Effect

val eval : ltm -> gtm lm
val eval_tp : ltp -> gtp lm
val eval_tele : ltele -> gtele lm

val gapp : gtm -> gtm -> gtm gm
val gproj : string -> gtm -> gtm gm

val tp_of_gtm : gtm -> gtp gm
val tp_of_gneu : gneu -> gtp gm

