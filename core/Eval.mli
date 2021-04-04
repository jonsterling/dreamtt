open Syntax
open Effect

val eval : ltm -> gtm lm
val eval_tp : ltp -> gtp lm
val eval_tele : ltele -> gtele lm

val gapp : gtm -> gtm -> gtm gm
val gproj : string -> gtm -> gtm gm

val whnf : gtm -> gtm gm
val whnf_tp : gtp -> gtp gm
