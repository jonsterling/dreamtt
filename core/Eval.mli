open Syntax
open Effect.G

val eval : env -> ltm -> gtm m
val eval_tp : env -> ltp -> gtp m
val eval_tele : env -> ltele -> gtele m

val gapp : gtm -> gtm -> gtm m
val gproj : string -> gtm -> gtm m

val tp_of_gtm : gtm -> gtp m
val tp_of_gneu : gneu -> gtp m

