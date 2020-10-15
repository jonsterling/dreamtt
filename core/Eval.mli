open Syntax

type 'a m
val run : 'a m -> 'a

val eval : env -> ltm -> gtm m
val eval_tp : env -> ltp -> gtp m

val gapp : gtm -> gtm -> gtm m
val gfst : gtm -> gtm m
val gsnd : gtm -> gtm m
