open Basis
open Syntax

include Monad.S

val run_exn : 'a m -> 'a

val eval : env -> ltm -> gtm m
val eval_tp : env -> ltp -> gtp m
val eval_tele : env -> ltele -> gtele m

val gapp : gtm -> gtm -> gtm m
val gfst : gtm -> gtm m
val gsnd : gtm -> gtm m
