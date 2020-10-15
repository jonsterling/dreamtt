open Basis

type tp = Syntax.gtp
type tm = Syntax.gtm

include Monad.S
val run : tm Env.t -> 'a m -> 'a
val scope : tp -> (tm -> 'a m) -> 'a m
val get_env : tm Env.t m
