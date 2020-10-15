open Basis

type tp = Syntax.gtp
type tm = Syntax.gtm

include Monad.S


(** {1 Operations} *)

val get_env : tm Env.t m
val throw : exn -> 'a m

(** {1 Control operators} *)

(** Extend the ambient context with a variable of a given type; the variable is
    passed to the continuation. *)
val scope : tp -> (tm -> 'a m) -> 'a m


(** {1 Runners} *)

val run : tm Env.t -> 'a m -> ('a, exn) Result.t
val run_exn : tm Env.t -> 'a m -> 'a
