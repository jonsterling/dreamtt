type 'a printer = Format.formatter -> 'a -> unit

module Env :
sig
  type t

  exception EmptyEnv
  exception UnboundVariable of {ix : int; env: t}

  val emp : t

  (** May raise {!UnboundVariable}. *)
  val var : int -> t -> string
  val bind : t -> string option -> string * t
  val bindn : t -> string option list -> string list * t

  (** May raise {!EmptyEnv}. *)
  val proj : t -> t
  val names : t -> string list
end

type env = Env.t
