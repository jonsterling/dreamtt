open Syntax
open Effect

type tp_rule
type chk_rule
type syn_rule
type tele_rule

module TpRule : sig
  type t = tp_rule
  val rule : ltp L.m -> t
  val run : t -> ltp L.m
end

module ChkRule : sig
  type t = chk_rule
  val rule : (gtp -> ltm L.m) -> t
  val brule : (gtp -> ltm part -> ltm L.m) -> t

  val run : t -> (gtp -> ltm L.m)
  val brun : t -> (gtp -> ltm part -> ltm L.m)
end

module SynRule : sig
  type t = syn_rule
  val rule : gtm L.m -> t
  val run : t -> gtm L.m
end

(** Invariant: does not return unless the list of labels has no shadowing *)
module TeleRule : sig
  type t = tele_rule
  val rule : (string list * ltele) L.m -> t
  val run : t -> (string list * ltele) L.m
end
