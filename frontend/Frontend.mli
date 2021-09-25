open Basis

(** {1 The source language}

    We begin by defining a naive source language.
*)

include module type of Code


(** {1 Elaboration} *)

module R := Core.Refiner

module Elaborator :
sig
  type resolver
  include Reader.S with type local = resolver

  (** The main entry-point: check a piece of code against a type. *)
  val elab_chk_code : code -> R.chk_rule m

  (** Checking introduction forms against their types. *)
  val elab_chk_rcode : rcode -> R.chk_rule m

  (** Rather than transitioning immediately to synthesize when we hit an [lcode],
      we perform type-directed eta expansion. This is the main ingredient to
      enable smooth elaboration of subtypes, including the "retyping principles"
      familiar from ML modules. *)
  val elab_chk_lcode : lcode -> R.chk_rule m

  (** Elaborating an elimination form. *)
  val elab_syn_lcode : lcode -> R.syn_rule m

  (** Elaborate a type *)
  val elab_tp_code : code -> R.tp_rule m
  val elab_tp_rcode : rcode -> R.tp_rule m
end


(** {1 Distillation} *)

(** The distiller takes a core-language term and turns it into a source language code. *)
module Distiller : sig
  include Monad.S
  val run : string Core.Env.t -> 'a m -> 'a
  val distill_ltm : Core.Syntax.ltm -> code m
end

module Driver = Driver
(** Toplevel driver for the proof assistant *)
