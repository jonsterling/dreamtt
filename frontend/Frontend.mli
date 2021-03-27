open Basis
open Core

(** {1 The source language}

    We begin by defining a naive source language.
*)

(** The central idea to the elaboration algorithm is to distinguish
    introduction forms from elimination forms; unlike some classic
    bidirectional algorithms, this distinction does not line up exactly with {i
    checking} vs. {i synthesis}, but it interacts with it in a non-trivial way:
    we only synthesize elimination forms at positive types. *)
type code = R of rcode | L of lcode

(** [rcode] is a type of introduction forms *)
and rcode = Bool | Pi of string * code * code | Sg of string * code * code | Tt | Ff | Lam of string * code | Pair of code * code

(** [lcode] is a type of elimination forms. Included via {!Core} is the
    collection of all core-language terms; this embedding is used to crucial
    effect by the elaborator. *)
and lcode = Var of string | App of code * code | Fst of code | Snd of code | Core of tm


(** {1 Elaboration} *)

module R = Refiner

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
  val run : string Env.t -> 'a m -> 'a Error.M.m
  val distill_ltm : Syntax.ltm -> code m
end

module Driver = Driver
(** Toplevel driver for the proof assistant *)
