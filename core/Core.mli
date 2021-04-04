(** {1 Core language}

    The representation of the core language is {i not} exposed. Instead, an
    abstract type is provided of both core language types and core language
    terms. Both terms and types support silent weakening: hence an element of
    {!tp} or {!tm} can be used in any scope.
*)

open Basis

module Env = Env
module Syntax = Syntax
module Equate = Equate
module Logic = Logic
module Effect = Effect

(** {2 Proof abstraction boundary} *)

(** We wrap the syntax in an abstraction boundary as in LCF. *)

module Proof :
sig
  type 'a t
  val out : 'a t -> 'a
end

type tp = Syntax.gtp Proof.t
type tm = Syntax.gtm Proof.t

val tp_of_tm : tm -> tp

(** {2 Inspecting types} *)

type tp_head = [`Pi | `Rcd of string list | `Ext | `Bool | `Abort]

(** The head of a type can be exposed in order to guide the elaborator.  It is
    (surprisingly) unnecessary to expose any more data of a type to the
    elaborator.*)
val tp_head : tp -> tp_head

(** {1 Constructing well-typed terms} *)

(** The refiner is the only way to construct terms. Any term constructed by the refiner is
    guaranteed to be well-typed, in the tradition of LCF. *)
module Refiner : sig
  (** {1 Rule types}

      The refiner follows a version of the bidirectional typing discipline,
      dividing proofs into {!chk_rule} and {!syn_rule}. The purpose of the bidirectional
      division of labor is to enable many steps of refinement that would
      otherwise induce dozens of conversion checks to be collated in such a way
      that only one conversion check is required.

      A related side-effect is that the refinement scripts contain very few
      annotations, drawing annotations inward from the goal and outward from
      the context.
  *)

  type tp_rule
  type chk_rule
  type syn_rule
  type tele_rule

  (** {1 Inference rules} *)

  (** {2 Telescopes} *)
  val tl_nil : tele_rule
  val tl_cons : string -> tp_rule -> (tm -> tele_rule) -> tele_rule

  (** {2 Booleans} *)

  val bool : tp_rule
  val tt : chk_rule
  val ff : chk_rule

  (** {2 Dependent product types} *)

  val pi : tp_rule -> (tm -> tp_rule) -> tp_rule
  val lam : (tm -> chk_rule) -> chk_rule
  val app : syn_rule -> chk_rule -> syn_rule

  (** {2 Dependent record types} *)

  val rcd_tp : tele_rule -> tp_rule
  val rcd : chk_rule StringMap.t -> chk_rule
  val proj : string -> syn_rule -> syn_rule

  (** {2 Dependent sum types} *)

  val sg : tp_rule -> (tm -> tp_rule) -> tp_rule
  val pair : chk_rule -> chk_rule -> chk_rule
  val fst : syn_rule -> syn_rule
  val snd : syn_rule -> syn_rule

  (** {2 Logical layer} *)
  val chk_abort : chk_rule


  (** {2 Structural rules} *)

  (** Every core language term carries has a unique type, and can hence be syn_rulethesized. *)
  val core : tm -> syn_rule

  (** The {i conversion rule} appears in the bidirectional setting as the
      transition from synthesis to checking. *)
  val conv : syn_rule -> chk_rule


  (** {1 Rule combinators} *)

  val with_tp : (tp -> chk_rule) -> chk_rule

  (** {2 Failing rules}
      The following rules will fail with an exception.
  *)

  val fail_tp : exn -> tp_rule
  val fail_chk : exn -> chk_rule
  val fail_syn : exn -> syn_rule
end
