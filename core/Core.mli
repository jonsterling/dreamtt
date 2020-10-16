(** {1 Core language} 

    The representation of the core language is {i not} exposed. Instead, an
    abstract type is provided of both core language types and core language
    terms. Both terms and types support silent weakening: hence an element of
    {!tp} or {!tm} can be used in any scope.
*)

module Env = Env

module Syntax = Syntax

module Theory = Theory

(** {2 Proof abstraction boundary} *)

(** We wrap the syntax in an abstraction boundary à la LCF. *)

module Proof : 
sig
  type 'a t
  val out : 'a t -> 'a
end

type tp = Syntax.gtp Proof.t
type tm = Syntax.gtm Proof.t

val tp_of_tm : tm -> tp

(** {2 Inspecting types} *)

type tp_head = [`Pi | `Sg | `Bool]

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
      dividing proofs into {!chk} and {!syn}. The purpose of the bidirectional
      division of labor is to enable many steps of refinement that would
      otherwise induce dozens of conversion checks to be collated in such a way
      that only one conversion check is required. 

      A related side-effect is that the refinement scripts contain very few
      annotations, drawing annotations inward from the goal and outward from
      the context.
  *)

  type chk
  type syn

  (** {2 Runners} *)

  (** A {!syn} refinement script can be executed to yield an abstract term;
      when it returns a value, that value is guaranteed to be well-typed. *)
  val run_syn : syn -> tm

  (** A {!chk} refinement script may be executed relative to a given type;
      when it returns a value, that value is guaranteed to have the type given. *)
  val run_chk : chk -> tp -> tm

  (** {1 Inference rules} *)

  (** {2 Booleans} *)

  val tt : chk
  val ff : chk

  (** {2 Dependent product types} *)

  val lam : (tm -> chk) -> chk
  val app : syn -> chk -> syn

  (** {2 Dependent sum types} *)

  val pair : chk -> chk -> chk
  val fst : syn -> syn
  val snd : syn -> syn

  (** {2 Structural rules} *)

  (** Every core language term carries has a unique type, and can hence be synthesized. *)
  val core : tm -> syn

  (** The {i conversion rule} appears in the bidirectional setting as the
      transition from synthesis to checking. *)
  val conv : syn -> chk


  (** {1 Rule combinators} *)

  val with_tp : (tp -> chk) -> chk
end
