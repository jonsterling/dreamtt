open Basis
open Core

(** The central idea to the elaboration algorithm is to distinguish
    introduction forms from elimination forms; unlike some classic
    bidirectional algorithms, this distinction does not line up exactly with {i
    checking} vs. {i synthesis}, but it interacts with it in a non-trivial way:
    we only synthesize elimination forms at positive types. *)
type code =
  | R of rcode
  | L of lcode

(** [rcode] is a type of introduction forms *)
and rcode =
  | Bool
  | Pi of string * code * code
  | RcdTp of tele_code
  | Sg of string * code * code
  | Tt
  | Ff
  | Lam of string * code
  | Pair of code * code
  | Rcd of code StringMap.t

(** [lcode] is a type of elimination forms. Included via {!Core} is the
    collection of all core-language terms; this embedding is used to crucial
    effect by the elaborator. *)
and lcode =
  | Var of string
  | App of code * code
  | Fst of code
  | Snd of code
  | Proj of string * code
  | Core of tm

and tele_code =
  | TlNil
  | TlCons of string * code * tele_code

