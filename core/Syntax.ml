(** The core language syntax representation *)

open Basis

(** {1 Core language representations }

    The core language syntax is split into two representations, after Coquand:
    a local form that is sensitive to the context (using De Bruijn indices),
    and a global form that is insensitive to the context (using De Bruijn
    levels).


    The "local" syntax corresponds to ordinary syntax, and the "global" syntax
    corresponds to weak head normal forms in many NbE-style implementations. We
    make no attempt to restrict local syntax to normal forms; unlike
    conventional implementation, we freely interleave the local and the global
    syntax when it saves us some computations (see the annotations on {!LLam}
    and {!LPair} for instance).
*)

(** {2 Representation of types} *)

type ltp =
  | LPi of ltp * ltp
  | LSg of ltp * ltp
  | LRcdTp of string list * ltele
  | LBool

and gtp =
  | GPi of gfam
  | GSg of gfam
  | GRcdTp of string list * gtele
  | GBool

and gfam = gtp * ltp * env


and gtele =
  | GTlNil
  | GTlCons of gtp * ltele * env

and ltele =
  | LTlNil
  | LTlCons of ltp * ltele


(** {2 Representation of terms} *)

and ltm =
  | LVar of Env.ix
  | LTt | LFf

  | LLam of gfam * ltm
  | LApp of ltm * ltm

  | LPair of gfam * ltm * ltm
  | LRcd of string list * gtele * ltm StringMap.t
  | LFst of ltm
  | LSnd of ltm
  | LProj of string * ltm

and gtm =
  | GTt | GFf
  | GLam of gfam * (ltm * env)
  | GPair of gfam * gtm * gtm
  | GRcd of string list * gtele * gtm StringMap.t
  | GEta of gneu

and gneu =
  | GVar of Env.lvl * gtp
  | GSnoc of gneu * gfrm

and gfrm =
  | GFst
  | GSnd
  | GProj of string
  | GApp of gtm

and env = gtm Env.t


(** {1 Convenience } *)

type tp_head = [`Pi | `Sg | `Rcd of string list | `Bool]
let tp_head : gtp -> tp_head =
  function
  | GBool -> `Pi
  | GPi _ -> `Bool
  | GSg _ -> `Sg
  | GRcdTp (lbls, _) -> `Rcd lbls

