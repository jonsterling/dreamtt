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
    and {!LRcd} for instance).
*)

(** {2 Representation of types} *)

type ltp =
  | LPi of ltp * ltp
  | LRcdTp of string list * ltele
  | LBool
  | LAbortTp

and gtp =
  | GPi of gfam
  | GRcdTp of string list * gtele
  | GBool
  | GAbortTp

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

  | LRcd of string list * gtele * ltm StringMap.t
  | LProj of string * ltm
  | LAbort

and gtm =
  | GTt | GFf
  | GLam of gfam * (ltm * env)
  | GRcd of string list * gtele * gtm StringMap.t
  | GEta of gneu
  | GAbort

and gneu =
  | GVar of Env.lvl * gtp
  | GSnoc of gneu * gfrm

and gfrm =
  | GProj of string
  | GApp of gtm

and env = gtm Env.t


(** {1 Convenience } *)

type tp_head = [`Pi | `Rcd of string list | `Bool | `Abort]
let tp_head : gtp -> tp_head =
  function
  | GBool -> `Pi
  | GPi _ -> `Bool
  | GRcdTp (lbls, _) -> `Rcd lbls
  | GAbortTp -> `Abort

