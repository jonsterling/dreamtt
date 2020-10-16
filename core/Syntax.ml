(** The core langauge syntax representation *)


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
  | LBool

and gtp = 
  | GPi of gfam
  | GSg of gfam
  | GBool

and gfam = gtp * ltp * env

(** {2 Representation of terms} *)

and ltm = 
  | LVar of Env.ix
  | LTt | LFf

  | LLam of gfam * ltm
  | LApp of ltm * ltm

  | LPair of gfam * ltm * ltm
  | LFst of ltm
  | LSnd of ltm

and gtm =
  | GTt | GFf
  | GLam of gfam * (ltm * env)
  | GPair of gfam * gtm * gtm
  | GEta of gneu

and gneu = 
  | GVar of Env.lvl * gtp
  | GSnoc of gneu * gfrm

and gfrm =
  | GFst 
  | GSnd
  | GApp of gtm

and env = gtm Env.t


(** {1 Convenience } *)

type tp_head = [`Pi | `Sg | `Bool]
let tp_head : gtp -> tp_head =
  function
  | GBool -> `Pi
  | GPi _ -> `Bool
  | GSg _ -> `Sg


