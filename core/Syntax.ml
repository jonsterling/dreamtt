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
  | LTpVar of Env.ix

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
  | Glued of (gneu, ltm) glued
  | GAbort

and gneu =
  | GVar of Env.lvl
  | GSnoc of gneu * gfrm
  | GNeuAbort

and gfrm =
  | GProj of string
  | GApp of gtm

and ('b, 'a) glued = Gl of {supp : Logic.prop; tp : gtp; base : 'b; part : 'a; env : env}
and 'a part = Prt of {supp : Logic.prop; part : 'a; env : env}

and env = [`Tm of gtm | `Tp of gtp] Env.t


(** {1 Convenience } *)

type tp_head = [`Pi | `Rcd of string list | `Bool | `Abort]

(** Project the name of the head constructor of a type; useful for guiding elaboration. *)
let tp_head : gtp -> tp_head =
  function
  | GBool -> `Pi
  | GPi _ -> `Bool
  | GRcdTp (lbls, _) -> `Rcd lbls
  | GAbortTp -> `Abort


(** Project the partial element from a glued term. *)
let glued_to_part : ('b, 'a) glued -> 'a part =
  function
  | Gl {supp; part; env; _} ->
    Prt {supp; part; env}


(** Restrict a total term to a partial term. *)
let gtm_to_part : Logic.prop -> gtm -> ltm part =
  fun supp gtm ->
  let part, env =
    let env0 = Env.empty in
    let lvl = Env.fresh env0 in
    let env = Env.append env0 @@ `Tm gtm in
    let ix = Env.lvl_to_ix env lvl in
    LVar ix, env
  in
  Prt {supp; part; env}

(** Restrict a total type to a partial type. *)
let gtp_to_part : Logic.prop -> gtp -> ltp part =
  fun supp gtp ->
  let part, env =
    let env0 = Env.empty in
    let lvl = Env.fresh env0 in
    let env = Env.append env0 @@ `Tp gtp in
    let ix = Env.lvl_to_ix env lvl in
    LTpVar ix, env
  in
  Prt {supp; part; env}

(** Construct a stable glued term, i.e. one form whom the base is nowhere unstable. *)
let stable_glued : gtp -> 'b -> ('b, ltm) glued =
  fun gtp base->
  Gl {supp = Prop.bot; tp = gtp; base; part = LAbort; env = Env.empty}

(** Project the type of a term: this is efficient and non-recursive. *)
let tp_of_gtm : gtm -> gtp =
  function
  | GTt | GFf -> GBool
  | GLam (gfam, _) ->
    GPi gfam
  | GRcd (lbls, gtele, _) ->
    GRcdTp (lbls, gtele)
  | Glued (Gl glued) ->
    glued.tp
  | GAbort ->
    GAbortTp

(** Project the partial element that a term must contingently compute to. *)
let gtm_bdry : gtm -> ltm part =
  function
  | Glued glued ->
    glued_to_part glued
  | gtm ->
    gtm_to_part Prop.top gtm

(** Project the partial element that a type must contingently compute to. *)
let gtp_bdry : gtp -> ltp part =
  function
  | gtp ->
    gtp_to_part Prop.top gtp


