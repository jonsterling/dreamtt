open Basis
open Syntax

exception UnequalTypes
exception Impossible

module type MonadTheory =
sig
  include Monad.S
  include Local.Ops with type 'a m := 'a m and type sort = gtp and type elt = gtm
  include Error.Ops with type 'a m := 'a m
end

module Make (M : MonadTheory) =
struct
  module Eval = Eval.Make (M)
  open Monad.Notation (M)

  let rec tp_of_gtm : gtm -> gtp M.m =
    function
    | GTt | GFf ->
      M.ret GBool
    | GLam (gfam, _) ->
      M.ret @@ GPi gfam
    | GPair (gfam, _, _) ->
      M.ret @@ GSg gfam
    | GEta gneu ->
      tp_of_gneu gneu

  and tp_of_gneu : gneu -> gtp M.m =
    function
    | GVar (_, gtp) -> M.ret @@ gtp
    | GSnoc (gneu, gfrm) ->
      let* gtp = tp_of_gneu gneu in
      match gtp, gfrm with
      | GPi (_, lfam, env), GApp gtm ->
        Eval.eval_tp (Env.append env gtm) lfam
      | GSg (gbase, _, _), GFst ->
        M.ret gbase
      | GSg (_, lfam, env), GSnd ->
        let gfst = GEta (GSnoc (gneu, GFst)) in
        Eval.eval_tp (Env.append env gfst) lfam
      | _ ->
        M.throw Impossible

  let rec equate_gtp : gtp -> gtp -> unit M.m =
    fun gtp0 gtp1 ->
    match gtp0, gtp1 with
    | GBool, GBool -> M.ret ()
    | GPi (gbase0, lfam0, env0), GPi (gbase1, lfam1, env1)
    | GSg (gbase0, lfam0, env0), GSg (gbase1, lfam1, env1) ->
      let* () = equate_gtp gbase0 gbase1 in
      M.scope gbase0 @@ fun x ->
      let envx0 = Env.append env0 x in
      let envx1 = Env.append env1 x in
      let* gfib0 = Eval.eval_tp envx0 lfam0 in
      let* gfib1 = Eval.eval_tp envx1 lfam1 in
      equate_gtp gfib0 gfib1
    | _ ->
      M.throw UnequalTypes
end
