open Basis
open Syntax

exception UnequalTypes
exception Impossible


let rec tp_of_gtm = 
  function
  | GTt | GFf -> GBool
  | GLam (gfam, _) ->
    GPi gfam 
  | GPair (gfam, _, _) -> 
    GSg gfam
  | GEta gneu -> 
    tp_of_gneu gneu

and tp_of_gneu = 
  function
  | GVar (_, gtp) -> gtp
  | GApp (gneu, gtm) ->
    begin
      match tp_of_gneu gneu with
      | GPi (_, lfam, env) -> 
        Eval.run @@ Eval.eval_tp (Env.append env gtm) lfam
      | _ -> 
        raise Impossible
    end
  | GFst gneu ->
    begin
      match tp_of_gneu gneu with
      | GSg (gbase, _, _) -> 
        gbase
      | _ -> 
        raise Impossible
    end
  | GSnd gneu ->
    begin
      match tp_of_gneu gneu with
      | GSg (_, lfam, env) -> 
        let gfst = GEta (GFst gneu) in
        Eval.run @@ Eval.eval_tp (Env.append env gfst) lfam
      | _ -> 
        raise Impossible
    end

module Tm : Local.Tm with type tp = gtp and type tm = gtm =
struct
  type tp = gtp 
  type tm = gtm 
  let var gtp lvl =
    GEta (GVar (lvl, gtp))
end

module M = Local.M

let rec equate_gtp : gtp -> gtp -> unit M.m = 
  let open Monad.Notation (M) in
  fun gtp0 gtp1 ->
  match gtp0, gtp1 with
  | GBool, GBool -> M.ret ()
  | GPi (gbase0, lfam0, env0), GPi (gbase1, lfam1, env1) 
  | GSg (gbase0, lfam0, env0), GSg (gbase1, lfam1, env1) -> 
    let* () = equate_gtp gbase0 gbase1 in
    M.scope gbase0 @@ fun x ->
    let envx0 = Env.append env0 x in
    let envx1 = Env.append env1 x in
    let gfib0 = Eval.run @@ Eval.eval_tp envx0 lfam0 in
    let gfib1 = Eval.run @@ Eval.eval_tp envx1 lfam1 in
    equate_gtp gfib0 gfib1
  | _ ->
    raise UnequalTypes
