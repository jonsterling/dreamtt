open Basis
open Syntax
open Effect

exception UnequalTypes

let rec equate_gtp : gtp -> gtp -> unit G.m =
  let open Monad.Notation (G) in
  fun gtp0 gtp1 ->
    match gtp0, gtp1 with
    | GBool, GBool -> G.ret ()
    | GPi (gbase0, lfam0, env0), GPi (gbase1, lfam1, env1) ->
      let gtl0 = GTlCons (gbase0, LTlCons (lfam0, LTlNil), env0) in
      let gtl1 = GTlCons (gbase1, LTlCons (lfam1, LTlNil), env1) in
      equate_gtele gtl0 gtl1
    | GRcdTp (lbls0, gtl0), GRcdTp (lbls1, gtl1) when lbls0 = lbls1 ->
      equate_gtele gtl0 gtl1
    | _ ->
      G.throw UnequalTypes

and equate_gtele : gtele -> gtele -> unit G.m =
  let open Monad.Notation (G) in
  fun gtl0 gtl1 ->
    match gtl0, gtl1 with
    | GTlNil, GTlNil -> G.ret ()
    | GTlCons (gtp0, ltl0, env0), GTlCons (gtp1, ltl1, env1) ->
      let gfib env gtp ltl =
        G.local env @@
        L.bind_tm gtp @@ fun _ ->
        Eval.eval_tele ltl
      in
      let* gfib0 = gfib env0 gtp0 ltl0 in
      let* gfib1 = gfib env1 gtp1 ltl1 in
      equate_gtele gfib0 gfib1
    | _ ->
      G.throw UnequalTypes
