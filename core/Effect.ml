open Basis
open Syntax

module E =
struct
  type local = {thy : Logic.thy; env : gtm Env.t}
  let update_thy upd {thy; env} =
    {thy = Logic.update upd thy; env}

  let bind_tm tm {thy; env} =
    {thy; env = Env.append env tm}

  let set_env env {thy; _} =
    {thy; env}
end

module L =
struct
  module M = Reader.MakeT (E) (Error.M)
  include M
  open Monad.Notation (M)

  let global m = m
  let local e m = locally (E.set_env e) m

  let catch (m : 'a m) (k : ('a, exn) Result.t -> 'b m) : 'b m =
    reader @@ fun env ->
    Error.M.run (run env m) @@ fun res ->
    run env @@ k res

  let throw e =
    lift @@ Error.M.throw e

  let theory =
    let+ x = read in
    x.thy

  let env =
    let+ x = read in
    x.env

  let scope_thy upd m =
    locally (E.update_thy upd) m

  let bind_tm gtp kont =
    let* e = env in
    let lvl = Env.fresh e in
    let var = GEta (GVar (lvl, gtp)) in
    locally (E.bind_tm var) @@ kont var
end

module G = L

type 'a gm = 'a G.m
type 'a lm = 'a L.m
