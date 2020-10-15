open Basis
open Syntax

exception Impossible

type 'a instr =
  | Ret : 'a -> 'a instr
  | Eval : env * ltm -> gtm instr
  | EvalTp : env * ltp -> gtp instr
  | Bind : 'a instr * ('a -> 'b instr) -> 'b instr
  | Throw : exn -> 'a instr

module M =
struct
  type 'a m = 'a instr
  let ret a = Ret a
  let bind m k = Bind (m, k)

  let eval env ltm = Eval (env, ltm)
  let eval_tp env ltp = EvalTp (env, ltp)
  let throw exn = Throw exn
end

include M

open Monad.Notation (M)

let gapp gtm0 gtm1 : gtm m =
  match gtm0 with
  | GLam (_, (ltm, tm_env)) -> 
    let tm_env = Env.append tm_env gtm1 in
    eval tm_env ltm
  | GEta gneu ->
    ret @@ GEta (GApp (gneu, gtm1))
  | _ ->
    throw Impossible

let gfst gtm =
  match gtm with
  | GPair(_, gtm0, _) -> 
    ret gtm0
  | GEta gneu ->
    ret @@ GEta (GFst gneu)
  | _ ->
    throw Impossible

let gsnd gtm =
  match gtm with
  | GPair(_, _, gtm1) -> 
    ret gtm1
  | GEta gneu ->
    ret @@ GEta (GSnd gneu)
  | _ ->
    throw Impossible

let eval_step env : ltm -> gtm m =
  function 
  | LLam (gfam, ltm) ->
    ret @@ GLam (gfam, (ltm, env))
  | LVar ix -> 
    ret @@ Env.proj env ix
  | LTt -> ret GTt
  | LFf -> ret GFf
  | LApp (ltm0, ltm1) ->
    let* gtm0 = eval env ltm0 in
    let* gtm1 = eval env ltm1 in
    gapp gtm0 gtm1
  | LPair (gfam, ltm0, ltm1) ->
    let* gtm0 = eval env ltm0 in
    let* gtm1 = eval env ltm1 in
    ret @@ GPair (gfam, gtm0, gtm1)
  | LFst ltm ->
    let* gtm = eval env ltm in
    gfst gtm
  | LSnd ltm ->
    let* gtm = eval env ltm in
    gsnd gtm

let eval_tp_step env : ltp -> gtp instr = 
  function
  | LPi (lbase, lfam) ->
    let* gbase = eval_tp env lbase in
    ret @@ GPi (gbase, lfam, env)
  | LSg (lbase, lfam) ->
    let* gbase = eval_tp env lbase in
    ret @@ GSg (gbase, lfam, env)
  | LBool ->
    ret GBool

let rec run : type a. a m -> a =
  fun instr ->
  match instr with
  | Ret a -> a
  | Bind (m, k) -> 
    let x = run m in
    run @@ k x
  | Eval (env, ltm) -> 
    run @@ eval_step env ltm
  | EvalTp (env, ltp) -> 
    run @@ eval_tp_step env ltp
  | Throw exn ->
    raise exn

