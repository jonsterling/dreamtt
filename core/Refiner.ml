open Basis

open Syntax

exception TypeError

module M = RefineM
open Monad.Notation (M)

type chk = gtp -> ltm M.m 
type syn = gtm M.m

let run_chk (chk : chk) gtp = 
  let ltm = M.run_exn Env.empty @@ chk gtp in
  Eval.run_exn @@ Eval.eval Env.empty ltm

let run_syn syn = 
  M.run_exn Env.empty syn

let with_tp kont tp = 
  kont tp tp

let core = 
  M.ret

let tt : chk =
  function
  | GBool -> M.ret LTt
  | _ -> M.throw TypeError

let ff : chk =
  function
  | GBool -> M.ret LFf
  | _ -> M.throw TypeError

let inst_tp_fam : ltp -> env -> gtm -> gtp M.m =
  fun lfam env gtm ->
  let envx = Env.append env gtm in
  M.lift_eval @@ Eval.eval_tp envx lfam

let inst_tm_fam : ltm -> env -> gtm -> gtm M.m =
  fun lfam env gtm ->
  let envx = Env.append env gtm in
  M.lift_eval @@ Eval.eval envx lfam

let lam (bdy : gtm -> chk) : chk = 
  function
  | GPi ((gbase, lfam, env) as gfam) ->
    M.scope gbase @@ fun var ->
    let+ lbdy = bdy var @<< inst_tp_fam lfam env var in
    LLam (gfam, lbdy)
  | _ -> 
    M.throw TypeError

let pair (chk0 : chk) (chk1 : chk) : chk = 
  function
  | GSg ((gbase, lfam, lfam_env) as gfam) ->
    let* ltm0 = chk0 gbase in
    let* gtm0 = 
      let* env = M.get_env in
      M.lift_eval @@ Eval.eval env ltm0 
    in
    let+ ltm1 = chk1 @<< inst_tp_fam lfam lfam_env gtm0 in
    LPair (gfam, ltm0, ltm1)
  | _ ->
    M.throw TypeError

let app (fn : syn) (arg : chk) : syn = 
  let* gtm0 = fn in
  match Theory.tp_of_gtm gtm0 with
  | GPi (gbase, _, _) ->
    let* larg = arg gbase in
    let* env = M.get_env in
    M.lift_eval @@
    let open Monad.Notation (Eval) in
    let* gtm1 = Eval.eval env larg in
    Eval.gapp gtm0 gtm1
  | _ -> 
    M.throw TypeError

let fst (syn : syn) : syn = 
  let* gtm = syn in
  match Theory.tp_of_gtm gtm with
  | GSg _ ->
    M.lift_eval @@ Eval.gfst gtm
  | _ ->
    M.throw TypeError

let snd (syn : syn) : syn = 
  let* gtm = syn in
  match Theory.tp_of_gtm gtm with
  | GSg _ ->
    M.lift_eval @@ Eval.gsnd gtm
  | _ ->
    M.throw TypeError


let rec conv_ : gtm -> chk =
  function
  | GTt -> tt
  | GFf -> ff
  | GLam (_, (ltm, env)) -> 
    lam @@ fun var gfib ->
    let* gtm = inst_tm_fam ltm env var in
    conv_ gtm gfib
  | GPair (_, gtm0, gtm1) -> 
    pair (conv_ gtm0) (conv_ gtm1)
  | GEta gneu -> 
    fun gtp ->
      let gtp' = Theory.tp_of_gneu gneu in
      let* () = Theory.equate_gtp gtp gtp' in
      conv_neu_ gneu

and conv_neu_ : gneu -> ltm M.m =
  function
  | GVar (lvl, _) -> 
    let+ env = M.get_env in
    let ix = Env.lvl_to_ix env lvl in 
    LVar ix

  | GSnoc (gneu, gfrm) ->
    let* ltm = conv_neu_ gneu in
    match gfrm with
    | GFst -> M.ret @@ LFst ltm
    | GSnd -> M.ret @@ LSnd ltm
    | GApp gtm -> 
      begin
        match Theory.tp_of_gneu gneu with
        | GPi (gbase, _, _) -> 
          let+ ltm' = conv_ gtm gbase in
          LApp (ltm, ltm')
        | _ -> 
          M.throw TypeError
      end

let conv : syn -> chk =
  fun syn gtp -> 
  let* gtm = syn in 
  conv_ gtm gtp
