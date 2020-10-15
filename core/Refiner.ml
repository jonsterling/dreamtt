open Basis

open Syntax

exception TypeError

module Tm : Local.Tm with type tp = gtp and type tm = gtm =
struct
  type tp = gtp 
  type tm = gtm 
  let var gtp lvl =
    GEta (GVar (lvl, gtp))
end


module M = Local.Make (Tm)
open Monad.Notation (M)

type chk = gtp -> ltm M.m 
type syn = gtm M.m

let run_chk (chk : chk) gtp = 
  let ltm = M.run Env.empty @@ chk gtp in
  Eval.run @@ Eval.eval Env.empty ltm

let run_syn syn = 
  M.run Env.empty syn

let with_tp kont tp = 
  kont tp tp

let core = 
  M.ret

let tt : chk =
  function
  | GBool -> M.ret LTt
  | _ -> raise TypeError

let ff : chk =
  function
  | GBool -> M.ret LFf
  | _ -> raise TypeError

let inst_tp_fam : ltp -> env -> gtm -> gtp M.m =
  fun lfam env gtm ->
  let envx = Env.append env gtm in
  M.ret @@ Eval.run @@ Eval.eval_tp envx lfam

let inst_tm_fam : ltm -> env -> gtm -> gtm M.m =
  fun lfam env gtm ->
  let envx = Env.append env gtm in
  M.ret @@ Eval.run @@ Eval.eval envx lfam

let lam (bdy : gtm -> chk) : chk = 
  function
  | GPi ((gbase, lfam, env) as gfam) ->
    M.scope gbase @@ fun var ->
    let* gfib = inst_tp_fam lfam env var in
    let* lbdy = bdy var gfib in
    M.ret @@ LLam (gfam, lbdy)
  | _ -> 
    raise TypeError

let pair (chk0 : chk) (chk1 : chk) : chk = 
  function
  | GSg ((gbase, lfam, lfam_env) as gfam) ->
    let* ltm0 = chk0 gbase in
    let* gtm0 = 
      let+ env = M.get_env in
      Eval.run @@ Eval.eval env ltm0 
    in
    let+ ltm1 = 
      let* gfib = inst_tp_fam lfam lfam_env gtm0 in
      chk1 gfib 
    in
    LPair (gfam, ltm0, ltm1)
  | _ ->
    raise TypeError

let app (fn : syn) (arg : chk) : syn = 
  let* gtm0 = fn in
  match Theory.tp_of_gtm gtm0 with
  | GPi (gbase, _, _) ->
    let* larg = arg gbase in
    let* env = M.get_env in
    M.ret @@ Eval.run @@ Eval.gapp gtm0 @@ Eval.run @@ Eval.eval env larg
  | _ -> 
    raise TypeError

let fst (syn : syn) : syn = 
  let* gtm = syn in
  match Theory.tp_of_gtm gtm with
  | GSg _ ->
    M.ret @@ Eval.run @@ Eval.gfst gtm
  | _ ->
    raise TypeError

let snd (syn : syn) : syn = 
  let* gtm = syn in
  match Theory.tp_of_gtm gtm with
  | GSg _ ->
    M.ret @@ Eval.run @@ Eval.gsnd gtm
  | _ ->
    raise TypeError


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
      let () = Theory.equate_gtp gtp gtp' in
      let* ltm = conv_neu_ gneu in
      M.ret @@ ltm

and conv_neu_ : gneu -> ltm M.m =
  function
  | GVar (lvl, _) -> 
    let* env = M.get_env in
    let ix = Env.lvl_to_ix env lvl in 
    M.ret @@ LVar ix

  | GApp (gneu, gtm) -> 
    let* ltm0 = conv_neu_ gneu in
    begin
      match Theory.tp_of_gneu gneu with
      | GPi (gbase, _, _) -> 
        let* ltm1 = conv_ gtm gbase in
        M.ret @@ LApp (ltm0, ltm1)
      | _ -> 
        raise TypeError
    end

  | GFst gneu ->
    let+ ltm = conv_neu_ gneu in
    LFst ltm

  | GSnd gneu ->
    let+ ltm = conv_neu_ gneu in
    LSnd ltm

let conv : syn -> chk =
  fun syn gtp -> 
  let* gtm = syn in 
  conv_ gtm gtp
