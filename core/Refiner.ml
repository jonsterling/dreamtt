open Basis

open Syntax

exception TypeError

module M = RefineM
open Monad.Notation (M)

type tp_rule = ltp M.m
type chk_rule = gtp -> ltm M.m
type syn_rule = gtm M.m

let chk_rule_to_ltm (chk : chk_rule) gtp =
  Error.M.run_exn @@ M.run Env.empty @@ chk gtp

let tp_rule_to_ltp (tp : tp_rule) : ltp =
  Error.M.run_exn @@ M.run Env.empty tp

let run_chk_rule (chk : chk_rule) gtp =
  Eval.run_exn @@ Eval.eval Env.empty @@
  chk_rule_to_ltm chk gtp

let run_tp_rule (tp : tp_rule) =
  Eval.run_exn @@ Eval.eval_tp Env.empty @@
  tp_rule_to_ltp tp

let run_syn_rule syn =
  Error.M.run_exn @@ M.run Env.empty syn

let with_tp kont tp =
  kont tp tp

let inst_tp_fam : ltp -> env -> gtm -> gtp M.m =
  fun lfam env gtm ->
  let envx = Env.append env gtm in
  M.lift_eval @@ Eval.eval_tp envx lfam

let inst_tm_fam : ltm -> env -> gtm -> gtm M.m =
  fun lfam env gtm ->
  let envx = Env.append env gtm in
  M.lift_eval @@ Eval.eval envx lfam


let core =
  M.ret

let bool : tp_rule =
  M.ret LBool

let tt : chk_rule =
  function
  | GBool -> M.ret LTt
  | _ -> M.throw TypeError

let ff : chk_rule =
  function
  | GBool -> M.ret LFf
  | _ -> M.throw TypeError


(* invariant: does not return unless the list of labels has no shadowing *)
type tele_rule = (string list * ltele) M.m

let tl_nil : tele_rule =
  M.ret ([], LTlNil)

let rec freshen lbl lbls =
  if List.mem lbl lbls then
    freshen (lbl ^ "'") lbls
  else
    lbl

let tl_cons lbl tp_rule tele_rule =
  let* lbase = tp_rule in
  let* gbase =
    let* env = M.read in
    M.lift_eval @@ Eval.eval_tp env lbase
  in
  M.scope gbase @@ fun var ->
  let+ lbls, lfam = tele_rule var in
  let lbl' = freshen lbl lbls in
  lbl' :: lbls, LTlCons (lbase, lfam)

let pi (base : tp_rule) (fam : gtm -> tp_rule) : tp_rule =
  let* lbase = base in
  let* gbase =
    let* env = M.read in
    M.lift_eval @@ Eval.eval_tp env lbase
  in
  M.scope gbase @@ fun var ->
  let+ lfam = fam var in
  LPi (lbase, lfam)


let rcd_tp (tele : tele_rule) : tp_rule =
  let+ lbls, ltl = tele in
  LRcdTp (lbls, ltl)


let lam (bdy : gtm -> chk_rule) : chk_rule =
  function
  | GPi ((gbase, lfam, env) as gfam) ->
    M.scope gbase @@ fun var ->
    let+ lbdy = bdy var @<< inst_tp_fam lfam env var in
    LLam (gfam, lbdy)
  | _ ->
    M.throw TypeError

let rcd (chk_map : chk_rule StringMap.t) : chk_rule =
  function
  | GRcdTp (lbls, gtl) ->
    let rec loop tmap lbls gtl =
      match lbls, gtl with
      | [], GTlNil -> M.ret tmap
      | lbl :: lbls, GTlCons (gtp, ltl, tlenv) ->
        begin
          match StringMap.find_opt lbl chk_map with
          | Some chk_rule ->
            let* ltm = chk_rule gtp in
            let* gtm =
              let* env = M.read in
              M.lift_eval @@ Eval.eval env ltm
            in
            let* gtl' = M.lift_eval @@ Eval.eval_tele (Env.append tlenv gtm) ltl in
            let tmap' = StringMap.add lbl ltm tmap in
            loop tmap' lbls gtl'
          | None ->
            M.throw TypeError
        end
      | _ ->
        M.throw TypeError
    in
    let* tmap = loop StringMap.empty lbls gtl in
    M.ret @@ LRcd (lbls, gtl, tmap)
  | _ ->
    M.throw TypeError

let app (fn : syn_rule) (arg : chk_rule) : syn_rule =
  let* gtm0 = fn in
  Eval.tp_of_gtm gtm0 |> M.lift_eval |>> function
  | GPi (gbase, _, _) ->
    let* larg = arg gbase in
    let* env = M.read in
    M.lift_eval @@
    let open Monad.Notation (Eval) in
    let* gtm1 = Eval.eval env larg in
    Eval.gapp gtm0 gtm1
  | _ ->
    M.throw TypeError

let proj lbl (syn_rule : syn_rule) : syn_rule =
  let* gtm = syn_rule in
  Eval.tp_of_gtm gtm |> M.lift_eval |>> function
  | GRcdTp (lbls, _) when List.mem lbl lbls ->
    M.lift_eval @@ Eval.gproj lbl gtm
  | _ ->
    M.throw TypeError

let fst (syn_rule : syn_rule) : syn_rule =
  proj "fst" syn_rule

let snd (syn_rule : syn_rule) : syn_rule =
  proj "snd" syn_rule

let sg (base : tp_rule) (fam : gtm -> tp_rule) : tp_rule =
  rcd_tp @@
  tl_cons "fst" base @@ fun var ->
  tl_cons "snd" (fam var) @@ fun _ ->
  tl_nil

let pair (chk_rule0 : chk_rule) (chk_rule1 : chk_rule) : chk_rule =
  StringMap.empty
  |> StringMap.add "fst" chk_rule0
  |> StringMap.add "snd" chk_rule1
  |> rcd



let rec conv_ : gtm -> chk_rule =
  function
  | GTt -> tt
  | GFf -> ff
  | GLam (_, (ltm, env)) ->
    lam @@ fun var gfib ->
    let* gtm = inst_tm_fam ltm env var in
    conv_ gtm gfib
  | GRcd (_, _, gmap) ->
    rcd @@ StringMap.map conv_ gmap
  | GEta gneu ->
    fun gtp ->
      let* gtp' = M.lift_eval @@ Eval.tp_of_gneu gneu in
      let* () = Theory.equate_gtp gtp gtp' in
      conv_neu_ gneu


and conv_neu_ : gneu -> ltm M.m =
  function
  | GVar (lvl, _) ->
    let+ env = M.read in
    let ix = Env.lvl_to_ix env lvl in
    LVar ix

  | GSnoc (gneu, gfrm) ->
    let* ltm = conv_neu_ gneu in
    match gfrm with
    | GProj lbl -> M.ret @@ LProj (lbl, ltm)
    | GApp gtm ->
      Eval.tp_of_gneu gneu |> M.lift_eval |>> function
      | GPi (gbase, _, _) ->
        let+ ltm' = conv_ gtm gbase in
        LApp (ltm, ltm')
      | _ ->
        M.throw TypeError

let conv : syn_rule -> chk_rule =
  fun syn gtp ->
  let* gtm = syn in
  conv_ gtm gtp
