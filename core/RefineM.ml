include Local

let lift_eval m = 
  try ret @@ Eval.run_exn m with
  | exn -> throw exn

