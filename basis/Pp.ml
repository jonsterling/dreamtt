type 'a printer = Format.formatter -> 'a -> unit

open Bwd.Notation

module Env =
struct
  type t = string Bwd.t

  exception EmptyEnv
  exception UnboundVariable of {ix : int; env: t}

  let emp = Bwd.Emp

  let nat_to_suffix n =
    let formatted = string_of_int n in
    let lookup : int -> string = List.nth ["₀";"₁";"₂";"₃";"₄";"₅";"₆";"₇";"₈";"₉"] in
    String.concat "" @@
    List.init (String.length formatted) @@
    fun n -> lookup (Char.code (String.get formatted n) - Char.code '0')

  let rec rename xs x i =
    let suffix = nat_to_suffix i in
    let new_x = x ^ suffix in
    if Bwd.mem new_x xs then (rename [@tailcall]) xs x (i + 1) else new_x

  let choose_name (env : t) (x : string) =
    if Bwd.mem x env then rename env x 1 else x

  let var i env =
    if i < Bwd.length env then
      Bwd.nth env i
    else
      raise @@ UnboundVariable {ix = i; env}

  let proj =
    function
    | Bwd.Emp -> raise EmptyEnv
    | Bwd.Snoc (xs, _) -> xs

  let bind (env : t) (nm : string option) : string * t =
    let x =
      match nm with
      | None -> choose_name env "_x"
      | Some x -> choose_name env x
    in
    x, env #< x

  let rec bindn (env : t) (nms : string option list) : string list * t =
    match nms with
    | [] ->
      [], env
    | nm :: nms ->
      let x, env' = bind env nm in
      let xs, env'' = bindn env' nms in
      (x :: xs), env''

  let names (env : t) : string list =
    env <>> []
end

type env = Env.t
