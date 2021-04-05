open Syntax
open Effect

type tp_rule = ltp L.m
type chk_rule = gtp -> ltm L.m
type syn_rule = gtm L.m

type tele_rule = (string list * ltele) L.m

module TpRule =
struct
  type t = tp_rule
  let rule x = x
  let run x = x
end

module ChkRule =
struct
  type t = chk_rule
  let rule x = x
  let run x = x
end

module SynRule =
struct
  type t = syn_rule
  let rule x = x
  let run x = x
end

module TeleRule =
struct
  type t = tele_rule
  let rule x = x
  let run x = x
end
