open Basis
open Syntax

module type G =
sig
  type 'a m
  include Error.Ops with type 'a m := 'a m

  (** Access the current logical theory. *)
  val theory : Logic.thy m

  (** Perform a monotone update to the current logical theory within a scope. *)
  val scope_thy : Logic.update -> 'a m -> 'a m
end

module type L =
sig
  include G

  (** Access the local variable environment *)
  val env : [`Tm of gtm | `Tp of gtp] Env.t m

  (** Bind a variable of a given type within a scope. *)
  val bind_tm : gtp -> (gtm -> 'a m) -> 'a m

  (** Append a term to the local environment within a scope. *)
  val append_tm : gtm -> 'a m -> 'a m
end
