(** The equational theory of types *)

open Basis


exception UnequalTypes

module type MonadTheory =
sig
  include Monad.S
  include Local.Ops with type 'a m := 'a m and type sort = Syntax.gtp and type elt = Syntax.gtm
  include Error.Ops with type 'a m := 'a m
end


module Make (M : MonadTheory) : sig
  val tp_of_gtm : Syntax.gtm -> Syntax.gtp M.m
  val tp_of_gneu : Syntax.gneu -> Syntax.gtp M.m
  val equate_gtp : Syntax.gtp -> Syntax.gtp -> unit M.m
end
