(** The equational theory of types *)

exception UnequalTypes

val tp_of_gtm : Syntax.gtm -> Syntax.gtp
val tp_of_gneu : Syntax.gneu -> Syntax.gtp

val equate_gtp : Syntax.gtp -> Syntax.gtp -> unit Local.M.m
val equate_gtele : Syntax.gtele -> Syntax.gtele -> unit Local.M.m
