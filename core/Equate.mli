(** The equational theory of types *)

exception UnequalTypes

val equate_gtp : Syntax.gtp -> Syntax.gtp -> unit Local.M.m
val equate_gtele : Syntax.gtele -> Syntax.gtele -> unit Local.M.m
