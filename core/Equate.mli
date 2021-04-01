(** The equational theory of types *)

exception UnequalTypes

open Effect

val equate_gtp : Syntax.gtp -> Syntax.gtp -> unit L.m
val equate_gtele : Syntax.gtele -> Syntax.gtele -> unit L.m
