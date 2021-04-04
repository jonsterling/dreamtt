(** The equational theory of types *)

exception UnequalTypes

open Effect

val equate_gtp : Syntax.gtp -> Syntax.gtp -> unit lm
val equate_gtele : Syntax.gtele -> Syntax.gtele -> unit lm
