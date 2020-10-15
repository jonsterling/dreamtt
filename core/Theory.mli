open Syntax

exception UnequalTypes

val tp_of_gtm : gtm -> gtp
val tp_of_gneu : gneu -> gtp

val equate_gtp : gtp -> gtp -> unit
