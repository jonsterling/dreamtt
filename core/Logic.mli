type prop = Syntax.gprop

type thy

val emp : thy
val ext : thy -> prop -> thy

val consistency : thy -> [`Consistent | `Inconsistent]
val test : thy -> prop list -> prop -> bool

type update =
  [`Ext of prop]

val update : update -> thy -> thy
