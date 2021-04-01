type prop = [`L of int] Prop.t

type thy

val emp : thy
val ext : thy -> prop -> thy

val consistency : thy -> [`Consistent | `Inconsistent]
val test : thy -> prop list -> prop -> bool
