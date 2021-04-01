type 'a f =
  | Top
  | Bot

type 'x t =
  | Var of 'x
  | Prop of 'x t f

val top : 'x t
val bot : 'x t
