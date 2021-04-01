type 'a f =
  | Top
  | Bot

type 'x t =
  | Var of 'x
  | Prop of 'x t f

let top = Prop Top
let bot = Prop Bot
