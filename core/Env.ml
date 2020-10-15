type 'a t = 'a list
type ix = Ix of int
type lvl = Lvl of int
let lvl l = Lvl l
let empty = []
let size = List.length
let append xs x = x :: xs
let proj xs (Ix i) = List.nth xs i
let lvl_to_ix xs (Lvl l) = Ix (size xs - l - 1)

let fresh xs = Lvl (size xs)
