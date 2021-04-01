(** Backward lists (notation inspired by Conor McBride) *)

type 'a t =
  | Emp
  | Snoc of 'a t * 'a

val nth : 'a t -> int -> 'a
val length : 'a t -> int
val mem : 'a -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val flat_map : ('a -> 'b list) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
val to_list : 'a t -> 'a list
val from_list : 'a list -> 'a t

module Notation :
sig
  val (<.>) : 'a t -> 'a t -> 'a t
  val (#<) : 'a t -> 'a -> 'a t
  val (<><) : 'a t -> 'a list -> 'a t
  val (<>>) : 'a t -> 'a list -> 'a list
end

