(** Representation of contexts and variables. *)

(** An abstract type representing environments that are extended on the right. *)
type 'a t

(** A pointer to a cell in the environment, counted from the right. *)
type ix

(** A pointer to a cell in the environment, counted from the left. *)
type lvl 

val empty : 'a t

val append : 'a t -> 'a -> 'a t
val proj : 'a t -> ix -> 'a

val lvl_to_ix : 'a t -> lvl -> ix
val fresh : 'a t -> lvl
