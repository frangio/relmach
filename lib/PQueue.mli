type 'a t

exception Empty

val empty : 'a t
val is_empty : 'a t -> bool

val snoc : 'a -> 'a t -> 'a t

val head : 'a t -> 'a
val tail : 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
