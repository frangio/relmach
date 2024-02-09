type 'a t
val create : 'a -> 'a t
val get : 'a t -> 'a option
val iter : ('a -> unit) -> 'a t -> unit
