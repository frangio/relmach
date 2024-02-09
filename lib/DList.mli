type 'a t
type 'a node

val create : unit -> 'a t
val add : 'a -> 'a t -> unit
val remove : 'a node -> unit
val iter : ('a -> unit) -> 'a t -> unit

val stub : unit -> 'a node
