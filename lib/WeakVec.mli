type 'a t
val create : unit -> 'a t
val add : 'a -> 'a t -> unit
val iter : ('a -> unit) -> 'a t -> unit
