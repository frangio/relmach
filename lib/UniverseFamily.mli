type t

val root : unit -> t
val branch : t -> t
val join : t -> t -> t

val kill : t -> unit
val is_dead : t -> bool
