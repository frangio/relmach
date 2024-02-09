type t

exception Not_joinable

val root : t
val choice : t -> unit -> t
val join : t -> t -> t
val joinable : t -> t -> bool

val print_choice : t -> unit
