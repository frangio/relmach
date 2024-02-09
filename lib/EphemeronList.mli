type ('k, 'd) t
val create : unit -> ('k, 'd) t
val pushe : ('k, 'd) Ephemeron.t -> ('k, 'd) t -> unit
val push : 'k -> 'd -> ('k, 'd) t -> unit
val filter_inplace : (('k, 'd) Ephemeron.t -> bool) -> ('k, 'd) t -> unit
