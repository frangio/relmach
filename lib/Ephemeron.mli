type ('k, 'd) t
exception Empty
val create : unit -> ('k, 'd) t
val make : 'k -> 'd -> ('k, 'd) t
val query : 'k -> ('k, 'd) t -> 'd option
val has : 'k -> ('k, 'd) t -> bool
val empty : ('k, 'd) t -> bool
val get_key_opt : ('k, 'd) t -> 'k option
val get_data_opt : ('k, 'd) t -> 'd option
val get_key : ('k, 'd) t -> 'k
val get_data : ('k, 'd) t -> 'd
val overwrite : 'k -> 'd -> ('k, 'd) t -> unit
