type ('k, 'd) t
exception Empty
val make : 'k -> 'd -> ('k, 'd) t
val is_empty : ('k, 'd) t -> bool
val get_key_opt : ('k, 'd) t -> 'k option
val get_data_opt : ('k, 'd) t -> 'd option
val get_key : ('k, 'd) t -> 'k
val get_data : ('k, 'd) t -> 'd
