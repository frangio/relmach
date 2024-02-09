type ('k, 'a) _t

module Make (H : Hashtbl.HashedType) : sig
  type key = H.t
  type 'a t = (key, 'a) _t

  val empty : 'a t
  val size : 'a t -> int

  val get : key -> 'a t -> 'a option
  val set : key -> 'a -> 'a t -> 'a t

  val merge : 'a t -> 'a t -> 'a t
  val merge_with : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end
