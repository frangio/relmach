module Make (H : Hashtbl.HashedType) : sig
  type key = H.t
  type 'a t

  val empty : 'a t
  val size : 'a t -> int

  val get : key -> 'a t -> 'a option
  val set : key -> 'a -> 'a t -> 'a t

  val merge : 'a t -> 'a t -> 'a t
  val merge_with : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val print_weakmap : (out_channel -> key * 'a -> unit) -> 'a t -> unit
end
