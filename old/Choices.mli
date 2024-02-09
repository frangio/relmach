type tree
type leaf

val init : unit -> tree
val extend : tree -> leaf
val branch : leaf -> tree
val join : tree -> tree -> tree option
(* val has_join : tree -> tree -> bool *)
