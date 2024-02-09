type path
type node

val make_root : unit -> path
val make_succ : path -> node
val make_edge : node -> path

(*
(* alternatives *)
val extend : path -> path
val branch : path -> path
*)

type tree

val make_tree : path -> tree
val join_trees : tree -> tree -> tree option
