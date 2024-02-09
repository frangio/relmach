module type Params = sig
  type clos_body
end

module Make (P : Params) : sig
  type unifiable = private
    | Sym of int * string
    | Struct of int * string * unifiable list
    | Clos of int * string * P.clos_body * unifiable_env
  and unifiable_env = unifiable list

  val make_symbol : string -> unifiable
  val make_closure : string -> P.clos_body -> unifiable_env -> unifiable
  val make_struct : string -> unifiable
  val cons_struct : unifiable -> unifiable -> unifiable

  type unifier

  module Unifier : sig
    type t = unifier
    val empty : t
    val find : t -> unifiable -> unifiable
    val subst : t -> unifiable -> unifiable
    val unify : t -> unifiable -> unifiable -> t option
    val merge : t -> t -> t option
  end
end
