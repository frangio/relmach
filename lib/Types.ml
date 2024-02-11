(* todo: rename to guard *)
type binop = App | Seq | Unif

type term =
  | Var of int (* de bruijn indices *)
  | Const of string
  | Bin of binop * term * term
  | Lam of string * lam_body
  | Nu of string * term

and lam_body = term list

include Unifiable.Make(struct type clos_body = lam_body end)

type universe = {
  family : UniverseFamily.t;
  mutable unifier : unifier;
  last_merged : unifier Weak.t;
}

type value = unifiable

and result = Result of value * stuck_cont PQueue.t

and stuck_cont = SLeftOf of binop * result | SRightOf of binop * result

and cont = Ret | LeftOf of frame | RightOf of frame | Resume of stuck_cont PQueue.t * cont

and frame = {
  k : cont;
  op : binop;
  mutable left : (result * universe) list;
  mutable right : (result * universe) list;
}

type env = value list
