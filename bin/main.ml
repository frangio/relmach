open Relmach
open AllocHash

type binop = App | Seq | Unif

type term =
  | Var of int (* de bruijn indices *)
  | Const of string
  | Bin of binop * term * term
  | Lam of string * lam_body
  | Nu of string * term
and lam_body = term list

open Unifiable.Make(struct type clos_body = lam_body end)

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

let value v = Result (v, PQueue.empty)

let is_stuck (Result (_, skq)) = not (PQueue.is_empty skq)

let rec map_result f (Result (v, skq)) =
  let map_stuck_cont = function
    | SLeftOf (op, r) -> SLeftOf (op, map_result f r)
    | SRightOf (op, r) -> SRightOf (op, map_result f r)
  in
  Result (f v, PQueue.map map_stuck_cont skq)

module Universe = struct
  module Family = UniverseFamily

  let root () = {
    family = Family.root ();
    unifier = Unifier.empty;
    last_merged = Weak.create 2;
  }

  let branch u =
    let family = Family.branch u.family in
    let last_merged = Weak.create 2 in
    Weak.blit u.last_merged 0 last_merged 0 2;
    { u with family; last_merged }

  let kill u = Family.kill u.family
  let is_dead u = Family.is_dead u.family

  let last_any w x =
    match Weak.(get w 0, get w 1) with
    | Some y, _ when x == y -> true
    | _, Some y when x == y -> true
    | _ -> false

  let join u0 u1 =
    let merge_into u =
      match Unifier.merge u0.unifier u1.unifier with
      | None ->
          kill u;
          None
      | Some g ->
          Weak.set u.last_merged 0 (Some u0.unifier);
          Weak.set u.last_merged 1 (Some u1.unifier);
          u.unifier <- g;
          Some u
    in
    let f = Family.join u0.family u1.family in
    if f == u0.family then
      if last_any u0.last_merged u1.unifier then
        Some u0
      else 
        merge_into u0
    else if f == u1.family then
      if last_any u1.last_merged u0.unifier then
        Some u1
      else 
        merge_into u1
    else
      let last_merged = Weak.create 2 in
      let u = { family = f; unifier = Unifier.empty; last_merged } in
      merge_into u
end

type job =
  | Eval of term * env * cont * universe
  | Return of result * cont * universe

type pool = job Queue.t

let init (t : term) : pool =
  let p = Queue.create () in
  Queue.add (Eval (t, [], Ret, Universe.root ())) p;
  p

let run (p : pool) : result Seq.t =
  let enqueue j = Queue.add j p in

  let try_unstuck k s sk u =
    let Result (sv, skq) = s in
    let sv = Unifier.find u.unifier sv in
    let skq = PQueue.snoc sk skq in
    match sv with
    | Sym _ -> enqueue (Return (Result (sv, skq), k, u))
    | _ -> enqueue (Return (value sv, Resume (skq, k), u))
  in

  let rec apply k op l r u =
    if is_stuck l then
      try_unstuck k l (SLeftOf (op, r)) u
    else
      match op with
      | Seq ->
          enqueue (Return (r, k, u))
      | Unif | App when is_stuck r ->
          try_unstuck k r (SRightOf (op, l)) u
      | Unif ->
          let Result (lv, _) = l in
          let Result (rv, _) = r in
          begin match Unifier.unify u.unifier lv rv with
          | None ->
              Universe.kill u
          | Some g ->
              u.unifier <- g;
              enqueue (Return (value (make_struct "ok"), k, u))
          end
      | App ->
          let Result (lv, _) = l in
          let Result (rv, _) = r in
          begin match lv with
          | Sym _ ->
              begin match Unifier.find u.unifier lv with
              | Sym _ as x ->
                  let skq = PQueue.(snoc (SLeftOf (App, r)) empty) in
                  enqueue (Return (Result (x, skq), k, u))
              | l' ->
                  apply k op (value l') r u
              end
          | Struct _ ->
              enqueue (Return (value (cons_struct lv rv), k, u))
          | Clos (_, _, [body], e) ->
              enqueue (Eval (body, rv :: e, k, u))
          | Clos (_, _, bodies, e) ->
              let app body =
                let u' = Universe.branch u in
                enqueue (Eval (body, rv :: e, k, u'))
              in
              List.iter app bodies
          end
  in

  let join_apply k op l r lu ru =
    Option.iter (apply k op l r) (Universe.join lu ru)
  in

  let rec next () =
    match Queue.take p with
    | exception Queue.Empty ->
        Seq.Nil
    | Eval (_, _, _, u) | Return (_, _, u) when Universe.is_dead u ->
        next ()
    | Eval (Bin (op, l, r), e, k, u) ->
        let f = { op; k; left = []; right = [] } in
        enqueue (Eval (l, e, LeftOf f, u));
        enqueue (Eval (r, e, RightOf f, u));
        next ()
    | Eval (Nu (x, t), e, k, u) ->
        let x = make_symbol x in
        enqueue (Eval (t, x :: e, k, u));
        next ()
    | Eval (Lam (x, t), e, k, u) ->
        let v = make_closure x t e in
        enqueue (Return (value v, k, u));
        next ()
    | Eval (Const c, _, k, u) ->
        let v = make_struct c in
        enqueue (Return (value v, k, u));
        next ()
    | Eval (Var i, e, k, u) ->
        let v = List.nth e i in
        enqueue (Return (value v, k, u));
        next ()
    | Return (r, Resume (skq, k), u) ->
        begin match r, PQueue.head skq, PQueue.tail skq with
        | exception PQueue.Empty ->
            enqueue (Return (r, k, u))
        | rl, SLeftOf (op, rr), skq'
        | rr, SRightOf (op, rl), skq' ->
            let k' = Resume (skq', k) in
            apply k' op rl rr u
        end;
        next ()
    | Return (l, LeftOf fr, lu) ->
        (* todo: optimization for prematurely stuck results *)
        List.iter (fun (r, ru) -> join_apply fr.k fr.op l r lu ru) fr.right;
        fr.left <- (l, lu) :: fr.left;
        next ()
    | Return (r, RightOf fr, ru) ->
        List.iter (fun (l, lu) -> join_apply fr.k fr.op l r lu ru) fr.left;
        fr.right <- (r, ru) :: fr.right;
        next ()
    | Return (r, Ret, u) ->
        let r' = map_result (Unifier.subst u.unifier) r in
        Seq.Cons (r', next)
  in
  next

(*

λ1 x.

 *)

let rec decode_value v =
  match v with
  | Sym _ -> failwith "open term"
  | Struct (_, c, vs) -> List.fold_right (fun r l -> Bin (App, l, decode_value r)) vs (Const c)
  | Clos (_, x, t, e) -> Lam (x, List.map (decode_term ~free:1 e) t)

and decode_term ?(free = 0) e t =
  match t with
  | Const c -> Const c
  | Var i -> if i >= free then decode_value (List.nth e (i - free)) else Var i
  | Bin (op, t, u) -> Bin (op, decode_term e t ~free, decode_term e u ~free)
  | Lam (x, t) -> Lam (x, List.map (decode_term e ~free:(free + 1)) t)
  | Nu (x, t) -> Nu (x, decode_term e t ~free:(free + 1))

let rec print_term ?(vars : string list = []) = function
  | Var i -> print_string (List.nth vars i)
  | Bin (App, Lam (x, t), Bin (App, u, s)) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_string "(";
      print_term (Bin (App, u, s)) ~vars;
      print_string ")"
  | Bin (App, Lam (x, t), u) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_term u ~vars
  | Bin (App, t, Bin (App, u, s)) ->
      print_term t ~vars;
      print_string "(";
      print_term (Bin (App, u, s)) ~vars;
      print_string ")"
  | Bin (App, t, u) ->
      print_term t ~vars;
      print_term u ~vars
  | Bin (Seq, t, u) ->
      print_term t ~vars;
      print_string ";";
      print_term u ~vars
  | Lam (x, t) ->
      Printf.printf "λ%s." x;
      List.iteri
        (fun i t0 ->
          if i > 0 then print_string "|";
          print_term t0 ~vars:(x :: vars))
        t
  | _ -> failwith "todo"

let () = Printexc.record_backtrace true
let id = Lam ("x", [ Var 0 ])
let const = Lam ("x", [ Lam ("y", [ Var 1 ]) ])
let dup = Lam ("x", [ Var 0; Var 0 ])
(* let small_omega = Lam ("x", App (Var 0, Var 0)) *)
(* let big_omega = App (small_omega, small_omega) *)

let () =
  let id2 = Bin (App, dup, id) in
  let prog = Bin (App, dup, id2) in
  print_term prog;
  print_newline ();
  print_newline ();
  let vs = run (init prog) in
  List.iter
    (fun v ->
      print_term (decode_value v);
      print_newline ())
    vs

(* let () = *)
(*   print_term big_omega; *)
(*   print_newline (); *)
(*   let s = init big_omega in *)
(*   Queue.add (id, [], Ret) s; *)
(*   let c = run s in *)
(*   print_term (decode c) *)
