open Types
open Result

type job =
  | Eval of term * env * universe * cont
  | Return of result * universe * cont

type pool = job Queue.t

let init (t : term) : pool =
  let p = Queue.create () in
  Queue.add (Eval (t, [], Universe.root (), Ret)) p;
  p

let run (p : pool) : result Seq.t =
  let enqueue j = Queue.add j p in

  let try_unstuck k s sk u =
    let Result (sv, skq) = s in
    let sv = Unifier.find u.unifier sv in
    let skq = PQueue.snoc sk skq in
    match sv with
    | Sym _ -> enqueue (Return (Result (sv, skq), u, k))
    | _ -> enqueue (Return (value sv, u, Resume (skq, k)))
  in

  let rec apply k op l r u =
    if is_stuck l then
      try_unstuck k l (SLeftOf (op, r)) u
    else
      match op with
      | Seq ->
          enqueue (Return (r, u, k))
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
              enqueue (Return (value (make_struct "ok"), u, k))
          end
      | App ->
          let Result (lv, _) = l in
          let Result (rv, _) = r in
          begin match lv with
          | Sym _ ->
              begin match Unifier.find u.unifier lv with
              | Sym _ as x ->
                  enqueue (Return (stuck x (SLeftOf (App, r)), u, k))
              | l' ->
                  apply k op (value l') r u
              end
          | Struct _ ->
              enqueue (Return (value (cons_struct lv rv), u, k))
          | Clos (_, _, [body], e) ->
              enqueue (Eval (body, rv :: e, u, k))
          | Clos (_, _, bodies, e) ->
              let app body =
                enqueue (Eval (body, rv :: e, Universe.branch u, k))
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
    | Eval (_, _, u, _) | Return (_, u, _) when Universe.is_dead u ->
        next ()
    | Eval (Bin (op, l, r), e, u, k) ->
        let f = { op; k; left = []; right = [] } in
        enqueue (Eval (l, e, u, LeftOf f));
        enqueue (Eval (r, e, u, RightOf f));
        next ()
    | Eval (Nu (x, t), e, u, k) ->
        let x = make_symbol x in
        enqueue (Eval (t, x :: e, u, k));
        next ()
    | Eval (Lam (x, t), e, u, k) ->
        let v = make_closure x t e in
        enqueue (Return (value v, u, k));
        next ()
    | Eval (Const c, _, u, k) ->
        let v = make_struct c in
        enqueue (Return (value v, u, k));
        next ()
    | Eval (Var i, e, u, k) ->
        let v = List.nth e i in
        enqueue (Return (value v, u, k));
        next ()
    | Return (r, u, Resume (skq, k)) ->
        begin match r, PQueue.head skq, PQueue.tail skq with
        | exception PQueue.Empty ->
            enqueue (Return (r, u, k))
        | rl, SLeftOf (op, rr), skq'
        | rr, SRightOf (op, rl), skq' ->
            let k' = Resume (skq', k) in
            apply k' op rl rr u
        end;
        next ()
    | Return (l, lu, LeftOf fr) ->
        (* todo: optimization for prematurely stuck results *)
        List.iter (fun (r, ru) -> join_apply fr.k fr.op l r lu ru) fr.right;
        fr.left <- (l, lu) :: fr.left;
        next ()
    | Return (r, ru, RightOf fr) ->
        List.iter (fun (l, lu) -> join_apply fr.k fr.op l r lu ru) fr.left;
        fr.right <- (r, ru) :: fr.right;
        next ()
    | Return (r, u, Ret) ->
        let r' = map_result (Unifier.subst u.unifier) r in
        Seq.Cons (r', next)
  in
  next
