open Types
open Result

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
                  enqueue (Return (stuck x (SLeftOf (App, r)), k, u))
              | l' ->
                  apply k op (value l') r u
              end
          | Struct _ ->
              enqueue (Return (value (cons_struct lv rv), k, u))
          | Clos (_, _, [body], e) ->
              enqueue (Eval (body, rv :: e, k, u))
          | Clos (_, _, bodies, e) ->
              let app body =
                enqueue (Eval (body, rv :: e, k, Universe.branch u))
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
