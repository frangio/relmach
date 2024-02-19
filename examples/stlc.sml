let fix =
  let f = fun x y -> y (fun z -> x x y z)
  in f f

let sub1 = fun n ->
  fresh n' in
  n ~ S n';
  n'

let nth = fix (fun nth n xs ->
  | fresh x _ in
    n ~ Z;
    xs ~ Cons x _;
    x
  | fresh x xs' in
    xs ~ Cons x xs';
    nth (sub1 n) xs')

let ty = fix (fun ty e t a ->
  | fresh n in
    let _ = t ~ Var n in
    a ~ nth n e
  | fresh u b c in
    let _ = t ~ Lam u; a ~ Fun b c in
    ty (Cons b e) u c
  | fresh u s b in
    let _ = t ~ App u s in
    ty e u (Fun b a);
    ty e s b)

let _ =
  fresh a in
  ty Nil (Lam (Lam (Var (S Z)))) a;
  a
