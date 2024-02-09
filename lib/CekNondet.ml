type term = Var of int | App of term * term | Lam of string * term

type clos = Clos of term * env
and env = clos list

type cont = Ret | Left of frame | Right of frame

and frame = {
  mutable left : clos option;
  mutable right : clos option;
  parent : cont;
}

type job = term * env * cont
type state = job Queue.t

let rec run (s : state) : clos =
  match Queue.take s with
  | App (t, u), e, k ->
      let f = { left = None; right = None; parent = k } in
      Queue.add (t, e, Left f) s;
      Queue.add (u, e, Right f) s;
      run s
  | Var i, e, k ->
      let (Clos (t, e')) = List.nth e i in
      Queue.add (t, e', k) s;
      run s
  | t, e, Left ({ right = None; _ } as f) ->
      f.left <- Some (Clos (t, e));
      run s
  | t, e, Right ({ left = None; _ } as f) ->
      f.right <- Some (Clos (t, e));
      run s
  | t, e, Left { right = Some (Clos (u, e')); parent; _ }
  | u, e', Right { left = Some (Clos (t, e)); parent; _ } -> (
      match t with
      | Lam (_, t) ->
          Queue.add (t, Clos (u, e') :: e, parent) s;
          run s
      | _ -> failwith "unexpected")
  | t, e, Ret -> Clos (t, e)

let init t : state =
  let s = Queue.create () in
  Queue.add (t, [], Ret) s;
  s

let rec decode (Clos (t, e)) : term =
  let rec decode' t e offset =
    match t with
    | Var i -> if i >= offset then decode (List.nth e (i - offset)) else Var i
    | App (t, u) -> App (decode' t e offset, decode' u e offset)
    | Lam (x, t) -> Lam (x, decode' t e (offset + 1))
  in
  decode' t e 0

let rec print_term ?(vars : string list = []) = function
  | Var i -> print_string (List.nth vars i)
  | App (Lam (x, t), App (u, s)) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_string "(";
      print_term (App (u, s)) ~vars;
      print_string ")"
  | App (Lam (x, t), u) ->
      print_string "(";
      print_term (Lam (x, t)) ~vars;
      print_string ")";
      print_term u ~vars
  | App (t, App (u, s)) ->
      print_term t ~vars;
      print_string "(";
      print_term (App (u, s)) ~vars;
      print_string ")"
  | App (t, u) ->
      print_term t ~vars;
      print_term u ~vars
  | Lam (x, t) ->
      Printf.printf "λ%s." x;
      print_term t ~vars:(x :: vars)

let () = Printexc.record_backtrace true
let id = Lam ("x", Var 0)
let small_omega = Lam ("x", App (Var 0, Var 0))
let big_omega = App (small_omega, small_omega)

(* let () = *)
(*   print_term id; *)
(*   print_newline (); *)
(*   let prog = App (id, id) in *)
(*   print_term prog; *)
(*   print_newline (); *)
(*   let c = run (init prog) in *)
(*   print_term (decode c); *)
(*   print_newline () *)

(* let () =
  print_term big_omega;
  print_newline ();
  let s = init big_omega in
  Queue.add (id, [], Ret) s;
  let c = run s in
  print_term (decode c) *)
